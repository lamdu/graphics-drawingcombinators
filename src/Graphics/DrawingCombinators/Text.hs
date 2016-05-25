{-# LANGUAGE BangPatterns #-}
-- | Text-rendering and fonts component of Drawing Combinators

module Graphics.DrawingCombinators.Text
    ( Font, openFont
    , fontAscender, fontDescender, fontHeight, fontLineGap
    , textAdvance
    , BoundingBox(..), textBoundingBox, textBoundingWidth
    , renderText
    , TextAttrs(..), defTextAttrs
    , cleanQueuedGlResources
    ) where

import           Control.Concurrent (ThreadId, myThreadId)
import           Control.Concurrent.MVar
import qualified Control.Exception as Exception
import           Control.Monad (void)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State (StateT(..))
import           Data.IORef
import           Data.List (partition)
import           Graphics.DrawingCombinators.Affine
import           Graphics.DrawingCombinators.Color
import qualified Graphics.FreetypeGL.FontManager as FontManager
import           Graphics.FreetypeGL.Init (initFreetypeGL)
import           Graphics.FreetypeGL.Markup (Markup(..))
import qualified Graphics.FreetypeGL.Markup as Markup
import           Graphics.FreetypeGL.Mat4 (Mat4)
import qualified Graphics.FreetypeGL.Mat4 as Mat4
import           Graphics.FreetypeGL.RGBA (RGBA(..))
import           Graphics.FreetypeGL.Shader (Shader)
import qualified Graphics.FreetypeGL.Shader as Shader
import           Graphics.FreetypeGL.TextBuffer (TextBuffer, BoundingBox(..))
import qualified Graphics.FreetypeGL.TextBuffer as TextBuffer
import qualified Graphics.FreetypeGL.TextureAtlas as TextureAtlas
import           Graphics.FreetypeGL.TextureFont (TextureFont)
import qualified Graphics.FreetypeGL.TextureFont as TextureFont
import qualified Graphics.Rendering.OpenGL.GL as GL
import           System.IO.Unsafe (unsafePerformIO) -- for pure text metrics

data FTGLFont = FTGLFont
    { _getTextureFont :: !TextureFont
    , getTextBuffer :: !(MVar TextBuffer)
    , getShader :: !Shader
    }

data Font = Font
    { getLCDFont :: !FTGLFont
    , getScaledFont :: !FTGLFont
    }

type CleanupAction = (ThreadId, IO ())

{-# NOINLINE glResourceCleanupQueue #-}
glResourceCleanupQueue :: IORef [CleanupAction]
glResourceCleanupQueue = unsafePerformIO (newIORef [])

cleanQueuedGlResources :: IO ()
cleanQueuedGlResources =
    do
        tid <- myThreadId
        Exception.mask_ $
            atomicModifyIORef glResourceCleanupQueue (partition ((/= tid) . fst))
            >>= mapM_ snd

queueGlResourceCleanup :: ThreadId -> IO () -> IO ()
queueGlResourceCleanup tid act =
    atomicModifyIORef glResourceCleanupQueue $
    \queue -> ((tid, act) : queue, ())

-- | Load a TTF font from a file. This is CPS'd to take care of finalization
newFTGLFont :: Shader -> TextBuffer.RenderDepth -> Float -> FilePath -> IO FTGLFont
newFTGLFont shader renderDepth size path =
    Exception.mask_ $
    do
        textBuffer <- TextBuffer.new renderDepth shader
        manager <- TextBuffer.getFontManager textBuffer
        font <- FontManager.getFromFileName manager path size
        mvar <- newMVar textBuffer
        tid <- myThreadId
        _ <-
            mkWeakMVar mvar
            (queueGlResourceCleanup tid (TextBuffer.delete textBuffer))
        return (FTGLFont font mvar shader)

openFont :: Float -> FilePath -> IO Font
openFont size path =
    do
        initFreetypeGL
        lcdShader <- Shader.newTextShader
        scaleShader <- Shader.newDistanceFieldShader
        lcdFont <- newFTGLFont lcdShader TextBuffer.LCD_FILTERING_ON size path
        scaledFont <- newFTGLFont scaleShader TextBuffer.LCD_FILTERING_OFF size path
        withMVar (getTextBuffer scaledFont) $ \scaledTextBuffer -> do
            scaledManager <- TextBuffer.getFontManager scaledTextBuffer
            scaledAtlas <- FontManager.getAtlas scaledManager
            TextureAtlas.setMode scaledAtlas TextureAtlas.DistanceField
        return (Font lcdFont scaledFont)

data TextAttrs = TextAttrs
    { spacing :: !Float
    , gamma :: !Float
    , foregroundColor :: Color
    , mOutline :: Maybe Color
    , mUnderline :: Maybe Color
    , mOverline :: Maybe Color
    , mStrikethrough :: Maybe Color
    }

defTextAttrs :: TextAttrs
defTextAttrs =
    TextAttrs 0 1.0 (Color 1 1 1 1) Nothing Nothing Nothing Nothing

toMarkup :: Color -> TextAttrs -> Markup
toMarkup tintColor (TextAttrs spc gma fgColor outline underLn overLn strikeThru) =
    Markup
    { Markup.spacing = spc
    , Markup.gamma = gma
    , Markup.foregroundColor    = tint fgColor
    , Markup.backgroundColor    = toRGBA mempty
    , Markup.outlineColor       = tint <$> outline
    , Markup.underlineColor     = tint <$> underLn
    , Markup.overlineColor      = tint <$> overLn
    , Markup.strikethroughColor = tint <$> strikeThru
    }
    where
        toRGBA (Color r g b a) =
            Markup.RGBA (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)
        tint = toRGBA . modulate tintColor

withTextBufferStr ::
    FTGLFont -> Markup -> String ->
    (TextBuffer -> StateT TextBuffer.Pen IO a) ->
    IO (a, TextBuffer.Pen)
withTextBufferStr (FTGLFont font mvarTextBuffer _) markup str act =
    withMVar mvarTextBuffer $ \textBuffer -> do
        h <- TextureFont.height font
        d <- TextureFont.descender font
        TextBuffer.clear textBuffer
        (`runStateT` TextBuffer.Pen 0 (h + d)) $
            do
                TextBuffer.addText textBuffer markup font str
                act textBuffer

textMetric :: (TextureFont -> IO Float) -> Font -> R
textMetric f (Font (FTGLFont font _ _) _) = realToFrac . unsafePerformIO $ f font

fontHeight :: Font -> R
fontHeight = textMetric TextureFont.height

fontLineGap :: Font -> R
fontLineGap = textMetric TextureFont.lineGap

fontDescender :: Font -> R
fontDescender = textMetric TextureFont.descender

fontAscender :: Font -> R
fontAscender = textMetric TextureFont.ascender

getMatrix :: Maybe GL.MatrixMode -> IO Mat4
getMatrix mode = do
    matrix <- GL.get (GL.matrix mode)
    components <- GL.getMatrixComponents GL.ColumnMajor (matrix :: GL.GLmatrix Float)
    return $ Mat4.fromList16 $ map realToFrac components

-- Add everything to the text buffers' atlases before rendering anything:
prepareText :: FTGLFont -> String -> Markup -> IO ()
prepareText font str markup =
    void $ withTextBufferStr font markup str $ \_textBuffer -> return ()

bindTextShaderUniforms :: Affine -> Shader -> Mat4 -> Mat4 -> IO ()
bindTextShaderUniforms tr shader modelview projection =
    Shader.bindTextShaderUniforms shader Shader.TextShaderUniforms
    { Shader.textShaderModel = asMat4 tr
    , Shader.textShaderView = modelview
    , Shader.textShaderProjection = projection
    }

bindDistanceFieldShaderUniforms :: Affine -> Shader -> Mat4 -> Mat4 -> IO ()
bindDistanceFieldShaderUniforms tr shader modelview projection =
    Shader.bindDistanceFieldShaderUniforms shader Shader.DistanceFieldShaderUniforms
    { Shader.distanceFieldColor = RGBA 1 1 1 1
    , Shader.distanceFieldShaderModel = asMat4 tr
    , Shader.distanceFieldShaderView = modelview
    , Shader.distanceFieldShaderProjection = projection
    }

roundR :: R -> R
roundR x = fromIntegral (round x :: Integer)

-- This is virtually guaranteed to be smaller than a pixel in GL
-- coordinate space:
epsilon :: R
epsilon = 1/131072

roughly :: R -> R -> Bool
roughly x y = abs (x-y) < epsilon

-- | Extract an lcd-text compatible affine transformation, if possible
-- without noticeable loss of quality
lcdAffine :: Affine -> Maybe Affine
lcdAffine (M a b x
             c d y)
    | a `roughly` 1 -- cannot rotate around x, would need to swap r,b channels
    && b `roughly` 0
    && c `roughly` 0
      -- d need not be compared, vertical scaling should not interfere
      -- with subpixel rendering
      = Just $
        M 1 0 (roundR x)
          0 d (roundR y)
lcdAffine _ = Nothing

renderText :: Font -> String -> TextAttrs -> Affine -> Color -> IO (IO ())
renderText !font !str !attrs !tr !tintColor = do
    prepareText ftglFont str markup
    return $ void $ withTextBufferStr ftglFont markup str $ \textBuffer -> lift $ do
        projection <- getMatrix (Just GL.Projection)
        modelview <- getMatrix (Just (GL.Modelview 0))
        bindShader (getShader ftglFont) modelview projection
        TextBuffer.render textBuffer
    where
        (bindShader, ftglFont) =
            case lcdAffine tr of
            Nothing ->
                -- Font is not just translated, use the scaleFont (distance-field) to render
                (bindDistanceFieldShaderUniforms tr, getScaledFont font)
            Just tr' ->
                -- Font is just translated, we need to round to a
                -- pixel and we can use prettier LCD rendering:
                (bindTextShaderUniforms tr', getLCDFont font)
        markup = toMarkup tintColor attrs

-- | @textBoundingBox font str@ is the pixel-bounding box around text in @text font str@.
textBoundingBox :: Font -> String -> TextBuffer.BoundingBox
textBoundingBox (Font font _) str =
    fst $ unsafePerformIO $ withTextBufferStr font Markup.def str $ \textBuffer ->
    TextBuffer.boundingBox textBuffer

-- | @textBoundingWidth font str@ is the pixel-bounding width of the text in @text font str@.
textBoundingWidth :: Font -> String -> R
textBoundingWidth font str =
    realToFrac $ TextBuffer.bbWidth $ textBoundingBox font str

-- | @textAdvance font str@ is the x-advance of the text in
-- @text font str@, i.e: where to place the next piece of text.
textAdvance :: Font -> String -> R
textAdvance (Font font _) str =
    realToFrac $ unsafePerformIO $ do
        ((), TextBuffer.Pen advanceX _advanceY) <-
            withTextBufferStr font Markup.def str $ \_ -> return ()
        return advanceX