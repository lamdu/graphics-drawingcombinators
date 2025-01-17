{-# LANGUAGE BangPatterns #-}
-- | Text-rendering and fonts component of Drawing Combinators

module Graphics.DrawingCombinators.Text
    ( Font, openFont, openFontNoLCD
    , fontAscender, fontDescender, fontHeight, fontLineGap
    , textAdvance, textPositions
    , BoundingBox(..), textBoundingBox, textBoundingWidth
    , renderText
    , TextAttrs(..), defTextAttrs
    ) where

import           Control.Concurrent.MVar
import qualified Control.Exception as Exception
import           Control.Monad (forM_, void)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State (StateT(..))
import           Data.Foldable (traverse_)
import           Data.IORef
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector.Unboxed.Mutable as Vector.Mutable
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vector
import           Graphics.DrawingCombinators.Affine
import           Graphics.DrawingCombinators.Cleanup (queueGlResourceCleanup)
import           Graphics.DrawingCombinators.Color
import           Graphics.FreetypeGL.Markup (Markup(..))
import qualified Graphics.FreetypeGL.Markup as Markup
import           Graphics.FreetypeGL.Shaders (TextShaderProgram(..), TextShaderUniforms(..))
import qualified Graphics.FreetypeGL.Shaders as Shaders
import           Graphics.FreetypeGL.TextBuffer (TextBuffer, BoundingBox(..))
import qualified Graphics.FreetypeGL.TextBuffer as TextBuffer
import           Graphics.FreetypeGL.TextureAtlas (TextureAtlas)
import qualified Graphics.FreetypeGL.TextureAtlas as TextureAtlas
import           Graphics.FreetypeGL.TextureFont (TextureFont)
import qualified Graphics.FreetypeGL.TextureFont as TextureFont
import           Graphics.Rendering.OpenGL.GL (($=))
import qualified Graphics.Rendering.OpenGL.GL as GL
import           System.IO.Unsafe (unsafePerformIO) -- for pure text metrics

data FTGLFont = FTGLFont
    { getTextureFont :: !TextureFont
    , _getTextBuffer :: !(MVar TextBuffer)
    , getShaders :: IO [TextShaderProgram]
    , getAtlas :: !TextureAtlas
    }

data Font = Font
    { getLCDFont :: !FTGLFont
    , getScaledFont :: !FTGLFont
    }

-- | Load a TTF font from a file. This is CPS'd to take care of finalization
newFTGLFont ::
    IO [TextShaderProgram] -> TextureAtlas.RenderDepth -> Float -> FilePath ->
    IO FTGLFont
newFTGLFont loadShaders renderDepth size path =
    Exception.mask_ $
    do
        atlas <- TextureAtlas.new 512 512 renderDepth
        font <- TextureFont.newFromFile atlas size TextureFont.RenderNormal path
        textBuffer <- TextBuffer.new
        mvar <- newMVar textBuffer
        shadersRef <- newIORef Nothing
        let getShade =
                do
                    mShaders <- readIORef shadersRef
                    case mShaders of
                        Just shaders -> return shaders
                        Nothing ->
                            do
                                shaders <- loadShaders
                                writeIORef shadersRef (Just shaders)
                                return shaders
        _ <-
            mkWeakMVar mvar $ queueGlResourceCleanup $
            do
                TextBuffer.delete textBuffer
                TextureFont.delete font
                TextureAtlas.delete atlas
                readIORef shadersRef
                    >>= traverse_
                        (mapM_ (GL.deleteObjectName . Shaders.shaderProgram))
        return (FTGLFont font mvar getShade atlas)

openFont :: Float -> FilePath -> IO Font
openFont size path =
    do
        lcdFont <- newFTGLFont Shaders.lcdShaders TextureAtlas.LCD_FILTERING_ON size path
        scaledFont <- newFTGLFont (return <$> Shaders.normalShader) TextureAtlas.LCD_FILTERING_OFF size path
        return (Font lcdFont scaledFont)

openFontNoLCD :: Float -> FilePath -> IO Font
openFontNoLCD size path =
    do
        font <- newFTGLFont (return <$> Shaders.normalShader) TextureAtlas.LCD_FILTERING_OFF size path
        return (Font font font)

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

withTextBuffer ::
    FTGLFont -> (TextBuffer -> StateT TextBuffer.Pen IO a) -> IO (a, TextBuffer.Pen)
withTextBuffer (FTGLFont font mvarTextBuffer _ _) act =
    withMVar mvarTextBuffer $ \textBuffer -> do
        h <- TextureFont.height font
        d <- TextureFont.descender font
        runStateT (act textBuffer) (TextBuffer.Pen 0 (h + d))

withTextBufferStr ::
    FTGLFont -> Markup -> Text ->
    (TextBuffer -> StateT TextBuffer.Pen IO a) ->
    IO (a, TextBuffer.Pen)
withTextBufferStr font markup !str act =
    withTextBuffer font $ \textBuffer -> do
        lift $ TextBuffer.clear textBuffer
        TextBuffer.addText textBuffer markup (getTextureFont font) str
        act textBuffer

textMetric :: (TextureFont -> IO Float) -> Font -> R
textMetric f (Font lcdFont _) =
    realToFrac . unsafePerformIO . f $ getTextureFont lcdFont

fontHeight :: Font -> R
fontHeight = textMetric TextureFont.height

fontLineGap :: Font -> R
fontLineGap = textMetric TextureFont.lineGap

fontDescender :: Font -> R
fontDescender = textMetric TextureFont.descender

fontAscender :: Font -> R
fontAscender = textMetric TextureFont.ascender

getMatrix :: Maybe GL.MatrixMode -> IO (GL.GLmatrix GL.GLfloat)
getMatrix mode = GL.get (GL.matrix mode)

-- Add everything to the text buffers' atlases before rendering anything:
prepareText :: FTGLFont -> Text -> Markup -> IO ()
prepareText font str markup =
    void $ withTextBufferStr font markup str $ \_textBuffer -> return ()

bindTextShaderUniforms ::
    Affine -> TextShaderProgram ->
    GL.GLmatrix GL.GLfloat -> GL.GLmatrix GL.GLfloat ->
    IO ()
bindTextShaderUniforms tr shader modelview projection =
    do
        GL.currentProgram $= Just (shaderProgram shader)
        let uniforms = shaderUniforms shader
        model <- toGLmatrix tr :: IO (GL.GLmatrix GL.GLfloat)
        GL.uniform (uniformModel uniforms) $= model
        GL.uniform (uniformView uniforms) $= modelview
        GL.uniform (uniformProjection uniforms) $= projection

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

renderText :: Font -> Text -> TextAttrs -> Affine -> Color -> IO (IO ())
renderText !font !str !attrs !tr !tintColor = do
    prepareText ftglFont str markup
    return $ void $ withTextBufferStr ftglFont markup str $ \textBuffer -> lift $ do
        TextureAtlas.uploadIfNeeded atlas
        projection <- getMatrix (Just GL.Projection)
        modelview <- getMatrix (Just (GL.Modelview 0))
        shaders <- getShaders ftglFont
        forM_ shaders $ \shader -> do
            bindTextShaderUniforms tr' shader modelview projection
            TextBuffer.render shader atlas textBuffer
    where
        (tr', ftglFont) =
            case lcdAffine tr of
            Nothing ->
                -- Font is not just translated, use the scaleFont to
                -- render
                (tr, getScaledFont font)
            Just lcdTr ->
                -- Font is just translated, we need to round to a
                -- pixel and we can use prettier LCD rendering:
                (lcdTr, getLCDFont font)
        markup = toMarkup tintColor attrs
        atlas = getAtlas ftglFont

-- | @textBoundingBox font str@ is the pixel-bounding box around text in @text font str@.
textBoundingBox :: Font -> Text -> TextBuffer.BoundingBox
textBoundingBox (Font font _) str =
    fst $ unsafePerformIO $ withTextBufferStr font Markup.def str $ \textBuffer ->
    TextBuffer.boundingBox textBuffer

-- | @textBoundingWidth font str@ is the pixel-bounding width of the text in @text font str@.
textBoundingWidth :: Font -> Text -> R
textBoundingWidth font str =
    realToFrac $ TextBuffer.bbWidth $ textBoundingBox font str

-- | @textAdvance font str@ is the x-advance of the text in
-- @text font str@, i.e: where to place the next piece of text.
textAdvance :: Font -> Text -> R
textAdvance (Font font _) str =
    realToFrac $ unsafePerformIO $ do
        ((), TextBuffer.Pen advanceX _advanceY) <-
            withTextBufferStr font Markup.def str $ \_ -> return ()
        return advanceX

-- | @textPositions font str@ is the x-advances of the text, after each char in the given text
textPositions :: Font -> Text -> Vector R
textPositions (Font font _) !str =
    unsafePerformIO $
    do
        pos <- Vector.Mutable.new (Text.length str)

        let go _  _     Nothing  []     = pure ()
            go !i !xpos Just{}   []     = do
                Vector.Mutable.write pos i xpos
            go !i !xpos (Just p) (c:cs) =
                do
                    g <- TextureFont.glyph c textureFont
                    kerning <- realToFrac <$> TextureFont.glyphKerning p g
                    advanceX <- realToFrac <$> TextureFont.glyphAdvanceX g
                    Vector.Mutable.write pos i (xpos+kerning)
                    go (i+1) (xpos+kerning + advanceX) (Just c) cs
            go !i !xpos Nothing  (c:cs) =
                do
                    advanceX <- TextureFont.glyph c textureFont >>= TextureFont.glyphAdvanceX
                    go (i+1) (xpos + realToFrac advanceX) (Just c) cs
        go (-1) 0 Nothing (Text.unpack str)
        Vector.unsafeFreeze pos
    where
        textureFont = getTextureFont font
