-- | Text-rendering and fonts component of Drawing Combinators

module Graphics.DrawingCombinators.Text
    ( Font, withFont, withFontCatch
    , fontAscender, fontDescender, fontHeight
    , textAdvance
    , BoundingBox(..), textBoundingBox, textBoundingWidth
    , renderText
    , TextAttrs(..), defTextAttrs
    ) where

import           Control.Concurrent.MVar
import           Control.Exception (Exception)
import qualified Control.Exception as Exception
import           Control.Monad (void)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State (StateT(..))
import           Graphics.DrawingCombinators.Affine
import           Graphics.DrawingCombinators.Color
import qualified Graphics.FreetypeGL.FontManager as FontManager
import           Graphics.FreetypeGL.Init (initFreetypeGL)
import           Graphics.FreetypeGL.Markup (Markup(..))
import qualified Graphics.FreetypeGL.Markup as Markup
import           Graphics.FreetypeGL.Mat4 (Mat4)
import qualified Graphics.FreetypeGL.Mat4 as Mat4
import           Graphics.FreetypeGL.Shader (Shader)
import qualified Graphics.FreetypeGL.Shader as Shader
import           Graphics.FreetypeGL.TextBuffer (TextBuffer, BoundingBox(..))
import qualified Graphics.FreetypeGL.TextBuffer as TextBuffer
import           Graphics.FreetypeGL.TextureFont (TextureFont)
import qualified Graphics.FreetypeGL.TextureFont as TextureFont
import qualified Graphics.Rendering.OpenGL.GL as GL
import           System.IO.Unsafe (unsafePerformIO) -- for pure text metrics

data Font = Font
    { _getFont :: !TextureFont
    , _getTextBuffer :: !(MVar TextBuffer)
    , getShader :: Shader
    }

withFont :: Float -> FilePath -> (Font -> IO a) -> IO a
withFont =
    withFontCatch (Exception.throwIO :: Exception.SomeException -> IO a)

withNewTextBuffer :: (Shader -> TextBuffer -> IO a) -> IO a
withNewTextBuffer act = do
    -- TODO: Is it OK to do multiple GLEW inits?
    initFreetypeGL
    -- TODO: Don't leak shader
    shader <- Shader.newTextShader
    Exception.bracket
        (TextBuffer.new TextBuffer.LCD_FILTERING_ON shader)
        TextBuffer.delete (act shader)

catchOrElse :: Exception e => IO a -> (e -> IO b) -> (a -> IO b) -> IO b
catchOrElse act err success =
    Exception.mask $ \restore ->
    Exception.try (restore act) >>= either err (restore . success)

-- | Load a TTF font from a file. This is CPS'd to take care of finalization
withFontCatch :: Exception e => (e -> IO a) -> Float -> FilePath -> (Font -> IO a) -> IO a
withFontCatch openFontError size path act =
    withNewTextBuffer $ \shader textBuffer ->
    do
        manager <- TextBuffer.getFontManager textBuffer
        catchOrElse
            (FontManager.getFromFileName manager path size)
            openFontError $ \font -> do
                mvar <- newMVar textBuffer
                act (Font font mvar shader)

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
    Font -> Markup -> String ->
    (TextBuffer -> StateT TextBuffer.Pen IO a) ->
    IO (a, TextBuffer.Pen)
withTextBufferStr (Font font mvarTextBuffer _) markup str act =
    withMVar mvarTextBuffer $ \textBuffer -> do
        h <- TextureFont.height font
        d <- TextureFont.descender font
        TextBuffer.clear textBuffer
        (`runStateT` TextBuffer.Pen 0 (h + d)) $
            do
                TextBuffer.addText textBuffer markup font str
                act textBuffer

fontHeight :: Font -> R
fontHeight (Font font _ _) = realToFrac . unsafePerformIO $ TextureFont.height font

fontDescender :: Font -> R
fontDescender (Font font _ _) = realToFrac . unsafePerformIO $ TextureFont.descender font

fontAscender :: Font -> R
fontAscender (Font font _ _) = realToFrac . unsafePerformIO $ TextureFont.ascender font

getMatrix :: Maybe GL.MatrixMode -> IO Mat4
getMatrix mode = do
    matrix <- GL.get (GL.matrix mode)
    components <- GL.getMatrixComponents GL.ColumnMajor (matrix :: GL.GLmatrix Float)
    return $ Mat4.fromList16 $ map realToFrac components

-- Add everything to the text buffers' atlases before rendering anything:
prepareText :: Font -> String -> Markup -> IO ()
prepareText font str markup =
    void $ withTextBufferStr font markup str $ \_textBuffer -> return ()

renderText :: Font -> String -> TextAttrs -> Affine -> Color -> IO (IO ())
renderText font str attrs tr tintColor = do
    prepareText font str markup
    return $ void $ withTextBufferStr font markup str $ \textBuffer -> lift $ do
        projection <- getMatrix (Just GL.Projection)
        modelView <- getMatrix (Just (GL.Modelview 0))
        Shader.bindTextShaderUniforms (getShader font) Shader.TextShaderUniforms
            { Shader.textShaderModel = asMat4 tr
            , Shader.textShaderView = modelView
            , Shader.textShaderProjection = projection
            }
        TextBuffer.render textBuffer
    where
        markup = toMarkup tintColor attrs

-- | @textBoundingBox font str@ is the pixel-bounding box around text in @text font str@.
textBoundingBox :: Font -> String -> TextBuffer.BoundingBox
textBoundingBox font str =
    fst $ unsafePerformIO $ withTextBufferStr font Markup.def str $ \textBuffer ->
    TextBuffer.boundingBox textBuffer

-- | @textBoundingWidth font str@ is the pixel-bounding width of the text in @text font str@.
textBoundingWidth :: Font -> String -> R
textBoundingWidth font str =
    realToFrac $ TextBuffer.bbWidth $ textBoundingBox font str

-- | @textAdvance font str@ is the x-advance of the text in
-- @text font str@, i.e: where to place the next piece of text.
textAdvance :: Font -> String -> R
textAdvance font str =
    realToFrac $ unsafePerformIO $ do
        ((), TextBuffer.Pen advanceX _advanceY) <-
            withTextBufferStr font Markup.def str $ \_ -> return ()
        return advanceX
