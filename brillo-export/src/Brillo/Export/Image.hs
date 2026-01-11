-- author: Samuel Gélineau (gelisam)
-- in response to https://www.reddit.com/r/haskell/comments/3u5s4e/is_there_a_way_to_write_the_frames_of_a_Brillo/
-- slightly improved
-- Modified for Brillo

{-# LANGUAGE CPP, RankNTypes, ScopedTypeVariables #-}
module Brillo.Export.Image
    ( Size
    , Animation
    , withBrilloState
    , withImage
    , withImages
    , exportPictureToFormat
    , exportPicturesToFormat
    ) where

import Codec.Picture.Types (Image(..), Pixel, PixelRGBA8, PixelRGB8, componentCount)
import Control.Exception (bracket, bracket_)
import Control.Monad (forM_, when)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Proxy (Proxy(..))
import Data.Vector.Storable (unsafeFromForeignPtr0)
import qualified Brillo.Rendering as Brillo
import Graphics.GL -- as GL*
import qualified Graphics.UI.GLFW as GLFW
import Foreign (newForeignPtr_)
import Foreign.Marshal.Array (allocaArray)
import Text.Printf (printf)
import GHC.Int
#ifdef linux_HOST_OS
import qualified Graphics.UI.GLUT as GLUT
#endif
import Prelude hiding (concat)

type Size = (Int, Int)
type Animation = Float -> Brillo.Picture

-- | Save a Brillo Picture to a file.
exportPictureToFormat :: (FilePath -> Image PixelRGBA8 -> IO ()) -- ^ function that saves an intermediate representation to a format. Written with writeXY from Codec.Picture in mind
                      -> Size                -- ^ (width, heigth) in pixels - as in Brillo.Display
                      -> Brillo.Color         -- ^ Background color
                      -> FilePath -> Brillo.Picture -> IO ()
exportPictureToFormat savefunc size bgc f p = do
    withBrilloState size $ \s -> do
      withImage size bgc s p $ \img -> do
        savefunc f img

-- | Acquire the Brillo.State required by the withImage* functions.
-- This allows the same OpenGL surface (of the given size) to be reused several times, which in turn makes Brillo bitmaps faster to render because their textures are kept in video memory.
withBrilloState :: Size -> (Brillo.State -> IO a) -> IO a
withBrilloState size body = do
#ifdef linux_HOST_OS
    _ <- GLUT.exit                     -- otherwise 'illegal reinitialization'
    (_,_) <- GLUT.getArgsAndInitialize -- needed for text  https://github.com/elisehuard/game-in-haskell/pull/3
#endif
    s <- Brillo.initState
    withGLFW $ do
      GLFW.windowHint (GLFW.WindowHint'Visible False)
      withWindow size "Brillo.Export.Image context" Nothing Nothing $ \window -> do
        GLFW.makeContextCurrent (Just window)
        body s

-- | A bracket API for GLFW.setErrorCallback which makes it easier to throw an exception upon failure.
withThrowGLFWError :: ((forall r. IO r) -> IO a) -> IO a
withThrowGLFWError body = do
    errorMessageRef <- newIORef "GLFW failed without an error message"

    let throwGLFWError :: IO r
        throwGLFWError = do
          errorMessage <- readIORef errorMessageRef
          error errorMessage

        errorCallback :: GLFW.ErrorCallback
        errorCallback _ errorMessage = do
          writeIORef errorMessageRef errorMessage

        acquire :: IO ()
        acquire = GLFW.setErrorCallback (Just errorCallback)

        release :: IO ()
        release = GLFW.setErrorCallback Nothing

    bracket_ acquire release $ do
      body throwGLFWError

-- | A bracket API for GLFW.init which throws an exception on failure.
withGLFW :: IO a -> IO a
withGLFW body = do
    withThrowGLFWError $ \throwGLFWError -> do
      bracket acquire release $ \glfwIsInitialized -> do
        if glfwIsInitialized then body else throwGLFWError
  where
    acquire :: IO Bool
    acquire = GLFW.init

    release :: Bool -> IO ()
    release glfwIsInitialized = when glfwIsInitialized GLFW.terminate

-- A bracket API for GLFW.createWindow which throws an exception on failure.
-- Must be called within withGLFW.
withWindow :: Size -> String -> Maybe GLFW.Monitor -> Maybe GLFW.Window
           -> (GLFW.Window -> IO a) -> IO a
withWindow (width, height) title monitor sharedContext body = do
    withThrowGLFWError $ \throwGLFWError -> do
      bracket acquire release $ \maybeWindow -> case maybeWindow of
        Just window -> body window
        Nothing -> throwGLFWError
  where
    acquire :: IO (Maybe GLFW.Window)
    acquire = GLFW.createWindow width height title monitor sharedContext

    release :: Maybe GLFW.Window -> IO ()
    release = mapM_ GLFW.destroyWindow




-- | Save a series of Brillo Picture to files of spcified format.
exportPicturesToFormat :: (FilePath -> Image PixelRGBA8 -> IO ()) -- ^ function that saves an intermediate representation to a format. Written with writeXY from Codec.Picture in mind
                       -> Size                -- ^ (width, height) in pixels - as in Brillo.Display
                       -> Brillo.Color         -- ^ background color
                       -> FilePath            -- ^ must contain "%d", will be replaced by frame number
                       -> Animation           -- ^ function that maps from point in time to Picture. analog to Brillo.Animation
                       -> [Float]             -- ^ list of points in time at which to evaluate the animation
                       -> IO ()
exportPicturesToFormat savefunc size bgc f anim ts = do
    withBrilloState size $ \s -> do
      forM_ (zip [1..] ts) $ \(n, t) -> do
        let filename = printf f (n :: Int)
        let picture = anim t
        withImage size bgc s picture $ \img -> do
          savefunc filename img


class Pixel pixel => OpenGLPixel pixel where
  openGLPixelFormat :: proxy pixel -> GLenum
  openGLPixelType   :: proxy pixel -> GLenum

instance OpenGLPixel PixelRGBA8 where
  openGLPixelFormat _ = GL_RGBA
  openGLPixelType   _ = GL_UNSIGNED_BYTE

instance OpenGLPixel PixelRGB8 where
  openGLPixelFormat _ = GL_RGB
  openGLPixelType   _ = GL_UNSIGNED_BYTE

-- | convert a Brillo 'Picture' into an 'Image'.
withImage :: forall pixel a. OpenGLPixel pixel
          => Size                -- ^ (width, height) in pixels - as in Brillo.Display
          -> Brillo.Color         -- ^ Background color
          -> Brillo.State         -- ^ Obtained via 'withBrilloState'
          -> Brillo.Picture
          -> (Image pixel -> IO a) -> IO a
withImage (windowWidth, windowHeight) bgc s p body = do
    let bytesPerPixel :: Int
        bytesPerPixel = componentCount (undefined :: pixel)

        pixelFormat :: GLenum
        pixelFormat = openGLPixelFormat (Proxy :: Proxy pixel)

        pixelType :: GLenum
        pixelType = openGLPixelType (Proxy :: Proxy pixel)

    --- the drawn image is flipped ([rowN,...,row1]) so we need to draw it upside down
    --- I guess this is because the origin is specified as topleft and bottomleft by different functions
    let flippedPicture :: Brillo.Picture
        flippedPicture = Brillo.Scale 1 (-1) p
    drawReadBuffer (windowWidth, windowHeight) bgc s flippedPicture
    allocaArray (windowWidth * windowHeight * bytesPerPixel) $ \imgData -> do
      let wW = fromIntegral windowWidth  :: GHC.Int.Int32
      let wH = fromIntegral windowHeight :: GHC.Int.Int32
      glReadPixels 0 0 wW wH pixelFormat pixelType imgData
      foreignPtr <- newForeignPtr_ imgData
      let vector = unsafeFromForeignPtr0 foreignPtr (windowWidth * windowHeight * bytesPerPixel)
      let image :: Image pixel
          image = Image windowWidth windowHeight vector
      
      body image

withImages :: OpenGLPixel pixel
           => Size                -- ^ (width, height) in pixels - as in Brillo.Display
           -> Brillo.Color         -- ^ Background color
           -> Brillo.State         -- ^ Obtained via 'withBrilloState'
           -> [Brillo.Picture]
           -> ([Image pixel] -> IO a) -> IO a
withImages _ _ _ [] body = body []
withImages size bgc s (p:ps) body = do
  withImage size bgc s p $ \image -> do
    withImages size bgc s ps $ \images -> do
      body (image:images)

drawReadBuffer :: Size
          -> Brillo.Color -- ^ Background color
          -> Brillo.State -> Brillo.Picture -> IO ()
drawReadBuffer (windowWidth, windowHeight) bg s p = do
    glDrawBuffer GL_BACK
    -- Set viewport to match the window size
    glViewport 0 0 (fromIntegral windowWidth) (fromIntegral windowHeight)
    Brillo.withClearBuffer bg $ Brillo.withModelview (windowWidth, windowHeight) $ do
                                                           glColor3f 0 0 0
                                                           Brillo.renderPicture s 1 p
    glReadBuffer GL_BACK 
