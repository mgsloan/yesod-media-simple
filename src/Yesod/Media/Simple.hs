{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

module Yesod.Media.Simple
    ( serve
    , serveHandler
    , RenderContent(..)
    -- * Diagrams
    -- $diagrams
    , serveDiagram
    -- * Images
    -- $images
    , PixelList(..)
    , Jpeg(..)
    , imageToDiagramEmb
    , imageToDiagramExt
    , diagramToImage
    ) where

import           Codec.Picture
import           Codec.Picture.Types
import           Control.Exception (finally)
import qualified Data.ByteString.Lazy as LBS
import           Data.Monoid (mempty)
import qualified Data.Vector.Storable as V
import           Data.Word
import           Diagrams.Backend.Cairo
import           Diagrams.Core
import           Diagrams.Prelude (Diagram, R2)
import           Diagrams.TwoD (SizeSpec2D(..))
import qualified Diagrams.TwoD.Image as Dia
import           System.Directory (getTemporaryDirectory)
import           System.Environment (lookupEnv, setEnv, unsetEnv)
import           System.IO (openTempFile, hClose)
import           Yesod

serve :: RenderContent a => a -> IO ()
serve = useDefaultPort . warpEnv . liteApp . onMethod "GET" . dispatchTo . renderContent

serveHandler :: RenderContent a => LiteHandler a -> IO ()
serveHandler = useDefaultPort . warpEnv . liteApp . onMethod "GET" . dispatchTo . (renderContent =<<)

class RenderContent a where
    renderContent :: a -> HandlerT site IO TypedContent

instance RenderContent a => RenderContent (IO a) where
    renderContent f = liftIO f >>= renderContent

--------------------------------------------------------------------------------
-- $diagrams
-- Cairo is used to render diagrams to pngs.

serveDiagram :: Diagram Cairo R2 -> IO ()
serveDiagram = serve

instance RenderContent (Diagram Cairo R2) where
    renderContent = renderContent . (Width 640, )

instance RenderContent (SizeSpec2D, Diagram Cairo R2) where
    renderContent (sz, dia) = do
        png <- liftIO $ do
            path <- getTempPath "out.png"
            renderCairo path sz dia
            LBS.readFile path
        return $ TypedContent typePng (toContent png)

--------------------------------------------------------------------------------
-- $images
-- The various types of 'Image' from JuicyPixels ("Codec.Picture") are servable
-- as pngs, unless the 'Jpeg' wrapper type is used.

renderPng :: (MonadHandler m, PngSavable a) => Image a -> m TypedContent
renderPng = return . TypedContent typePng . toContent . encodePng

instance RenderContent (Image PixelRGBA8)  where renderContent = renderPng
instance RenderContent (Image PixelRGB8)   where renderContent = renderPng
instance RenderContent (Image PixelYA8)    where renderContent = renderPng
instance RenderContent (Image Pixel8)      where renderContent = renderPng

#if MIN_VERSION_JuicyPixels(3,0,0)
instance RenderContent (Image PixelRGBA16) where renderContent = renderPng
instance RenderContent (Image PixelRGB16)  where renderContent = renderPng
instance RenderContent (Image PixelYA16)   where renderContent = renderPng
instance RenderContent (Image Pixel16)     where renderContent = renderPng
#endif

-- | This type wraps RGB8 image data stored in nested lists, so that you don't
--   need to use "Codec.Picture".  The inner list is one row of the image, and
--   the tuple elements are the red / green / blue values, respectively.
data PixelList = PixelList Int Int [[(Word8, Word8, Word8)]]

pixelListToImage :: PixelList -> Image PixelRGB8
pixelListToImage (PixelList w h pixels) =
    Image w h $ V.fromList $ concat $ concat
        [ [ [r, g, b]
          | (r, g, b) <- take w $ row ++ repeat (0,0,0)
          ]
        | row <- take h $ pixels ++ repeat (repeat (0,0,0))
        ]

instance RenderContent PixelList where
    renderContent = renderContent . pixelListToImage

-- | This type wraps image data that is appropriate for JPEG export, along with
--   the requested quality (from 0 to 100).
data Jpeg = Jpeg Word8 (Image PixelYCbCr8)

instance RenderContent Jpeg where
    renderContent (Jpeg q x) =
        return $ TypedContent typeJpeg $ toContent $ encodeJpegAtQuality q x

-- | Convert a JuicyPixels 'Image' to an image embedded in the
-- 'Diagram'. Note that this image is *NOT* renderable by the Cairo
-- backend, which is used by other functions in this module.
imageToDiagramEmb :: (Renderable (Dia.DImage Dia.Embedded) b) => DynamicImage -> Diagram b R2
imageToDiagramEmb img =
    imageFromDynamicImage img $ \img' ->
        let w = fromIntegral (imageWidth img')
            h = fromIntegral (imageHeight img')
         in Dia.image (Dia.DImage (Dia.ImageRaster img) w h mempty)

-- | Write a JuicyPixels 'Image' to a temporary file
imageToDiagramExt :: (Renderable (Dia.DImage Dia.External) b) => DynamicImage -> IO (Diagram b R2)
imageToDiagramExt img =
    imageFromDynamicImage img $ \img' -> do
        path <- getTempPath "out.png"
        either fail (\_ -> return ()) =<< writeDynamicPng path img
        let w = fromIntegral (imageWidth img')
            h = fromIntegral (imageHeight img')
        return (Dia.image (Dia.DImage (Dia.ImageRef path) w h mempty))

imageFromDynamicImage :: DynamicImage -> (forall a. Image a -> b) -> b
imageFromDynamicImage (ImageY8     img) f = f img
imageFromDynamicImage (ImageY16    img) f = f img
imageFromDynamicImage (ImageYF     img) f = f img
imageFromDynamicImage (ImageYA8    img) f = f img
imageFromDynamicImage (ImageYA16   img) f = f img
imageFromDynamicImage (ImageRGB8   img) f = f img
imageFromDynamicImage (ImageRGB16  img) f = f img
imageFromDynamicImage (ImageRGBF   img) f = f img
imageFromDynamicImage (ImageRGBA8  img) f = f img
imageFromDynamicImage (ImageRGBA16 img) f = f img
imageFromDynamicImage (ImageYCbCr8 img) f = f img
imageFromDynamicImage (ImageCMYK8  img) f = f img
imageFromDynamicImage (ImageCMYK16 img) f = f img

-- | Convert a 'Diagram' to a JuicyPixels 'Image'.
diagramToImage :: Diagram Cairo R2 -> Double -> Double -> IO (Either String DynamicImage)
diagramToImage dia w h = do
    path <- getTempPath "out.png"
    renderCairo path (Dims w h) dia
    readPng path

-- | Set PORT environment variable to 3000 if it's unset.  Tells
-- stdout which port it's listening to.
useDefaultPort :: IO () -> IO ()
useDefaultPort inner = do
    mport <- lookupEnv "PORT"
    case mport of
        Just port -> do
            putStrLn $ "Running server on localhost:" ++ port
            inner
        Nothing -> do
            setEnv "PORT" "3000"
            inner `finally` unsetEnv "PORT"

getTempPath :: FilePath -> IO FilePath
getTempPath base = do
    tempDir <- getTemporaryDirectory
    (path, handle) <- openTempFile tempDir base
    hClose handle
    return path
