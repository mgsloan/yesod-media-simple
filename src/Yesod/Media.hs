{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Yesod.Media
    ( RenderContent(..)
    , serve
    -- * Diagrams
    -- $diagrams
    , serveDiagram
    -- * Images
    -- $images
    , PixelList(..)
    , Jpeg(..)
    ) where

import Codec.Picture
import Codec.Picture.Types
import Control.Monad
import Control.Monad.Primitive (PrimMonad)
import Data.Word
import Diagrams.Prelude (Diagram, R2)
import Diagrams.TwoD (SizeSpec2D(..))
import Diagrams.Backend.Cairo
import Yesod

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Vector.Storable as V

serve :: RenderContent LiteHandler a => a -> IO ()
serve = warpEnv . liteApp . onMethod "GET" . dispatchTo . renderContent

class MonadHandler m => RenderContent m a where
    renderContent :: a -> m TypedContent

instance RenderContent (HandlerT site m) a
    => RenderContent (HandlerT site m) (HandlerT site m a) where
    renderContent f = f >>= renderContent

instance (MonadIO m, RenderContent (HandlerT site m) a)
    => RenderContent (HandlerT site m) (IO a) where
    renderContent f = liftIO f >>= renderContent

-- $diagrams
-- Cairo is used to render diagrams to pngs.

serveDiagram :: Diagram Cairo R2 -> IO ()
serveDiagram = serve

instance (MonadHandler m, MonadIO m)
    => RenderContent m (Diagram Cairo R2) where
    renderContent = renderContent . (Width 640, )

instance (MonadHandler m, MonadIO m)
    => RenderContent m (SizeSpec2D, Diagram Cairo R2) where
    renderContent (sz, img) = do
        let path = "out.png"
        png <- liftIO $ do
            renderCairo path sz img
            LBS.readFile path
        return $ TypedContent typePng (toContent png)

-- $images
-- The various types of 'Image' from JuicyPixels ("Codec.Picture") are servable
-- as pngs, unless the 'Jpeg' wrapper type is used.

renderPng :: (MonadHandler m, PngSavable a) => Image a -> m TypedContent
renderPng = return . TypedContent typePng . toContent . encodePng

instance MonadHandler m => RenderContent m (Image PixelRGBA8)  where renderContent = renderPng
instance MonadHandler m => RenderContent m (Image PixelRGB8)   where renderContent = renderPng
instance MonadHandler m => RenderContent m (Image PixelYA8)    where renderContent = renderPng
instance MonadHandler m => RenderContent m (Image Pixel8)      where renderContent = renderPng

#if MIN_VERSION_JuicyPixels(3,0,0)
instance MonadHandler m => RenderContent m (Image PixelRGBA16) where renderContent = renderPng
instance MonadHandler m => RenderContent m (Image PixelRGB16)  where renderContent = renderPng
instance MonadHandler m => RenderContent m (Image PixelYA16)   where renderContent = renderPng
instance MonadHandler m => RenderContent m (Image Pixel16)     where renderContent = renderPng
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

instance MonadHandler m => RenderContent m PixelList where
    renderContent = renderContent . pixelListToImage

-- | This type wraps image data that is appropriate for JPEG export, along with
--   the requested quality (from 0 to 100).
data Jpeg = Jpeg Word8 (Image PixelYCbCr8)

instance MonadHandler m => RenderContent m Jpeg where
    renderContent (Jpeg q x) =
        return $ TypedContent typeJpeg $ toContent $ encodeJpegAtQuality q x
