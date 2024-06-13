{-# LANGUAGE GADTs #-}

module Thumbnails
    ( Thumbnail (..)
    , thumbnailImage
    , margin
    , rowSpacing
    ) where

import Codec.Picture

-- | A file thumbnail, as represented by its size and a function which maps pixel locations to
-- pixel colors.
data Thumbnail p = Pixel p => Thumbnail Int Int (Int -> Int -> p)

-- | Creates an @Image@ from the given @Thumbnail@.
thumbnailImage :: Pixel p => Thumbnail p -> Image p
thumbnailImage (Thumbnail w h f) = generateImage f w h

-- | Adds a margin to all sides of the @Thumbnail@, filling the gaps with the given @Pixel@.
margin :: Pixel p => p -> Int -> Thumbnail p -> Thumbnail p
margin fillPixel marginSize (Thumbnail w h f) = Thumbnail w h f'
  where
    f' x y = if x > marginSize && x < w - marginSize && y > marginSize && y < h - marginSize
        then f (x - marginSize) (y - marginSize)
        else fillPixel

-- | Spaces out rows of pixels - filling in the intermediate gaps with the given @Pixel@.
rowSpacing :: Pixel p => p -> Int -> Thumbnail p -> Thumbnail p
rowSpacing fillPixel spacing (Thumbnail w h f) = Thumbnail w h f'
  where
    f' x y = case y `mod` (spacing + 1) of
        0 -> f x (y `div` (spacing + 1))
        _ -> fillPixel
