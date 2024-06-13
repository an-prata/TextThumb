module Main (main) where

import System.Environment
import Codec.Picture

import FileContent
import Thumbnails

main :: IO ()
main = do
    args <- getArgs

    case args of
       [inFile, outFile] -> do
            impression <- readImpression inFile
            let background = PixelRGB8 255 255 255
            let fill = PixelRGB8 94 96 96
            let thumb = createImpressionThumbnail background fill impression
            writePng outFile thumb

       _ -> putStrLn "Bad args"

    return ()

createImpressionThumbnail :: Pixel p => p -> p -> FileImpression -> Image p
createImpressionThumbnail background fill impression
    = thumbnailImage $ margin background 4 . rowSpacing background 2 $ base
  where
    base = Thumbnail 144 164 (\x y
        -> (if not (isSpaceAt impression y x) then fill else background))
