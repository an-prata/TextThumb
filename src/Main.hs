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
            let thumb = createImpressionThumbnail impression
            writePng outFile thumb
       _ -> putStrLn "Bad args"

    return ()

createImpressionThumbnail :: FileImpression -> Image PixelRGB8
createImpressionThumbnail impression
    = thumbnailImage $ margin blankPixel 4 . rowSpacing blankPixel 2 $ base
  where
    base = Thumbnail 144 164 (\x y
        -> (if not (isSpaceAt impression y x) then fillPixel else blankPixel))
    fillPixel = PixelRGB8 94 96 96
    blankPixel = PixelRGB8 255 255 255
