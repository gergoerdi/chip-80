module Target.TRS80.Video2x3 where

import CHIP80.Video

import Z80
import Z80.Utils
import Data.Word
import Data.Int
import Control.Monad
import Data.Bits
import Data.Char
import qualified Data.ByteString as BS
import Codec.Picture
import Data.Tuple.Extra (uncurry3)

windowWidth :: Word16
windowWidth = pictureWidth `div` 2

windowHeight :: Word16
windowHeight = pictureHeight `div` 3

encodeFromPng :: [Word8] -> BS.ByteString -> (Word8, Word8, [Word8])
encodeFromPng blocks bs = (byteWidth, byteHeight, bytes)
  where
    byteWidth = fromIntegral $ pixelWidth `div` 2
    byteHeight = fromIntegral $ pixelHeight `div` 3

    emptyLine = replicate (fromIntegral byteWidth) (False, False)

    bytes = concatMap encodeRow $ triples (emptyLine, emptyLine, emptyLine) . map doubles $ bits
      where
        encodeRow = uncurry3 $ zipWith3 encodeBlock
        encodeBlock (a, b) (c, d) (e, f) = blocks !! idx
          where
            idx =
                (if a then 0b000001 else 0b000000) .|.
                (if b then 0b000010 else 0b000000) .|.
                (if c then 0b000100 else 0b000000) .|.
                (if d then 0b001000 else 0b000000) .|.
                (if e then 0b010000 else 0b000000) .|.
                (if f then 0b100000 else 0b000000)

    bits = [ [ pixelOpacity pixel /= 0
             | x <- [0..pixelWidth - 1]
             , let pixel = pixelAt img x y
             ]
           | y <- [0..pixelHeight - 1]
           ]
      where
        img = convertRGBA8 dimg

    doubles :: [a] -> [(a, a)]
    doubles (x1:x2:xs) = (x1, x2):doubles xs
    doubles [] = []

    triples :: (a, a, a) -> [a] -> [(a, a, a)]
    triples y@(y1, y2, y3) = \case
        (x1:x2:x3:xs) -> (x1, x2, x3):triples y xs
        [x1, x2] -> [(x1, x2, y3)]
        [x1] -> [(x1, y2, y3)]
        [] -> []

    dimg = case decodePng bs of
        Right dimg -> dimg
        Left err -> error err
    pixelWidth = dynamicMap imageWidth dimg
    pixelHeight = dynamicMap imageHeight dimg
