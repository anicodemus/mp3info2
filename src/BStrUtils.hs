module BStrUtils (
  cw8, w8i,strToBstr,calcSize,clean, substring, toI
  )where

import qualified Data.ByteString.Lazy as B
import Data.Word
import GHC.Int

cw8::Char->Word8
cw8 = fromIntegral . fromEnum

w8i::Word8-> Int64
w8i = fromIntegral

strToBstr = B.pack . map cw8

calcSize = foldl1 (\x y-> (x*2^8)+y) . map w8i . B.unpack
  
clean = B.takeWhile (0/=)

toI :: B.ByteString -> Integer
toI b = f b 0
  where f b n | B.null b = fromIntegral n
              | otherwise = f (B.tail b) (10*n + ((B.head b)-48))

substring b i j = B.take j (B.drop i b)
