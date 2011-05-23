module BStrUtils (
  ByteReader, takeN, dropN, takeAsNum, runByteReader,
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

newtype ByteReader a = ByteReader (B.ByteString -> (a,B.ByteString))

instance Monad ByteReader where
  ByteReader a >>= fa2 = ByteReader (\b -> let (v,rest) = a b
                                               (ByteReader a2) = fa2 v
                                           in a2 rest)
  return k = ByteReader (\b -> (k,b))

runByteReader b (ByteReader f) = f b

takeN :: Int64 -> ByteReader B.ByteString
takeN n = (ByteReader f)
  where f b = let (a,c) = B.splitAt n b
                  (ByteReader g) = return a
              in g c

dropN :: Int64 -> ByteReader ()
dropN n = (ByteReader f)
  where f b = let c = B.drop n b
                  (ByteReader g) = return ()
              in g c                    

takeAsNum n = do
  i <- takeN n
  return (calcSize i)