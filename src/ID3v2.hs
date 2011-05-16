module ID3v2 ( readv2Tag ) where

import qualified Data.ByteString.Lazy as B
import System.IO
import Data.Word
import GHC.Int
import ID3vN
import BStrUtils
                   
newtype MP a = MP (B.ByteString -> (a,B.ByteString))

instance Monad MP where
  MP a >>= fa2 = MP (\b -> let (v,rest) = a b
                               MP a2 = fa2 v
                           in a2 rest)
  return k = MP (\b -> (k,b))

runMP b (MP f) = f b

takeN :: Int64 -> MP B.ByteString
takeN n = (MP f)
  where f b = let (a,c) = B.splitAt n b
                  (MP g) = return a
              in g c

dropN :: Int64 -> MP ()
dropN n = (MP f)
  where f b = let c = B.drop n b
                  (MP g) = return ()
              in g c                    

takeAsNum n = do
  i <- takeN n
  return (calcSize i)
  
end = strToBstr "\NUL\NUL\NUL\NUL"

framesOfInterest = [(strToBstr "TIT2", Title),
                    (strToBstr "TALB", Album),
--                    (strToBstr "TPE2", Artist),
                    (strToBstr "TPE1", Artist),
                    (strToBstr "TCON", Genre),
                    (strToBstr "TDRC", Year),
                    (strToBstr "COMM", Comment),
                    (strToBstr "TRCK", Track)]

isOfInterest b = lookup b framesOfInterest  

version v =  let a = B.index v 0
                 b = B.index v 1
                 in (fromIntegral a) + ((fromIntegral b) / 10)

exHeader flags = let a = B.head flags
                     b = div (w8i a) 64
                 in  b == 1 || b == 3

readFrames :: ID3Tag -> MP ID3Tag
readFrames tags = do
  (id,d) <- readFrameHeader
  case id == B.empty of  
    True  -> return tags
    False -> case isOfInterest id of
        Just a -> readFrames ((a,d):tags)
        Nothing -> readFrames tags

readFrameHeader = do                 
  id <- takeN 4
  case id == B.empty of
    True  -> return (id,id)
    False -> do
      size  <- takeAsNum 4
      dropN 2 -- flags
      dat   <- takeN size
      return (id,B.tail dat)
               
skipExtendedHead flag = case exHeader flag of
        False -> return ()
        True -> takeAsNum 4 >>= dropN

readTag :: MP (Integer,ID3Tag)
readTag = do
  tag <- takeN 3
  case B.isPrefixOf  (strToBstr "ID3") tag of
    False -> return (0,[])
    True -> do
      dropN 2 --version
      flag <- takeN 1
      size <- takeAsNum 4
      skipExtendedHead flag
      tags <- readFrames []
      return (fromIntegral size,tags)
      
collectTags b = fst (runMP b readTag)

readv2Tag file = do
  contents <- B.readFile file
  return (collectTags contents)

