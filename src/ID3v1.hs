module ID3v1 ( readv1Tag ) where

import System.IO as I
import qualified Data.ByteString.Lazy as B
import ID3vN
import BStrUtils

size Tag = 3
size Year = 4
size Track = 1
size Genre = 1
size _ = 30

start Tag = 0
start Track = (start Comment) + (size Comment) - 1
start f = let p = pred f in (start p) + (size p)

getTag b Track = let i = getT b
                  in strToBstr (show i)

getTag b f = substring b (start f) (size f)

getG b = fromIntegral $ B.index b (start Genre) 
getT b = fromIntegral $ B.index b (start Track) 

readv1Tag file = do
  h <- openFile file ReadMode
  tag <- hSeek h SeekFromEnd (-128) >> B.hGetContents h
  case (B.isPrefixOf (strToBstr "TAG") tag) of
    True  -> return (map (\f -> (f, getTag tag f)) (enumFrom Title))
    False -> return []
