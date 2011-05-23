module ID3 (
  ID3Tag, Field (Tag, Title, Artist, Album, Year, Comment, Track, Genre),
  getArtist, getTitle, getAlbum, getComment,
  getYear, getGenre, getGenreNum, getTrack,
  readv1Tag, readv2Tag
  ) where

import qualified Data.ByteString.Lazy as B
import System.IO
import Genre
import GHC.Int
import BStrUtils

data Field =  Tag | Title | Artist | Album | Year | Comment | Track | Genre
           deriving (Enum,Eq)

type ID3Tag = [(Field,B.ByteString)]

getM f [] = B.empty
getM f (t:ts) = case (lookup f t) of
  Just a -> clean a
  Nothing -> getM f ts

getArtist = getM Artist
getTitle = getM Title
getAlbum = getM Album
getComment = getM Comment
getYear = getM Year
getGenreNum = toI . getM Genre
getTrack = toI . getM Track
getGenre = genreByNum . fromIntegral . getGenreNum

toNum:: B.ByteString -> Integer
toNum b | B.null b = 0
        | otherwise = fromIntegral (B.head b)

trim f b = substring b 0 (size f)

-- ID3v1 relevant code

size Tag = 3
size Year = 4
size Track = 1
size Genre = 1
size _ = 30

start Tag = 0
start Track = (start Comment) + (size Comment) - 1
start f = let p = pred f in (start p) + (size p)

getT b = fromIntegral $ B.index b (start Track)
getG b = fromIntegral $ B.index b (start Genre)

getTag b Track = (strToBstr . show . getT) b
getTag b Genre = (strToBstr . show . getG) b
getTag b f = substring b (start f) (size f)

getTags :: B.ByteString -> ID3Tag
getTags b = map (\f -> (f, getTag b f)) (enumFrom Title)

readv1Tag file = do
  h <- openFile file ReadMode
  tag <- hSeek h SeekFromEnd (-128) >> B.hGetContents h
  case B.isPrefixOf (strToBstr "TAG") tag of
    True  -> return (getTags tag)
    False -> return []

-- ID3v2 relevant code

end = strToBstr "\NUL\NUL\NUL\NUL"

framesOfInterest = [(strToBstr "TIT2", Title),
                    (strToBstr "TALB", Album),
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

readFrames :: ID3Tag -> ByteReader ID3Tag
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

readTag :: ByteReader (Integer,ID3Tag)
readTag = do
  tag <- takeN 3
  case  tag == (strToBstr "ID3") of
    False -> return (0,[])
    True -> do
      dropN 2 --version
      flag <- takeN 1
      size <- takeAsNum 4
      skipExtendedHead flag
      tags <- readFrames []
      return (fromIntegral size,tags)

collectTags b = fst (runByteReader b readTag)

readv2Tag file = do
  contents <- B.readFile file
  return (collectTags contents)

