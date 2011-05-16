module ID3vN (
  ID3Tag,
  getArtist, getTitle, getAlbum, getComment, 
  getYear, getGenre, getGenreNum, getTrack,
  Field (Tag, Title, Artist, Album, Year, Comment, Track, Genre)
  ) where

import Data.ByteString.Lazy as B
import Genre
import GHC.Int
import BStrUtils

data Field =  Tag | Title | Artist | Album | Year | Comment | Track | Genre
           deriving (Enum,Eq,Show)
                    
type ID3Tag = [(Field,B.ByteString)]

get = lookup

getM f [] = B.empty
getM f (t:ts) = case (get f t) of
  Just a -> clean a
  Nothing -> getM f ts

getArtist = getM Artist
getTitle = getM Title
getAlbum = getM Album
getComment = getM Comment
getYear = getM Year
getGenreNum = toNum . getM Genre
getTrack = toI . getM Track
getGenre = genreByNum . fromIntegral . getGenreNum

toNum:: B.ByteString -> Integer
toNum b | B.null b = 0
        | otherwise = fromIntegral ( B.head b)