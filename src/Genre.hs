module Genre (
  genreArray, genres, genreByNum, printGenres
  ) where

import Text.Printf
import BStrUtils
import Data.Array
import Data.List

genreByNum = (genreArray !)

genreArray = array (0,255) $ Prelude.zip [0..] $ map strToBstr genres

printMessage = putStr "Extended MP3 ID3 Genres\n=======================\n"

type Index = Maybe (Integer, [Char])

printColumn :: (Index,Index,Index) -> IO ()
printColumn (a,b,c) = do
  printIndex a
  printIndex b
  printIndex c
  putChar '\n'

printIndex (Just (n,a)) = printf "%3d %-22s" n a  
printIndex Nothing = return ()

printGenres = printMessage >> mapM printColumn cols >> return ()

dumbStrCmp [] [] = EQ
dumbStrCmp [] x = GT
dumbStrCmp x [] = LT
dumbStrCmp a b = let ac = head a
                     bc = head b
                 in case compare a b of
                   EQ -> dumbStrCmp (tail a) (tail b)
                   q -> q

compareSnd x y = dumbStrCmp (snd x) (snd y)

sortedGenres = sortBy compareSnd $ zip [1..147] genres

cols = let (a,x) = splitAt 50 sortedGenres
           (b,c) = splitAt 50 x
           in zip3 (toSize 50 a) (toSize 50 b) (toSize 50 c)
              
toSize n (x:xs) =  (Just x) : toSize (n-1) xs
toSize 0 _ = []
toSize n [] = replicate n Nothing

genres = [
  "Blues", "Classic Rock", "Country", "Dance", "Disco", "Funk","Grunge", 
  "Hip-Hop", "Jazz", "Metal", "New Age", "Oldies", "Other", "Pop", "R&B",          "Rap", "Reggae", "Rock", "Techno","Industrial", "Alternative", "Ska", 
  "Death Metal", "Pranks","Soundtrack", "Euro-Techno", "Ambient", 
  "Trip-Hop", "Vocal","Jazz+Funk", "Fusion", "Trance", "Classical",
  "Instrumental", "Acid", "House", "Game", "Sound Clip", "Gospel", "Noise",
  "Alt. Rock", "Bass", "Soul", "Punk", "Space", "Meditative",
  "Instrumental Pop", "Instrumental Rock", "Ethnic", "Gothic","Darkwave", 
  "Techno-Industrial", "Electronic", "Pop-Folk", "Eurodance", "Dream", 
  "Southern Rock", "Comedy", "Cult", "Gangsta Rap", "Top 40", "Christian Rap", 
  "Pop/Funk", "Jungle","Native American", "Cabaret", "New Wave", "Psychedelic", 
  "Rave", "Showtunes", "Trailer", "Lo-Fi", "Tribal", "Acid Punk", "Acid Jazz",
  "Polka", "Retro", "Musical", "Rock & Roll", "Hard Rock", "Folk", "Folk/Rock",
  "National Folk", "Swing", "Fast-Fusion", "Bebob", "Latin", "Revival",
  "Celtic", "Bluegrass", "Avantgarde", "Gothic Rock", "Progressive Rock",
  "Psychedelic Rock", "Symphonic Rock", "Slow Rock", "Big Band", "Chorus",
  "Easy Listening", "Acoustic", "Humour", "Speech", "Chanson", "Opera",
  "Chamber Music", "Sonata", "Symphony", "Booty Bass", "Primus", "Porn Groove",
  "Satire", "Slow Jam", "Club", "Tango", "Samba", "Folklore", "Ballad",
  "Power Ballad", "Rhythmic Soul", "Freestyle", "Duet", "Punk Rock",
  "Drum Solo", "A Cappella", "Euro-House", "Dance Hall", "Goa", "Drum & Bass",
  "Club-House", "Hardcore", "Terror", "Indie", "BritPop", "Negerpunk",
  "Polsk Punk", "Beat", "Christian Gangsta Rap", "Heavy Metal", "Black Metal",
  "Crossover", "Contemporary Christian ", "Christian Rock", "Merengue", "Salsa",
  "Thrash Metal", "Anime", "JPop", "Synthpop"]
         ++ (Prelude.replicate 108 "Unknown")

  