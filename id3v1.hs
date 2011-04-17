module ID3v1 where

import System as S
import Data.Array
import System.IO as I
import Word
import qualified Data.ByteString as B
import Data.Bits

-- Field Defintions

data Field =  Tag | Title | Artist | Album | Year | Comment | Track | Genre
           deriving Enum

size Tag = 3
size Year = 4
size Track = 1
size Genre = 1
size _ = 30

start Tag = 0
start Track = (start Comment) + (size Comment) - 1
start f = let p = pred f in (start p) + (size p)

cToF c = case c of
  't'-> Title
  'a'-> Artist
  'l'-> Album
  'y'-> Year
  'c'-> Comment
  'n'-> Track
  'g'-> Genre
  _ -> error ("improper flag (" ++ [c] ++ ") in control string")

-- IO definitions

clean = B.takeWhile (0/=)
substring b i j = B.take j (B.drop i b)

getTag b f = clean (substring b (start f) (size f))

printField b Track = if c == 0 && d > 0 then I.putStr (show d) else return ()
  where i = start Track 
        c = B.index b (i-1)
        d = B.index b (i)

printField b Genre = print (genreArray ! i)
  where i = B.index b (start Genre)

printField b f = B.putStr (getTag b f)
    
-- File related code

expectedTag = B.pack [84,65,71] -- "TAG"

readTag file = do
  h <- openFile file ReadMode
  hSeek h SeekFromEnd (-128)
  B.hGetContents h

readv1Tag file = do
  tag <- readTag file
  case (B.isPrefixOf expectedTag tag) of
    True-> return (Just tag)
    False -> return Nothing

printTag t cstr file = f cstr
  where f ('%':'f':cs) = I.putStr file >> f cs
        f ('%':c:cs) = printField t (cToF c) >> f cs
        f ('%':[]) = error "control string ended with a '%'"
        f (c:cs) = putChar c >> f cs
        f [] = return ()
  
printfile cstr file = do
  tag <- readv1Tag file
  case tag of
    Just a -> printTag a cstr file
    Nothing -> I.putStrLn "File does not have an ID3v1 tag"  

-- Argument handling
    
controlStr = "File: %f\n" ++ 
             "Title: %t\t" ++ "Track: %n\n" ++
             "Artitst: %a\n" ++ 
             "Album: %l\t" ++ "Year: %y\n" ++
             "Comment: %c\t" ++ "Genre: %g"
             
printInfo = printfile controlStr             
-- Data --

genreArray = array (0,255) $ Prelude.zip [0..] genres
genres = [
  "Blues", "Classic Rock", "Country", "Dance", "Disco", "Funk","Grunge", 
  "Hip-Hop", "Jazz", "Metal", "New Age", "Oldies", "Other", "Pop", "R&B",          "Rap", "Reggae", "Rock", "Techno","Industrial", "Alternative", "Ska", 
  "Death Metal", "Pranks","Soundtrack", "Euro-Techno", "Ambient", 
  "Trip-Hop", "Vocal","Jazz+Funk", "Fusion", "Trance", "Classical",
  "Instrumental", "Acid", "House", "Game", "Sound Clip", "Gospel", "Noise",
  "Alternative Rock", "Bass", "Soul", "Punk", "Space", "Meditative",
  "Instrumental Pop", "Instrumental Rock", "Ethnic", "Gothic","Darkwave", 
  "Techno-Industrial", "Electronic", "Pop-Folk", "Eurodance", "Dream", 
  "Southern Rock", "Comedy", "Cult", "Gangsta", "Top 40", "Christian Rap", 
  "Pop/Funk", "Jungle","Native US", "Cabaret", "New Wave", "Psychadelic", "Rave",
  "Showtunes", "Trailer", "Lo-Fi", "Tribal", "Acid Punk", "Acid Jazz",
  "Polka", "Retro", "Musical", "Rock & Roll", "Hard Rock", "Folk", "Folk-Rock",
  "National Folk", "Swing", "Fast Fusion", "Bebob", "Latin", "Revival",
  "Celtic", "Bluegrass", "Avantgarde", "Gothic Rock", "Progressive Rock",
  "Psychedelic Rock", "Symphonic Rock", "Slow Rock", "Big Band", "Chorus",
  "Easy Listening", "Acoustic", "Humour", "Speech", "Chanson", "Opera",
  "Chamber Music", "Sonata", "Symphony", "Booty Bass", "Primus", "Porn Groove",
  "Satire", "Slow Jam", "Club", "Tango", "Samba", "Folklore", "Ballad",
  "Power Ballad", "Rhythmic Soul", "Freestyle", "Duet", "Punk Rock",
  "Drum Solo", "Acapella", "Euro-House", "Dance Hall", "Goa", "Drum & Bass",
  "Club - House", "Hardcore", "Terror", "Indie", "BritPop", "Negerpunk",
  "Polsk Punk", "Beat", "Christian Gangsta Rap", "Heavy Metal", "Black Metal",
  "Crossover", "Contemporary Christian", "Christian Rock", "Merengue", "Salsa",
  "Thrash Metal", "Anime", "JPop", "Synthpop"]
         ++ (Prelude.replicate 108 "Unknown")


                   
