import System as S
import Data.Array
-- import System.IO as I
import Word
import qualified Data.ByteString as B
import Header
import ID3v1

output c file heads id1 = case c of
    'F' -> putStr file   -- Filename with the path [string]
    'f' -> putStr (shortenFileName file)   -- Filename without the path [string]
    'k' -> print (getFileSize file)   -- File size in KB [integer]
    'a' -> printField id1 Artist  -- Artist [string]
    'c' -> printField id1 Comment   -- Comment [string]
    'g' -> printField id1 Gengre   -- Musical genre [string]
    'G' -> print (B.index id1 127)  -- Musical genre number [integer]
    'l' -> printField id1 Album   -- Album name [string]
    'n' -> printField id1 Track   -- Track [integer]
    't' -> printField id1 Title   -- Track Title [string]
    'y' -> printField id1 Year   -- Year [string]
    'C' -> putStr (getCopyright heads)   -- Copyright flag [string])
    'e' -> putStr (getEmphasis heads)   -- Emphasis [string]
    'E' -> putStr (getCrc heads)   -- CRC Error protection [string]
    'L' -> putStr (getLayer heads)   -- MPEG Layer [string]
    'O' -> putStr (getOriginality heads)   -- Original material flag [string]
    'o' -> putStr (getMode heads)   -- Stereo/mono mode [string]
    'p' -> putStr (getPadded heads)   -- Padding [string]
    'v' -> putStr (getVersion heads)   -- MPEG Version [float]
    'u' -> putStr (getInvalidFrames  heads)   -- Number of good audio frames
    'b' -> putStr (getValidFrames  heads)   -- Number of corrupt audio frames
    'Q' -> putStr (getFrequency heads)   -- Sampling frequency in Hz [integer]
    'q' -> putStr (getFrequencyK heads)   -- Sampling frequency in kHz [integer]
    'r' -> putStr (getBitRate heads)   -- Bit  Rate  in  kbps
    'm' -> print  (minutes heads)   -- Playing time: minutes only [integer])
    's' -> print  (secs heads)   -- Playing time: seconds only [integer] (usually used  in
    'S' -> print   seconds   -- Total playing time in seconds [integer]
    '%' -> putChar '%'   -- Single percent sign
    _  -> putChar c

shortenFileName:: String -> String
shortenFileName = last . f
  where  f "" =  []
         f s  =  let (l, s') = break (== '/') s
                 in  l : case s' of
                   []      -> []
                   (_:s'') -> f s''

printTag t cstr file = loop cstr
  where loop ('%':'f':cs) = putStr file >> loop cs
        loop ('%':c:cs) = printField t (cToF c) >> loop cs
        loop ('%':[]) = error "control string ended with an '%'"
        loop (c:cs) = putChar c >> loop cs
        loop [] = return ()
        

pT  cstr file id31 mp3i = loop cstr
  where
    loop [] = return ()
    loop s = go s
    go ('%':c:cs) = output c file id31 mp3i >> loop cs
    go ('%':[]) = error "control string ended with an '%'"

printfile cstr file = do
  tag <- readv1Tag file
  case tag of
    Just a -> printTag a cstr file
    Nothing -> putStrLn "File does not have an ID3v1 tag"

-- Argument handling

controlStr = "File: %f\n" ++
             "Title: %t\t" ++ "Track: %n\n" ++
             "Artitst: %a\n" ++
             "Album: %l\t" ++ "Year: %y\n" ++
             "Comment: %c\t" ++ "Genre: %g"

printInfo = printfile controlStr

main = do
  args <- getArgs
  case args of
    [] -> putStrLn "case exist usageStr of\n\tMaybe str -> putStrLn str\n\tNothing   -> error \"you are a failure\""
    as ->  mapM printInfo as >> return ()