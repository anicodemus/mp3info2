import System as S
import Data.Array
import System.IO as I
-- import Word
import qualified Data.ByteString as B
import Header
import ID3v1
import Format

lookUpTable::FilePath->MP3Info->B.ByteString->Integer->[(Char,Formatable)]
lookUpTable file heads id1 size = [
  ('k', pack size),   -- [integer]
  ('f', pack (shortenFileName file)),   -- [string]
  ('F', pack file),   -- [string]
  ('a', pack (getField id1 Artist)),  -- [string]
  ('c', pack (getField id1 Comment)),   -- [string]
  ('g', pack (getGenre id1)),   -- [string]
  ('G', pack (fromIntegral (B.index id1 127)::Int)),  -- [integer]
  ('l', pack (getField id1 Album)),   -- [string]
  ('n', pack (getTrack id1)),   -- [integer]
  ('t', pack (getField id1 Title)),   --  [string]
  ('y', pack (getField id1 Year)),   --  [string]
  ('C', pack (getCopyright heads)),   --  [string]
  ('e', pack (getEmphasis heads)),   --  [string]
  ('E', pack (getCrc heads)),   --  [string]
  ('L', pack (getLayer heads)),   --  [string]
  ('O', pack (getOriginality heads)),   --  [string]
  ('o', pack (getMode heads)),   --  [string]
  ('p', pack (getPadded heads)),   --  [string]
  ('v', pack (getVersion heads)),   --  [float]
  ('u', pack (getInvalidFrames  heads)),   -- [Integer]
  ('b', pack (getValidFrames  heads)),   -- [Integer]
  ('Q', pack (getFrequency heads)),   --  [integer]
  ('q', pack (getFrequencyK heads)),   -- [integer]
  ('r', pack (getBitRate heads)),   -- varies
  ('m', pack (minutes heads)),   --  [integer]
  ('s', pack (secs heads)),   --  [integer] 
  ('S', pack (secondsS heads)),   -- [integer]
  ('%', pack "%")]

shortenFileName:: String -> String
shortenFileName = last . f
  where  f "" =  []
         f s  =  let (l, s') = break (== '/') s
                 in  l : case s' of
                   []      -> []
                   (_:s'') -> f s''

valid (Just a) (x:xs) size file = f (lookUpTable file  (x:xs) a size)
  where f t c = lookup c t
valid _ _ _ _ = error "Invalid tag"

printfile cstr file = do
  h <- openFile file ReadMode
  size <- hFileSize h
  tag <- readv1Tag h
  m3i <- openFile file ReadMode >>= readMP3Info
  printWithControlStr (valid tag m3i size file) (cstr ++ "\n")

-- Argument handling

controlStr =  "File: %F\nTitle:   %-30t Track: %n\nArtist:  %a\nAlbum:   %-30l Year:  %y\nComment: %-30c Genre: %g [%G]\n"
             
mp3str = "Media Type:  MPEG %2.1v Layer %L\nAudio:       %r kbps, %q kHz (%o)\nEmphasis:    %e\nCRC:         %E\nCopyright:   %C\nOriginal:    %O\nPadding:     %p\nLength:      %m:%02s\n"

printInfo = printfile controlStr

main = do
  args <- getArgs
  case args of
    [] -> putStrLn "case exist usageStr of\n\tMaybe str -> putStrLn str\n\tNothing   -> error \"you are a failure\""
    as ->  mapM printInfo as >> return ()