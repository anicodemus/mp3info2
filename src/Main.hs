import qualified Data.ByteString.Lazy as B
import MP3
import ID3
import Genre
import Format
import IO
import System

-- Command line options too support

-- -G     Display  a  list  of  valid  genres and their associated numeric
--               codes. These are the only values accepted by the -g switch.

-- -h     Display a help page

-- -x     Display technical attributes of the MP3 file
-- -r a|m|v
--        Report bit rate of Variable Bit Rate (VBR) files as one  of  the
--        following  (See  the  section  below entitled Bit Rates for more
--        information):

--        a - Average bit rate [float](Note: this option also  causes  the
--               bit  rates  of  non-VBR files to be displayed as floating
--               point values).
--        m - Median bit rate [integer]
--        v - Simply  use  the  word  'Variable'  [string]  (this  is  the
-- --               default).
--        -p "FORMAT_STRING"

--               Print  MP3 attributes according to FORMAT_STRING.  FORMAT_STRING
--               is similar to a printf(3) format string i that  it  is  printed
--               verbatim   except  for  the  following  conversions  and  escape
--               sequences. Any conversion specifier may optionally  include  the
--               various alignment, precision, and field width modifiers accepted
--               by printf(3).  See the EXAMPLES section below  for  examples  of
--               how format strings are used in mp3info.

--               Conversion Specifiers -- see look up table
--               Escape Sequences

--                  \n     Newline
--                  \t     Horizontal tab
--                  \v     Vertical tab
--                  \b     Backspace
--                  \r     Carriage Return
--                  \f     Form Feed
--                  \a     Audible Alert (terminal bell)
--                  \xhh   Any  arbitrary  character specified by the hexidecimal
--                         number hh
--                  \ooo   Any arbitrary character specified by the octal  number
--                         ooo
--                  \\     A single backslash character


usage = "case exist usageStr of\n\tMaybe str -> putStrLn str\n\tNothing   -> error \"you are a failure\""

data BitRateFn = Variable | Median | Mean
-- Argument handling

controlStr =  "File: %F\nTitle:   %-30t Track: %n\nArtist:  %a\nAlbum:   %-30l Year:  %y\nComment: %-30c Genre: %g [%G]\n"
             
mp3str = "Media Type:  MPEG %2.1v Layer %L\nAudio:       %r kbps, %q kHz (%o)\nEmphasis:    %e\nCRC:         %E\nCopyright:   %C\nOriginal:    %O\nPadding:     %p\nLength:      %m:%02s\n"


lookUpTable ::FilePath -> MP3Info ->[ID3Tag] ->Integer ->[(Char,Formatable)]
lookUpTable file heads tags size = [
  ('k', pack size),   -- [integer]
  ('f', pack (shortenFileName file)),   -- [string]
  ('F', pack file),   -- [string]
  ('a', pack (getArtist tags)),  -- [string]
  ('c', pack (getComment tags)),   -- [string]
  ('g', pack (getGenre tags)),   -- [string]
  ('G', pack (getGenreNum tags)),  -- [integer]
  ('l', pack (getAlbum tags)),   -- [string]
  ('n', pack (getTrack tags)),   -- [integer]
  ('t', pack (getTitle tags)),   --  [string]
  ('y', pack (getYear tags)),   --  [string]
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
  ('r', pack "Variable" ),--(getBitRate heads)),   -- varies
  ('m', pack (minutes heads)),   --  [integer]
  ('s', pack (secs heads)),   --  [integer] 
  ('S', pack (secondsS heads)),   -- [integer]
  ('%', pack "%")]

shortenFileName :: String ->String
shortenFileName = last . f
  where  f "" =  []
         f s  =  let (l, s') = break (== '/') s
                 in  l : case s' of
                   []      -> []
                   (_:s'') -> f s''

valid file heads tags size = f (lookUpTable file heads tags size)
  where f t c = lookup c t
-- valid _ _ _  _ = error "Invalid tag"

printFile cstr file = do
  h        <- openFile file ReadMode
  size     <- hFileSize h
  tag1     <- readv1Tag file
  (n,tag2) <- readv2Tag file
  m3i      <- readMP3Info file n
  printWithControlStr (valid file m3i [tag1,tag2] size) cstr

data Option = Genres | UserDefined String | Technical | Default 

rank Genres          = 4 
rank (UserDefined _) = 3
rank Technical       = 2
rank Default         = 1 

chooseOpt o1 o2 = if (rank o1) > (rank o2) then  o1 else o2

parseOptions = loop Default
  where loop option ("-G":xs) = loop (chooseOpt option Genres) xs
        loop option ("-x":xs) = loop (chooseOpt option Technical) xs
        loop option ("-p":y:xs) = loop (chooseOpt option (UserDefined y)) xs
        loop option xs = (option,xs)
        
doFiles s files = mapM (printFile s) files >> return ()
dofiles s [] = putStrLn "No MP3 files specified!"                  

run (Default, []) = putStrLn usage
run (o,files) = case o of
  Genres -> printGenres
  UserDefined s -> doFiles s files
  Technical -> doFiles (controlStr++mp3str++"\n") files
  Default ->  doFiles (controlStr++"\n") files

main = do
  args <- getArgs
  run (parseOptions args)