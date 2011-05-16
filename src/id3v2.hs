import Data.ByteString as B
import System
import System.IO
import Word
import Data.Bits

g::Word8->Int
g = fromIntegral 

sToB = (pack . Prelude.map (fromIntegral . fromEnum))
bToI bstr = f 0 0 
  where f x y | x < n =  f (x+1) ((idx x) + (y*2^8))
              | otherwise = y
        n = B.length bstr
        idx = g . B.index bstr

expectedTag =  sToB "ID3"

readHeader file = do
  handle <- openFile file ReadMode
  header <- hGet handle 10
  return (parse header,handle)

parse header = (t,v,f,s)
  where (t,h2) = B.splitAt 3 header
        (v,h3) = B.splitAt 2 h2
        (f,s') = B.splitAt 1 h2
        s = (bToI s')
        
readv2Tag file = do
  ((tag,version,flag,size),handle) <- readHeader file
  contents <- hGet handle size
  case (expectedTag == tag) of
    True-> return (Just contents)
    False -> return Nothing

printfile cstr file = do
  tag <- readv2Tag file
  case tag of
    Just a -> print a
    Nothing -> Prelude.putStrLn "File does not have an ID3v2 tag"  
    
controlStr = "File: %f\n" ++ 
             "Title: %t\t" ++ "Track: %n\n" ++
             "Artitst: %a\n" ++ 
             "Album: %l\t" ++ "Year: %y\n" ++
             "Comment: %c\t" ++ "Genre: %g"
             
printInfo = printfile controlStr         

main = do 
  args <- getArgs
  case args of
    [] -> Prelude.putStrLn "case exist usageStr of\n\tMaybe str -> putStrLn str\n\tNothing   -> error \"you are a failure\""
    as ->  mapM printInfo as >> return ()
    