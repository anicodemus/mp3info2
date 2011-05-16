module Format (
  Format,
  Formatable,
  parseFormatStr,
  pack,
  format,
  printWithControlStr
  ) where

import Text.ParserCombinators.Parsec
import qualified Data.ByteString.Lazy as B

data Formatable = forall a . Format a => Packed a
pack::Format a => a -> Formatable
pack = Packed
instance Format Formatable where
  format p (Packed a) = format p a

class Format a where
  format::FormatType->a->IO ()

{-# LANGUAGE TypeSynonymInstances #-}

instance Format String where format = formatS

formatS (Pad i) s = formatS None (padStr s i ' ')
formatS (Decimal i d) s = formatS (Pad i) (take d s)
formatS _ s = putStr s

instance Format Integer where format = formatI

formatI None i = putStr (show i)
formatI (Pad p) i = formatS (Pad p) (show i)
formatI (Zeros j) i = putNum i j
formatI (Decimal x y) i = formatS (Pad x) (padNum i y)

instance Format Int where format = formatIn

formatIn None i = putStr (show i)
formatIn (Pad p) i = formatS (Pad p) (show i)
formatIn (Zeros j) i = putNum i j
formatIn (Decimal x y) i = formatS (Pad x) (padNum i y)


instance Format B.ByteString where format = formatB

formatB (Pad i) s = formatB None (padBstr s i 32) -- 32 == ' '
formatB (Decimal i d) s = formatB (Pad i) (B.take (fromIntegral d) s)
formatB _ b = B.putStr b

instance Format Double where format = formatD

formatD None d = putNum d 0
formatD (Pad p) d = formatS (Pad p) (show d)
formatD (Zeros j) d = putNum d j
formatD (Decimal x y) d | y /= 0 = putNum (truncateX d y) x
                        | otherwise = putNum (truncate d) x

truncateX::Double->Int->Double
truncateX d i = p / t
  where t = 10^i
        p = fromIntegral (floor (d * t))

putNum i j= putStr (padNum i j)
padNum i j = padStr (show i) (-j) '0'
padStr str n c = if n < 0 then padding ++ str else str ++ padding
  where
    i = abs n
    l = length str
    padding = if (i-l) < 0 then "" else replicate (i-l) c

padBstr str n c = B.concat a
  where
    a = if n < 0 then [str,padding] else [padding,str]
    i = fromIntegral (abs n)
    l = B.length str
    padding = if (i-l) < 0 then B.empty else B.replicate (i-l) c

{-
Parsing Code
-}

data FormatType = Pad Int | Zeros Int | Decimal Int Int | None
                deriving Show

data Outputter = AsIs Char | Complex FormatType Char
               deriving Show


printOutput::(Format a)=>(Char-> Maybe a)->[Outputter]->IO ()
printOutput _ [] = return ()
printOutput lu (x:xs) = (f x) >> printOutput lu xs
  where f (AsIs c) = putChar c
        f (Complex t c) = g t (lu c)
        g t (Just a) = format t a
        g t Nothing = error "this shouldn't be, there is an error in the  parser"

printWithControlStr::(Format a)=>(Char-> Maybe a)->String->IO ()
printWithControlStr f s = let p = parse parseFormatStr "" s
                          in case p of
                            Right a -> printOutput f a
                            Left b -> print b

parseFormatStr::Parser [Outputter]
parseFormatStr = end <|> parseOutputter

end::Parser [Outputter]
end = eof >> return []

parseOutputter::Parser [Outputter]
parseOutputter = do
  o<-(try fmt) <|> escape <|> els
  rest <- parseFormatStr
  return (o:rest)

convert::String->Int
convert "" = 0
convert a = read a

decimal::Parser FormatType
decimal = do
  sign <- optionMaybe (char '-')
  a <- many digit
  char '.'
  b <- many digit
  return (Decimal (fix sign (convert a)) (convert b))

zero::Parser FormatType
zero = do
  char '0'
  digits <- many1 digit
  return (Zeros (convert digits))

fix (Just a) ds = -ds
fix Nothing ds = ds
fixS m ss = fix m (read ss)

pad::Parser FormatType
pad = do
  sign <- optionMaybe (char '-')
  digits <- many1 digit
  return (Pad (fixS sign digits))

fmt::Parser Outputter
fmt = do
  char '%'
  ft<- choice [(try decimal),(try zero), (try pad), return None]
  c <- oneOf "FfkacgGlntyCeELOopvubQqrmsS%"
  return (Complex ft c)

lk = [('n','\n'),('t','\t'),('v','\v'),
      ('b','\b'),('r','\r'),('f','\f'),('a','\a')]
ch = do 
  c <- anyChar
  case (lookup c lk) of
    Just a -> return (AsIs a)
    Nothing -> return (AsIs c)

ot = do
  a <- oct
  b <- oct
  c <- oct
  return (AsIs (octToChar a b c))
  
oct:: Parser Char
oct = oneOf "01234567"
oc c = (fromEnum c) - 48
octToChar::Char->Char->Char->Char
octToChar a b c = toEnum $ (oc a) * 32 + (oc b) * 8 + (oc c)

hc c | c >= '0' && c <= '9' = (fromEnum c) - 48
     | c >= 'A' && c <= 'F' = (fromEnum c) - 55
     | c >= 'a' && c <= 'f' = (fromEnum c) - 87
hexToChar::Char->Char->Char
hexToChar a b = toEnum $ (hc a) * 16 + (hc b)

hex::Parser Char
hex = oneOf "0123456789abcdefABCDEF"
hx = do
  char 'x'
  a <- hex
  b <- hex
  return (AsIs (hexToChar a b))
  
escape::Parser Outputter
escape = do
  char '\\'
  s<-ch <|> hx <|> ot
  return s

els::Parser Outputter
els = do
   c <- anyChar
   return (AsIs c)


