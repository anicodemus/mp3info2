module Header (
  MP3Info,
  secondsS,
  seconds,
  minutes,
  secs,
  getBitRate,
  getLayer,
  getVersion,
  getCopyright,
  getCrc,
  getMode,
  getFrequency,
  getFrequencyK,
  getEmphasis,
  getOriginality,
  getPadded,
  getInvalidFrames,
  getValidFrames,
  gmins,
  gsecs,
  readMP3Info
  ) where

       

import qualified Data.ByteString.Lazy as B
import GHC.Int
import System.IO
-- import IO
import Data.Word
import Data.Bits
import Data.Array


--Frequency Data
-- __  	 MPEG1	 MPEG2	 MPEG2.5
-- 00	 44100	 22050	 11025
-- 01	 48000	 24000	 12000
-- 10	 32000	 16000	 8000
-- 11	 reserv. reserv. reserv.
data Frequency = F Int | FReserved

instance Show Frequency where show = showF

frequencies = listArray ((0,0),(3,2)) [(F 44100), (F 22050), (F 11025),
                                       (F  48000),(F 24000), (F 12000),
                                       (F  32000), (F 16000),(F 8000),
                                       FReserved, FReserved, FReserved]
fixFreq MPG1 = 0
fixFreq MPG2 = 1
fixFreq MPG25 = 2

getFreq i v = frequencies ! (i , fixFreq v)

showF (F i) = show i
showF FReserved = "Reserved"

-- Bit Rate Data
-- Bit Rate Table
-- ____ V1,L1   V1,L2	 V1,L3	 V2,L1	 V2, L2 & L3
-- 0000	 free	 free	 free	 free	 free
-- 0001	 32	 32	 32	 32	 8
-- 0010	 64	 48	 40	 48	 16
-- 0011	 96	 56	 48	 56	 24
-- 0100	 128	 64	 56	 64	 32
-- 0101	 160	 80	 64	 80	 40
-- 0110	 192	 96	 80	 96	 48
-- 0111	 224	 112	 96	 112	 56
-- 1000	 256	 128	 112	 128	 64
-- 1001	 288	 160	 128	 144	 80
-- 1010	 320	 192	 160	 160	 96
-- 1011	 352	 224	 192	 176	 112
-- 1100	 384	 256	 224	 192	 128
-- 1101	 416	 320	 256	 224	 144
-- 1110	 448	 384	 320	 256	 160
-- 1111	 bad	 bad	 bad	 bad	 bad

data BitRate = Br Int | Free | Bad
instance Show BitRate where show = showB

rateTable = [Free, Free, Free, Free, Free,
             (Br 32),(Br 32),(Br 32), (Br 32) ,(Br 8),
             (Br 64),(Br 48),(Br 40), (Br 48) ,(Br 16),
             (Br 96),(Br 56),(Br 48), (Br 56) ,(Br 24),
             (Br 128),(Br 64),(Br 56), (Br 64) ,(Br 32),
             (Br 160),(Br 80),(Br 64), (Br 80) ,(Br 40),
             (Br 192),(Br 96),(Br 80), (Br 96) ,(Br 48),
             (Br 224),(Br 112),(Br 96), (Br 112) ,(Br 56),
             (Br 256),(Br 128),(Br 112), (Br 128) ,(Br 64),
             (Br 288),(Br 160),(Br 128), (Br 144) ,(Br 80),
             (Br 320),(Br 192),(Br 160), (Br 160) ,(Br 96),
             (Br 352),(Br 224),(Br 192), (Br 176) ,(Br 112),
             (Br 384),(Br 256),(Br 224), (Br 192) ,(Br 128),
             (Br 416),(Br 320),(Br 256), (Br 224) ,(Br 144),
             (Br 448),(Br 384),(Br 320), (Br 256) ,(Br 160),
             Bad, Bad, Bad, Bad, Bad]

bitRates = listArray ((0,0),(15,4)) rateTable

fixRate MPG1 Layer1 = 0
fixRate MPG1 Layer2 = 1
fixRate MPG1 Layer3 = 2
fixRate MPG2 Layer1 = 3
fixRate MPG2 Layer2 = 4
fixRate MPG2 Layer3 = 4
fixRate _ _ = error "Unsupport version"

getRate r v l = bitRates ! (r,(fixRate v l))

showB (Br i) = show i
showB Bad = "bad"
showB Free = "free"

data Mode = Stereo | JointStereo | DualChannel | SingleChannel
          deriving Enum

instance Show Mode where show = showM

showM Stereo = "stereo"
showM JointStereo = "joint stereo"
showM DualChannel = "dual channel"
showM SingleChannel = "single channel"

data Emphasis = None | F5015ms | Empty |  CCITTj17
              deriving Enum
instance Show Emphasis where show = showE

showE None = "none"
showE F5015ms = "f5015ms"
showE Empty = "empty"
showE CCITTj17 = "ccittj17"

-- MP3 Header definition
data Version = MPG25 | VReserved | MPG2 | MPG1
             deriving Enum

instance Show Version where show = showV

showV MPG25 = "2.5"
showV VReserved = "reserved"
showV MPG2 = "2.0"
showV MPG1 = "1.0"

vToD MPG25 = 2.5
vToD VReserved = 0
vToD MPG2 = 2.0
vToD MPG1 = 1.0

data Layer = LReserved | Layer3 | Layer2 | Layer1
           deriving Enum

instance Show Layer where show = showL

showL LReserved = "reserved"
showL Layer3 = "III"
showL Layer2 = "II"
showL Layer1 = "I"

data MP3Header = Valid {
  version       :: Version,
  layer         :: Layer,
  protected  	:: Bool,
  bitrate	:: BitRate,
  frequency	:: Frequency,
  padded	:: Bool,
  priv		:: Bool,
  mode		:: Mode,
  copy		:: Bool,
  original	:: Bool,
  emphasis	:: Emphasis
} | Invalid deriving Show

createHeader MPG25 _ _ _ _ _ _ _ _ _ _ = Invalid
createHeader VReserved _ _ _ _ _ _ _ _ _ _ = Invalid
createHeader _ LReserved _ _ _ _ _ _ _ _ _ = Invalid
createHeader _ _ _ Bad _ _ _ _ _ _ _ = Invalid
createHeader _ _ _ Free  _ _ _ _ _ _ _ = Invalid
createHeader _ _ _ _ FReserved _ _ _ _ _ _ = Invalid
createHeader ve la pr ra fr pa pi mo co og em =
  (Valid ve la pr ra fr pa pi mo co og em)

-- Logic

getBits::Word8->Int->Int->Int
getBits w a b | a > 7 || a < 0 || b > 7 || b < 0 = error "only 8 bits in a word"
              | a <= b = fromIntegral ans
              | otherwise = getBits w b a
  where ans = shiftR (w .&. (2^(b+1)-1)) a

getBit w a = getBits w a a
isSet w b = (getBit w b) == 1

strToHeader::Word8->Word8->Word8->MP3Header
strToHeader a b c | 7 == (getBits a 7 5) =  bytesToH a b c
                  | otherwise = Invalid

bytesToH a b c = createHeader ve la pt br fr pd pr mo cp og em
  where ve = toEnum (getBits a 4 3)
        la = toEnum (getBits a 2 1)
        pt = not (isSet a 0)
        br = getRate (getBits b 7 4) ve la
        fr = getFreq (getBits b 3 2) ve
        pd = isSet b 1
        pr = isSet b 0
        mo = toEnum (getBits c 7 6)
        cp = isSet c 3
        og = isSet c 2
        em = toEnum (getBits c 1 0)

readHeader s = let a = B.index s 0
                   b = B.index s 1
                   c = B.index s 2
               in (strToHeader a b c)

findNextHeader bstr = case (B.head bstr) of
  -- 255 is the syncWord,
  -- variables cannot be as deconstructing values
  -- thus the literal 255 here
  255 -> readHeader (B.take 3 (B.tail bstr))
  _ -> Invalid


frameLength::MP3Header->Int64
frameLength Invalid = 1
frameLength (Valid  _ Layer1 pr (Br rate) (F freq) pa _ _ _ _ _) = fr1
  where fr1 = fromIntegral (((12000 * rate `div` freq) + pad)*4)
        pad = if pa then 1 else 0

frameLength (Valid  _ _ pr (Br rate) (F freq) pa _ _ _ _ _) =  fr2
  where fr2 = fromIntegral (pad + ((144000 * rate) `div` freq))
        pad = if pa then 1 else 0

getHeaders bstr | B.null bstr = []
                | otherwise = let header = findNextHeader bstr
                                  i = (frameLength header)
                                  next = B.drop i bstr
                                  rest = getHeaders next
                              in case header of
                                 Invalid -> rest
                                 _ -> header:rest
                               -- header:rest

yesno True = "Yes"
yesno False = "No"

secondsPerFrame = 2.6e-2

type MP3Info = [MP3Header]

hvalid Invalid = False
hvalid _ = True

valid = filter hvalid

seconds mi = secondsPerFrame * l
  where l = fromIntegral $ length (valid mi)

secondsS  = ceiling . seconds

minutes mi = div (secondsS mi) 60

guessFrames:: MP3Info-> Integer -> Integer
guessFrames mi size = div size (f (head mi)) 
  where f = fromIntegral . frameLength
        
guessLength mi size = div ((fs `div` 38) +  (fs `div` 39)) 2
   where fs = guessFrames mi size

gmins mi size = (guessLength mi size) `div` 60
gsecs mi size = (guessLength mi size) `mod` 60

secs mi = if s > 10 then ss else '0':ss
  where s = (mod) (secondsS mi) 60
        ss = show s

getBitRate mi = if const then show x else "Variable"
  where getbr (Valid {bitrate=(Br i)}) = i
        (x:xs) = map getbr (valid mi)
        const = and (map (\y->x==y) xs)

getLayer = show . layer . head

getVersion = vToD . version . head

getCopyright = getcopyright . head
  where getcopyright = yesno . copy

getCrc = getCrc . head
  where getCrc = yesno . protected

getMode = getmode . head
  where getmode = show . mode

getFrequency = g . frequency . head
  where g (F i) = i
        g FReserved = -1

getFrequencyK = getfrequency . head
  where getfrequency  = g . frequency
        g (F i) = (i `div` 1000)
        g f = -1

getEmphasis = getEmphasis . head
  where getEmphasis = show . emphasis

getOriginality = getOriginality . head
  where getOriginality = yesno . original
        
getPadded = getpad . head
  where getpad = yesno . padded

getInvalidFrames mi = f (dropWhile (not . hvalid) mi) 0 0
  where f [] x y = (show y)
        f (h:rest) x y | hvalid h = f rest 0 (x+y)
                       | otherwise = f rest (x+1) y

getValidFrames = length . valid 

readMP3Info file n = do 
  h <- openFile file ReadMode
  s <- hSeek h AbsoluteSeek n >> B.hGetContents h
  return (getHeaders s)


