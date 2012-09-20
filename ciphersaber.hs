import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import qualified Data.ByteString.Lazy as BS
import Data.Char
import Data.Word
import System.Console.ParseArgs
import System.IO

data ArgInd = ArgEncrypt | ArgDecrypt | ArgKey
     deriving (Ord, Eq, Show)

argd :: [ Arg ArgInd ]
argd = [
  Arg {
     argIndex = ArgEncrypt,
     argName = Just "encrypt",
     argAbbr = Just 'e',
     argData = Nothing,
     argDesc = "Use decryption mode."
  },
  Arg {
     argIndex = ArgDecrypt,
     argName = Just "decrypt",
     argAbbr = Just 'd',
     argData = Nothing,
     argDesc = "Use encryption mode."
  },
  Arg {
     argIndex = ArgKey,
     argName = Nothing,
     argAbbr = Nothing,
     argData = argDataRequired "key" ArgtypeString,
     argDesc = "Encryption or decryption key."
  } ]

type CState s = STUArray s Word8 Word8

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

initKeystream :: String -> [Word8] -> ST s (CState s)
initKeystream key iv = do
  let kl = cycle $ map (fi . ord) key ++ iv
  state <- newListArray (0, 255) [0..255]
  mixArray kl state (0, 0)
  where
    mixArray :: [Word8] -> CState s -> (Int, Int) -> ST s (CState s)
    mixArray k a (i, j) | i < 256 = do
      si <- readArray a (fi i)
      let j' = j + fi si + fi (k !! i)
      sj' <- readArray a (fi j')
      writeArray a (fi i) sj'
      writeArray a (fi j) si
      mixArray k a (i + 1, j')
    mixArray _ a _ = return a

getIV :: IO [Word8]
getIV = 
  withBinaryFile "/dev/urandom" ReadMode $ \h ->
  do
    hSetBuffering h NoBuffering
    ivs <- BS.hGet h 10
    return $ BS.unpack ivs

main :: IO ()
main = do
  argv <- parseArgsIO ArgsComplete argd
  let k = getRequiredArg argv ArgKey
  let e = gotArg argv ArgEncrypt
  let d = gotArg argv ArgDecrypt
  unless ((e && not d) || (d && not e)) $
    usageError argv "Exactly one of -e or -d is required."
  iv <- getIV
  let v = runST $ 
           do 
             s <- initKeystream k iv
             sn <- readArray s 255
             return sn
  putStrLn $ show v
