import Control.Monad.ST
import Data.Array.ST
--import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.Word
import Foreign
import System.Environment
import System.IO

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
    fp <- mallocForeignPtrBytes 10
    withForeignPtr fp $ \buf ->
      do
        10 <- hGetBuf h buf 10
        mapM (peekElemOff buf) [0..9]

main :: IO ()
main = do
  (k : _) <- getArgs
  iv <- getIV
  let v = runST $ 
           do 
             s <- initKeystream k iv
             sn <- readArray s 255
             return sn
  putStrLn $ show v
