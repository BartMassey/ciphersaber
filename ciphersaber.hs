import Control.Monad.ST
import Data.Array.ST
--import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.Word
import System.Environment

type CState s = STUArray s Word8 Word8

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

initKeystream :: String -> ST s (CState s)
initKeystream key = do
  state <- newListArray (0, 255) [0..255]
  mixArray state (0, 0)
  where
    mixArray :: CState s -> (Int, Int) -> ST s (CState s)
    mixArray a (i, j) | i < 256 = do
      si <- readArray a (fi i)
      let ch = key !! (i `mod` length key)
      let j' = (j + fi si + ord ch) `mod` 256
      sj' <- readArray a (fi j')
      writeArray a (fi i) sj'
      writeArray a (fi j) si
      mixArray a (i + 1, j')
    mixArray a _ = return a


main :: IO ()
main = do
  (k : _) <- getArgs
  let v = runST $ 
           do 
             s <- initKeystream k
             sn <- readArray s 255
             return sn
  putStrLn $ show v
