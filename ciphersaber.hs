{-# LANGUAGE BangPatterns #-}
-- Copyright © 2012 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

import Control.Exception
import Control.Monad
import Data.Array.IO
import qualified Data.Bits as B
import qualified Data.ByteString.Lazy as BS
import Data.Char
import Data.Word
import System.Console.ParseArgs
import System.IO

data ArgInd = ArgEncrypt | ArgDecrypt | ArgKey | ArgReps
     deriving (Ord, Eq, Show)

argd :: [ Arg ArgInd ]
argd = [
  Arg {
     argIndex = ArgEncrypt,
     argName = Just "encrypt",
     argAbbr = Just 'e',
     argData = Nothing,
     argDesc = "Use encryption mode."
  },
  Arg {
     argIndex = ArgDecrypt,
     argName = Just "decrypt",
     argAbbr = Just 'd',
     argData = Nothing,
     argDesc = "Use decryption mode."
  },
  Arg {
     argIndex = ArgKey,
     argName = Just "key",
     argAbbr = Just 'k',
     argData = argDataOptional "keytext" ArgtypeString,
     argDesc = "Encryption or decryption key (dangerous)."
  },
  Arg {
     argIndex = ArgReps,
     argName = Just "reps",
     argAbbr = Just 'r',
     argData = argDataDefaulted "repcount" ArgtypeInt 20,
     argDesc = "Number of key sched reps (20 default, 1 for CS1)."
  } ]

type CState = IOUArray Word8 Word8

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

initKeystream :: String -> [Word8] -> Int -> IO CState
initKeystream key iv reps = do
  let keystream = concat $ replicate reps $
                  take 256 $ cycle $ map (fi . ord) key ++ iv
  state <- newListArray (0, 255) [0..255]
  mixArray keystream state (0, 0)
  where
    mixArray :: [Word8] -> CState -> (Word8, Word8) -> IO CState
    mixArray (k : ks) a (i, j0) = do
      si <- readArray a i
      let j = j0 + si + k
      sj <- readArray a j
      writeArray a i sj
      writeArray a j si
      mixArray ks a (i + 1, j)
    mixArray [] a (0, _) = return a
    mixArray _ _ _ = error "internal error: bad mix state"

readIV :: IO [Word8]
readIV = do
  ivs <- BS.hGet stdin 10
  let ivl = BS.unpack ivs
  return ivl

makeIV :: IO [Word8]
makeIV = 
  withBinaryFile "/dev/urandom" ReadMode $ \h ->
  do
    hSetBuffering h NoBuffering
    ivs <- BS.hGet h 10
    BS.hPut stdout ivs
    return $ BS.unpack ivs

{-
accumMapM :: (Functor m, Monad m) => (a -> b -> m (a, b)) -> a -> [b] -> m [b]
accumMapM _ _ [] = return []
accumMapM a acc (b : bs) = do
  (acc', b') <- a acc b
  fmap (b' :) $ accumMapM a acc' bs
-}

accumMapM_ :: Monad m => (a -> b -> m a) -> a -> [b] -> m ()
accumMapM_ _ _ [] = return ()
accumMapM_ a acc (b : bs) = do
  acc' <- a acc b
  accumMapM_ a acc' bs

type Accum = ((Word8, Word8), CState)

stepRC4 :: Accum -> Char -> IO Accum
stepRC4 ((!i0, !j0), !state) !b = do
  let !i = i0 + 1
  !si <- readArray state i
  let !j = j0 + si
  !sj <- readArray state j
  writeArray state j si
  writeArray state i sj
  !k <- readArray state (si + sj)
  putChar $ chr $ fi $ B.xor k $ fi $ ord b
  return ((i, j), state)

applyKeystream :: CState -> String -> IO ()
applyKeystream state intext = 
  accumMapM_ stepRC4 ((0, 0), state) intext

applyStreamCipher :: CState -> IO ()
applyStreamCipher state =
  getContents >>= applyKeystream state

-- http://stackoverflow.com/a/4064482
getKey :: String -> IO String
getKey prompt = do
  bracket
    (openFile "/dev/tty" ReadWriteMode)
    (\h -> hClose h)
    (\h -> do
       hPutStr h prompt
       hFlush h
       old <- hGetEcho h
       key <- bracket_
                (hSetEcho h False)
                (hSetEcho h old)
                (hGetLine h)
       hPutStrLn h ""
       return key)

main :: IO ()
main = do
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  hSetBuffering stdin $ BlockBuffering $ Just $ 64 * 1024
  hSetBuffering stdout $ BlockBuffering $ Just $ 64 * 1024
  argv <- parseArgsIO ArgsComplete argd
  let e = gotArg argv ArgEncrypt
  let d = gotArg argv ArgDecrypt
  unless ((e && not d) || (d && not e)) $
       usageError argv "Exactly one of -e or -d is required."
  k <- case gotArg argv ArgKey of
         True -> return $ getRequiredArg argv ArgKey
         False -> getKey "key:"
  let r = getRequiredArg argv ArgReps
  unless (r > 0) $
       usageError argv "Reps must be positive."
  iv <- if e then makeIV else readIV
  s <- initKeystream k iv r
  applyStreamCipher s
