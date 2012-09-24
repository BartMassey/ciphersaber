{-# LANGUAGE BangPatterns #-}
-- Copyright © 2012 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

import Control.Monad
import Data.Array.IO
import qualified Data.Bits as B
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

type CState = IOUArray Word8 Word8

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

initKeystream :: String -> [Word8] -> IO CState
initKeystream key iv = do
  let kl = cycle $ map (fi . ord) key ++ iv
  state <- newListArray (0, 255) [0..255]
  mixArray kl state (0, 0)
  where
    mixArray :: [Word8] -> CState -> (Int, Word8) -> IO CState
    mixArray k a (i, j) | i < 256 = do
      si <- readArray a (fi i)
      let j' = j + fi si + fi (k !! i)
      sj' <- readArray a j'
      writeArray a (fi i) sj'
      writeArray a j' si
      mixArray k a (i + 1, j')
    mixArray _ a _ = return a

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
stepRC4 ((!i, !j), !state) !b = do
  let !i' = i + 1
  !si' <- readArray state i'
  let !j' = j + si'
  writeArray state j' si'
  !sj' <- readArray state j'
  writeArray state i' sj'
  !k <- readArray state (si' + sj')
  putChar $ chr $ fi k `B.xor` (fi (ord b))
  return ((i', j'), state)

applyKeystream :: CState -> String -> IO ()
applyKeystream state intext = 
  accumMapM_ stepRC4 ((0, 0), state) intext

applyStreamCipher :: CState -> IO ()
applyStreamCipher state =
  getContents >>= applyKeystream state

main :: IO ()
main = do
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  hSetBuffering stdin $ BlockBuffering $ Just $ 64 * 1024
  hSetBuffering stdout $ BlockBuffering $ Just $ 64 * 1024
  argv <- parseArgsIO ArgsComplete argd
  let k = getRequiredArg argv ArgKey
  let e = gotArg argv ArgEncrypt
  let d = gotArg argv ArgDecrypt
  unless ((e && not d) || (d && not e)) $
    usageError argv "Exactly one of -e or -d is required."
  iv <- if e then makeIV else readIV
  s <- initKeystream k iv
  applyStreamCipher s
