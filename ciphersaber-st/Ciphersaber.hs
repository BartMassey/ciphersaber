-- Copyright © 2015 Bart Massey

-- | Implementation of the "Ciphersaber" RC4 encryption
-- method. This variant drops 200 initial keystream values
-- rather than 20.
module Ciphersaber (rc4, rc4dropN, encryptDropN, decryptDropN,
                    encrypt1, decrypt1) where

import Control.Monad
import Control.Monad.ST.Safe
import Data.Array.ST
import Data.Bits
import Data.Char
import Data.Word

-- | Number of bytes of IV to use in Ciphersaber encryption/decryption
-- below. This is the standard Ciphersaber IV size.
ivLength :: Int
ivLength = 10

-- | Actually generate an RC4 keystream. This code takes a length
-- of keystream to generate, which is un-Haskellike but hard to avoid
-- given the strictness of 'STUArray'.
rc4 :: Int -> [Word8] -> [Word8]
rc4 keystreamLength key =
    runST $ do
      let keyLength = fromIntegral $ length key
      -- Create and initialize the state.
      s <- newListArray (0, 255) [0..255] :: ST s (STUArray s Word8 Word8)
      -- One step of the key schedule
      let schedStep j i = do
            si <- readArray s i
            let j' = j + si + (key !! fromIntegral (i `mod` keyLength))
            sj <- readArray s j'
            writeArray s i sj
            writeArray s j' si
            return j'
      -- Do the key scheduling.
      foldM_ schedStep 0 [0..255]
      -- Do the keystream generation.
      let keystream 0 _ _ = return []
          keystream n i j = do
            let i' = i + 1
            si <- readArray s i'
            let j' = j + si
            sj <- readArray s j'
            writeArray s i' sj
            writeArray s j' si
            sk <- readArray s (si + sj)
            ks <- keystream (n - 1) i' j'
            return $ sk : ks
      -- Get the keystream.
      keystream keystreamLength 0 0

-- | Just RC4 with the first n keystream elements
-- dropped. This resists attack.
rc4dropN :: Int -> Int -> [Word8] -> [Word8]
rc4dropN nDrop keystreamLength key =
  drop nDrop $ rc4 (keystreamLength + 200) key

-- | Ciphersaber requires using an initial value (IV) to
-- protect against keystream recovery. Given the key and IV,
-- this code will turn a plaintext 'String' message into a
-- sequence of ciphertext bytes. (Yes, the type is a little
-- weird.)
encryptDropN :: Int -> [Word8] -> [Word8] -> String -> [Word8]
encryptDropN nDrop key iv plaintext
    | length iv == ivLength =
        let plaintextBytes = map (fromIntegral . ord) plaintext
            keystream = rc4dropN nDrop (length plaintextBytes) (key ++ iv) in
        iv ++ zipWith xor keystream plaintextBytes
    | otherwise = error $ "expected IV length " ++ show ivLength

-- | Ciphersaber recovers the IV from the start of the ciphertext.
-- Given the key, this code will turn a sequence of
-- ciphertext bytes into a plaintext 'String' message. (Yes,
-- the type is a little weird.)
decryptDropN :: Int -> [Word8] -> [Word8] -> String
decryptDropN nDrop key ciphertext0 =
    let (iv, ciphertext) = splitAt ivLength ciphertext0
        keystream = rc4dropN nDrop (length ciphertext) (key ++ iv)
        plaintextBytes = zipWith xor keystream ciphertext in
    map (chr . fromIntegral) plaintextBytes

-- | Ciphersaber-1 encryption has no drops.
encrypt1 :: [Word8] -> [Word8] -> String -> [Word8]
encrypt1 = encryptDropN 0

-- | Ciphersaber-1 decryption has no drops.
decrypt1 :: [Word8] -> [Word8] -> String
decrypt1 = decryptDropN 0
