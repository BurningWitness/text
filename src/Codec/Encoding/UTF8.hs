{-# LANGUAGE ViewPatterns #-}

module Codec.Encoding.UTF8 where

import           Data.Bits
import           Data.Word
import           GHC.Base



data UTF8 = UTF8_1 Word8
          | UTF8_2 Word8 Word8
          | UTF8_3 Word8 Word8 Word8
          | UTF8_4 Word8 Word8 Word8 Word8

{-# INLINE utf8 #-}
utf8 :: Char -> UTF8
utf8 (ord -> i)
  | i < 0x80      = UTF8_1 (fromIntegral i)

  | i < 0x800     = UTF8_2 (0xC0 + fromIntegral ( i `unsafeShiftR` 6))
                           (0x80 + fromIntegral ( i .&. 0x3F))

  | i <= 0xFFFF   = UTF8_3 (0xE0 + fromIntegral ( i `unsafeShiftR` 12))
                           (0x80 + fromIntegral ((i `unsafeShiftR` 6)  .&. 0x3F))
                           (0x80 + fromIntegral ( i                    .&. 0x3F))

  | i <= 0x10FFFF = UTF8_4 (0xF0 + fromIntegral ( i `unsafeShiftR` 18))
                           (0x80 + fromIntegral ((i `unsafeShiftR` 12) .&. 0x3F))
                           (0x80 + fromIntegral ((i `unsafeShiftR` 6)  .&. 0x3F))
                           (0x80 + fromIntegral ( i                    .&. 0x3F))

  | otherwise     =
      errorWithoutStackTrace $
        "Codec.Encoding.UTF8.utf8: character out of range: " <> show i
