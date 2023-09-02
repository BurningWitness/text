{-# LANGUAGE RankNTypes #-}

module Codec.Decoding.UTF8.Unsafe
  ( UTF8 (..)
  , utf8

  , Conv1 (..)
  , char1

  , Conv2 (..)
  , char2

  , Conv3 (..)
  , char3

  , Conv4 (..)
  , char4
  ) where

import           Data.Bits
import           Data.Word
import           GHC.Base



newtype Conv1 a = Conv1 (Word8 -> a)

{-# INLINE char1 #-}
char1 :: Conv1 Char
char1 = Conv1 $ unsafeChr . fromIntegral

newtype Conv2 a = Conv2 (Word8 -> Word8 -> a)

{-# INLINE char2 #-}
char2 :: Conv2 Char
char2 = Conv2 $ \w0 w1 ->
          unsafeChr $ unsafeShiftL (fromIntegral w0 .&. 0x1F) 6
                    +              (fromIntegral w1 .&. 0x3F)

newtype Conv3 a = Conv3 (Word8 -> Word8 -> Word8 -> a)

{-# INLINE char3 #-}
char3 :: Conv3 Char
char3 = Conv3 $ \w0 w1 w2 ->
          unsafeChr $ unsafeShiftL (fromIntegral (w0 .&. 0x0F)) 12
                    + unsafeShiftL (fromIntegral (w1 .&. 0x3F)) 6
                    +              (fromIntegral (w2 .&. 0x3F))

newtype Conv4 a = Conv4 (Word8 -> Word8 -> Word8 -> Word8 -> a)

{-# INLINE char4 #-}
char4 :: Conv4 Char
char4 = Conv4 $ \w0 w1 w2 w3 ->
          unsafeChr $ unsafeShiftL (fromIntegral (w0 .&. 0x07)) 18
                    + unsafeShiftL (fromIntegral (w1 .&. 0x3F)) 12
                    + unsafeShiftL (fromIntegral (w2 .&. 0x3F))  6
                    +              (fromIntegral (w3 .&. 0x3F))



data UTF8 = UTF8_1 (forall a. Conv1 a -> a)
          | UTF8_2 (Word8 -> (forall a. Conv2 a -> a))
          | UTF8_3 (Word8 -> Word8 -> (forall a. Conv3 a -> a))
          | UTF8_4 (Word8 -> Word8 -> Word8 -> (forall a. Conv4 a -> a))
          | Continuation
          | Invalid

{-# INLINE utf8 #-}
utf8 :: Word8 -> UTF8
utf8 w0
  | (w0 .&. 0x80) == 0 = UTF8_1 $ \(Conv1 f) -> f w0
  | (w0 .&. 0x40) == 0 = Continuation
  | (w0 .&. 0x20) == 0 = UTF8_2 $ \w1 (Conv2 f) -> f w0 w1
  | (w0 .&. 0x10) == 0 = UTF8_3 $ \w1 w2 (Conv3 f) -> f w0 w1 w2
  | (w0 .&. 0x08) == 0 = UTF8_4 $ \w1 w2 w3 (Conv4 f) -> f w0 w1 w2 w3
  | otherwise          = Invalid
