{-# LANGUAGE RankNTypes #-}

module Codec.Decoding.UTF8
  ( UTF8 (..)
  , UTF8_2 (..)
  , Part_3_1 (..)
  , UTF8_3 (..)
  , Part_4_1 (..)
  , Part_4_2 (..)
  , UTF8_4 (..)
  , Error (..)
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

import           Codec.Decoding.UTF8.Unsafe hiding (UTF8 (..), utf8)

import           Data.Bits
import           Data.Word



-- | Invalid character sequences that may be encountered when parsing UTF-8.
data Error = -- | Byte 1 is @10xxxxxx@
             Continuation Word8

             -- | Byte 1 is @11111xxx@
           | Invalid Word8

             -- | Byte 1 is @1110__1101__@, byte 2 is @10__1__xxxxx@
           | Surrogate Word8

             -- | Byte 1 is @110__0000__x@
           | Overlong_2 Word8

             -- | Byte 1 is @1110__0000__@, byte 2 is @01__0__xxxxx@
           | Overlong_3 Word8

             -- | Byte 1 is @11110__000__@, byte 2 is @01__00__xxxx@
           | Overlong_4 Word8

             -- | Byte 1 is @110xxxxx@, byte 2 is not @10xxxxx@.
           | Incomplete_2_2 Word8 Word8

             -- | Byte 1 is @1110xxxx@, byte 2 is not @10xxxxx@.
           | Incomplete_2_3 Word8 Word8

             -- | Byte 1 is @11110xxx@, byte 2 is not @10xxxxx@.
           | Incomplete_2_4 Word8 Word8

             -- | Byte 1 is @1110xxxx@, byte 3 is not @10xxxxx@
           | Incomplete_3_3 Word8 Word8 Word8

             -- | Byte 1 is @11110xxx@, byte 3 is not @10xxxxx@
           | Incomplete_3_4 Word8 Word8 Word8

             -- | Byte 1 is @11110xxx@, byte 4 is not @10xxxxx@.
           | Incomplete_4 Word8 Word8 Word8 Word8

             -- | Byte 1 is @11110__1__xx@ and either of the @x@es is not 0.
           | Overflow_1 Word8

             -- | Byte 1 is @11110__100__@, byte 2 is @10__00__xxxx@
           | Overflow_2 Word8



data UTF8 = UTF8_1 (forall a. Conv1 a -> a)
          | Part_2 (Word8 -> UTF8_2)
          | Part_3_1 (Word8 -> Part_3_1)
          | Part_4_1 (Word8 -> Part_4_1)
          | Error_1 Error


data UTF8_2 = UTF8_2 (forall a. Conv2 a -> a)
            | Error_2 Error


data Part_3_1 = Part_3_2 (Word8 -> UTF8_3)
              | Error_3_1 Error

data UTF8_3 = UTF8_3 (forall a. Conv3 a -> a)
            | Error_3_2 Error


data Part_4_1 = Part_4_2 (Word8 -> Part_4_2)
              | Error_4_1 Error

data Part_4_2 = Part_4_3 (Word8 -> UTF8_4)
              | Error_4_2 Error

data UTF8_4 = UTF8_4 (forall a. Conv4 a -> a)
            | Error_4_3 Error



{-# INLINE utf8 #-}
utf8 :: Word8 -> UTF8
utf8 w0
  | (w0 .&. 0x80) == 0 = UTF8_1 $ \(Conv1 f) -> f w0

  | (w0 .&. 0x40) == 0 = Error_1 $ Continuation w0

  | (w0 .&. 0x20) == 0 =
      if (w0 .&. 0x1F) < 0x02
        then Error_1 $ Overlong_2 w0
        else Part_2 $ \w1 ->
               if (w1 .&. 0xC0) /= 0x80
                 then Error_2 $ Incomplete_2_2 w0 w1
                 else UTF8_2 $ \(Conv2 f) -> f w0 w1

  | (w0 .&. 0x10) == 0 =
      Part_3_1 $ \w1 ->
        if (w1 .&. 0xC0) /= 0x80
          then Error_3_1 $ Incomplete_2_3 w0 w1
          else
            if (w0 .&. 0x0F) == 0x0D && (w1 .&. 0x20) /= 0
              then Error_3_1 $ Surrogate w1
              else
                if (w0 .&. 0x0F) == 0 && (w1 .&. 0x20) == 0
                  then Error_3_1 $ Overlong_3 w1
                  else Part_3_2 $ \w2 ->
                        if (w2 .&. 0xC0) /= 0x80
                          then Error_3_2 $ Incomplete_3_3 w0 w1 w2
                          else UTF8_3 $ \(Conv3 f) -> f w0 w1 w2

  | (w0 .&. 0x08) == 0 =
      if (w0 .&. 0x07) > 0x04
        then Error_1 $ Overflow_1 w0
        else Part_4_1 $ \w1 ->
               if (w1 .&. 0xC0) /= 0x80
                 then Error_4_1 $ Incomplete_2_4 w0 w1
                 else
                   if (w0 .&. 0x07) == 0x04 && (w1 .&. 0x3F) >= 0x10
                     then Error_4_1 $ Overflow_2 w1
                     else
                       if (w0 .&. 0x07) == 0x00 && (w1 .&. 0x3F) < 0x10
                         then Error_4_1 $ Overlong_4 w1
                         else Part_4_2 $ \w2 ->
                                if (w2 .&. 0xC0) /= 0x80
                                  then Error_4_2 $ Incomplete_3_4 w0 w1 w2
                                  else Part_4_3 $ \w3 ->
                                         if (w3 .&. 0xC0) /= 0x80
                                           then Error_4_3 $ Incomplete_4 w0 w1 w2 w3
                                           else UTF8_4 $ \(Conv4 f) -> f w0 w1 w2 w3

  | otherwise         = Error_1 $ Invalid w0
