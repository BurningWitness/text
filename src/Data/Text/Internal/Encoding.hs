{-# LANGUAGE BangPatterns, CPP, GeneralizedNewtypeDeriving, MagicHash,
    UnliftedFFITypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Data.Text.Internal.Builder
-- License     : BSD-style (see LICENSE)
-- Stability   : experimental
--
-- /Warning/: this is an internal module, and does not have a stable
-- API or name. Functions in this module may not check or enforce
-- preconditions expected by public modules. Use at your own risk!
--
-- Internals of "Data.Text.Encoding".
--
-- @since 2.0.2
module Data.Text.Internal.Encoding
  ( validateChunk
  , validateChunkSlow
  , Resume (..)
  , Decoded (..)
  , decodeChunk

  , StrictBuilder()
  , strictBuilderToText
  , textToStrictBuilder
  ) where

import Codec.Decoding.UTF8
#if defined(ASSERTS)
import Control.Exception (assert)
#endif
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup(..))
#endif
import Data.Text.Encoding.Error (OnDecodeError)
import Data.Text.Internal (Text(..))
import Data.Text.Internal.StrictBuilder (StrictBuilder)
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.Text.Internal.StrictBuilder as SB
#if defined(ASSERTS)
import GHC.Stack (HasCallStack)
#endif

#ifdef SIMDUTF
import Data.Text.Internal.ByteStringCompat (withBS)
import Data.Text.Internal.Unsafe (unsafeWithForeignPtr)
import Data.Text.Unsafe (unsafeDupablePerformIO)
import Foreign.C.Types (CSize(..))
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr)
#endif

-- | Use 'StrictBuilder' to build 'Text'.
--
-- @since 2.0.2
strictBuilderToText :: StrictBuilder -> Text
strictBuilderToText = SB.toText

-- | Copy 'Text' in a 'StrictBuilder'
--
-- @since 2.0.2
textToStrictBuilder :: Text -> StrictBuilder
textToStrictBuilder = SB.fromText

#ifdef SIMDUTF
foreign import ccall unsafe "_hs_text_is_valid_utf8" c_is_valid_utf8
    :: Ptr Word8 -> CSize -> IO CInt
#endif

{-# INLINE validateChunk #-}
validateChunk :: ByteString -> Int -> Int
validateChunk bs n =
  -- B.isValidUtf8 is buggy before bytestring-0.11.5.0
#if defined(SIMDUTF) || MIN_VERSION_bytestring(0,11,5)
  let len = B.length bs
      i0 = len - 1
      i1 = i0 - 1
      i2 = i1 - 1
      i3 = i2 - 1

      nonCont u = (u .&. 0x80) == 0 || (u .&. 0x40) /= 0

  in if len - n < 4
       then n
       else
         if nonCont $ B.unsafeIndex bs i0
           then validate i0
           else
             if nonCont $ B.unsafeIndex bs i1
               then validate i1
               else
                 if nonCont $ B.unsafeIndex bs i2
                   then validate i2
                   else
                     if nonCont $ B.unsafeIndex bs i3
                       then validate i3
                       else fallback

  where
    {-# NOINLINE fallback #-}
    fallback = validateChunkSlow bs n

    validate i =
#ifdef SIMDUTF
      let advance x =
              case x of
                1 -> i
                _ -> fallback

      in advance .
           withBS (B.drop n bs) $ \fp _ ->
             unsafeDupablePerformIO $
               unsafeWithForeignPtr fp $ \ptr ->
                 c_is_valid_utf8 ptr (fromIntegral $ i - n)
#else
      if B.isValidUtf8 $ B.take i (B.drop n bs)
        then i
        else fallback
#endif
#else
  validateChunkSlow bs n
#endif



{-# INLINE validateChunkSlow #-}
-- | Input 'ByteString' must be non-empty.
validateChunkSlow :: ByteString -> Int -> Int
validateChunkSlow bs = go
  where
    {-# NOINLINE len #-}
    len = B.length bs

    go n0
      | n0 >= len = n0
      | otherwise =
          let n1 = n0 + 1
              n2 = n1 + 1
              n3 = n2 + 1
              n4 = n3 + 1

              u0 = B.unsafeIndex bs n0
              u1 = B.unsafeIndex bs n1
              u2 = B.unsafeIndex bs n2
              u3 = B.unsafeIndex bs n3

          in case utf8 u0 of
               UTF8_1 _  -> go n1

               Part_2 f0 ->
                 if n1 >= len
                   then n0
                   else case f0 u1 of
                          UTF8_2 _  -> go n2
                          Error_2 _ -> n0

               Part_3_1 f0 ->
                 if n1 >= len
                   then n0
                   else case f0 u1 of
                          Part_3_2 f1 ->
                            if n2 >= len
                              then n0
                              else case f1 u2 of
                                     UTF8_3 _    -> go n3
                                     Error_3_2 _ -> n0

                          Error_3_1 _ -> n0

               Part_4_1 f0 ->
                 if n1 >= len
                   then n0
                   else case f0 u1 of
                          Part_4_2 f1 ->
                            if n2 >= len
                              then n0
                              else case f1 u2 of
                                     Part_4_3 f2 ->
                                       if n3 >= len
                                         then n0
                                         else case f2 u3 of
                                                UTF8_4 _    -> go n4
                                                Error_4_3 _ -> n0

                                     Error_4_2 _ -> n0

                          Error_4_1 _ -> n0

               Error_1 _ -> n0



data Resume = Resume_2_2 Word8 (Word8 -> UTF8_2)
            | Resume_2_3 Word8 (Word8 -> Part_3_1)
            | Resume_2_4 Word8 (Word8 -> Part_4_1)

            | Resume_3_3 Word8 Word8 (Word8 -> UTF8_3)
            | Resume_3_4 Word8 Word8 (Word8 -> Part_4_2)

            | Resume_4 Word8 Word8 Word8 (Word8 -> UTF8_4)

data Decoded = Decoded StrictBuilder (Maybe Resume)

{-# INLINE decodeChunk #-}
-- | 'ByteString' must be non-empty.
decodeChunk
  :: (ByteString -> Int -> Int)
  -> OnDecodeError
  -> ByteString
  -> Maybe Resume
  -> Decoded
decodeChunk validate handler bs mayResume =
  case mayResume of
    Nothing     -> fast mempty 0
    Just resume ->
      case resume of
        Resume_2_2 u0 f0 -> slow_2_2 mempty 0 u0 f0
        Resume_2_3 u0 f0 -> slow_2_3 mempty 0 u0 f0
        Resume_2_4 u0 f0 -> slow_2_4 mempty 0 u0 f0

        Resume_3_3 u0 u1 f1 -> slow_3_3 mempty 0 u0 u1 f1
        Resume_3_4 u0 u1 f1 -> slow_3_4 mempty 0 u0 u1 f1

        Resume_4 u0 u1 u2 f2 -> slow_4 mempty 0 u0 u1 u2 f2
  where
    {-# NOINLINE len #-}
    len = B.length bs

    {-# INLINE recover #-}
    recover copy n _e =
      case handler "Data.Text.Encoding: Invalid UTF-8 stream" $ Just 0 of
        Just c  -> if n >= len
                     then Decoded (copy <> SB.fromChar c) Nothing
                     else slow (copy <> SB.fromChar c) n

        Nothing -> if n >= len
                     then Decoded copy Nothing
                     else slow copy n

    {-# INLINE slow_2_2 #-}
    slow_2_2 copy n1 u0 f0 =
      let n2 = n1 + 1
          u1 = B.unsafeIndex bs n1

      in case f0 u1 of
           UTF8_2 _  -> fast (copy <> SB.unsafeWrite2 u0 u1) n2
           Error_2 e -> recover copy n1 e

    {-# INLINE slow_2_3 #-}
    slow_2_3 copy n1 u0 f0 =
      let n2 = n1 + 1
          u1 = B.unsafeIndex bs n1

      in case f0 u1 of
           Part_3_2 f1 ->
             if n2 >= len
               then Decoded copy . Just $ Resume_3_3 u0 u1 f1
               else slow_3_3 copy n2 u0 u1 f1

           Error_3_1 e -> recover copy n1 e

    {-# INLINE slow_3_3 #-}
    slow_3_3 copy n2 u0 u1 f1 =
      let n3 = n2 + 1
          u2 = B.unsafeIndex bs n2

      in case f1 u2 of
           UTF8_3 _    -> fast (copy <> SB.unsafeWrite3 u0 u1 u2) n3
           Error_3_2 e -> recover copy n2 e


    {-# INLINE slow_2_4 #-}
    slow_2_4 copy n1 u0 f0 =
      let n2 = n1 + 1
          u1 = B.unsafeIndex bs n1

      in case f0 u1 of
           Part_4_2 f1 ->
             if n2 >= len
               then Decoded copy . Just $ Resume_3_4 u0 u1 f1
               else slow_3_4 copy n2 u0 u1 f1

           Error_4_1 e -> recover copy n1 e

    {-# INLINE slow_3_4 #-}
    slow_3_4 copy n2 u0 u1 f1 =
      let n3 = n2 + 1
          u2 = B.unsafeIndex bs n2

      in case f1 u2 of
           Part_4_3 f2 ->
             if n3 >= len
               then Decoded copy . Just $ Resume_4 u0 u1 u2 f2
               else slow_4 copy n3 u0 u1 u2 f2

           Error_4_2 e -> recover copy n2 e

    {-# INLINE slow_4 #-}
    slow_4 !copy n3 u0 u1 u2 f2 =
      let n4 = n3 + 1
          u3 = B.unsafeIndex bs n3

      in case f2 u3 of
           UTF8_4 _    -> fast (copy <> SB.unsafeWrite4 u0 u1 u2 u3) n4
           Error_4_3 e -> recover copy n3 e


    {-# NOINLINE fast #-}
    fast !copy n0
      | n0 >= len = Decoded copy Nothing
      | otherwise =
          let n' = validate bs n0
              copyBS = copy <> SB.unsafeFromByteString (B.unsafeDrop n0 (B.unsafeTake n' bs))
          in if n' >= len
               then Decoded copyBS Nothing
               else if n' > n0
                      then slow copyBS n'
                      else slow copy n0

    {-# NOINLINE slow #-}
    slow !copy n0 =
      let n1 = n0 + 1
          u0 = B.unsafeIndex bs n0

      in case utf8 u0 of
           UTF8_1 _  -> fast (copy <> SB.unsafeWrite1 u0) n1

           Part_2 f0 ->
             if n1 >= len
               then Decoded copy . Just $ Resume_2_2 u0 f0
               else slow_2_2 copy n1 u0 f0

           Part_3_1 f0 ->
             if n1 >= len
               then Decoded copy . Just $ Resume_2_3 u0 f0
               else slow_2_3 copy n1 u0 f0

           Part_4_1 f0 ->
             if n1 >= len
               then Decoded copy . Just $ Resume_2_4 u0 f0
               else slow_2_4 copy n1 u0 f0

           Error_1 e -> recover copy n1 e
