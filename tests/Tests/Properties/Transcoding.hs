-- | Tests for encoding and decoding

{-# LANGUAGE CPP
           , DataKinds
           , KindSignatures
           , OverloadedStrings
           , ScopedTypeVariables
           , TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Tests.Properties.Transcoding
    ( testTranscoding
    ) where

import Prelude hiding (head, tail)
import Data.Bits ((.&.), shiftR)
import Data.Char (chr, ord)
import Data.Int
import Data.Proxy
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup ((<>))
#endif
import Data.String
import Data.Word (Word8)
import GHC.Base (unsafeChr)
import GHC.TypeLits (Nat, KnownNat, natVal)
import Test.QuickCheck hiding ((.&.))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.HUnit ((@?=), assertFailure, testCase)
import Tests.QuickCheckUtils
import qualified Control.Exception as Exception
import qualified Data.Bits as Bits (shiftL, shiftR)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Extra as B
import qualified Data.ByteString.Builder.Prim as BP
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.Encoding.Error as E
import qualified Data.Text.Internal.Encoding as E
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as T (Builder, fromText, toLazyText)
import qualified Data.Text.Lazy.Encoding as EL

t_ascii t    = E.decodeASCII (E.encodeUtf8 a) === a
    where a  = T.map (\c -> chr (ord c `mod` 128)) t
tl_ascii t   = EL.decodeASCII (EL.encodeUtf8 a) === a
    where a  = TL.map (\c -> chr (ord c `mod` 128)) t

t_latin1     = E.decodeLatin1 `eq` (T.pack . BC.unpack)
tl_latin1    = EL.decodeLatin1 `eq` (TL.pack . BLC.unpack)

t_utf8       = (E.decodeUtf8 . E.encodeUtf8) `eq` id
t_utf8'      = (E.decodeUtf8' . E.encodeUtf8) `eq` (id . Right)
tl_utf8      = (EL.decodeUtf8 . EL.encodeUtf8) `eq` id
tl_utf8'     = (EL.decodeUtf8' . EL.encodeUtf8) `eq` (id . Right)
t_utf16LE    = (E.decodeUtf16LE . E.encodeUtf16LE) `eq` id
tl_utf16LE   = (EL.decodeUtf16LE . EL.encodeUtf16LE) `eq` id
t_utf16BE    = (E.decodeUtf16BE . E.encodeUtf16BE) `eq` id
tl_utf16BE   = (EL.decodeUtf16BE . EL.encodeUtf16BE) `eq` id
t_utf32LE    = (E.decodeUtf32LE . E.encodeUtf32LE) `eq` id
tl_utf32LE   = (EL.decodeUtf32LE . EL.encodeUtf32LE) `eq` id
t_utf32BE    = (E.decodeUtf32BE . E.encodeUtf32BE) `eq` id
tl_utf32BE   = (EL.decodeUtf32BE . EL.encodeUtf32BE) `eq` id

runBuilder :: B.Builder -> B.ByteString
runBuilder =
  -- Use smallish buffers to exercise bufferFull case as well
  BL.toStrict . B.toLazyByteStringWith (B.safeStrategy 5 5) ""

t_encodeUtf8Builder_ toBuilder = (runBuilder . toBuilder) `eq` E.encodeUtf8

t_encodeUtf8Builder_nonZeroOffset_ toBuilder (Positive n) =
  (runBuilder . toBuilder . T.drop n) `eq` (E.encodeUtf8 . T.drop n)

t_encodeUtf8Builder = t_encodeUtf8Builder_ E.encodeUtf8Builder
t_encodeUtf8Builder_nonZeroOffset = t_encodeUtf8Builder_nonZeroOffset_ E.encodeUtf8Builder

t_encodeUtf8BuilderEscaped = t_encodeUtf8Builder_ (E.encodeUtf8BuilderEscaped (BP.liftFixedToBounded BP.word8))
t_encodeUtf8BuilderEscaped_nonZeroOffset = t_encodeUtf8Builder_nonZeroOffset_ (E.encodeUtf8BuilderEscaped (BP.liftFixedToBounded BP.word8))

t_encodeUtf8Builder_sanity t =
  (runBuilder . E.encodeUtf8Builder) t ===
    (runBuilder . E.encodeUtf8BuilderEscaped (BP.liftFixedToBounded BP.word8)) t

t_utf8_incr (Positive n) =
  (T.concat . map fst . feedChunksOf n E.streamDecodeUtf8 . E.encodeUtf8) `eq` id

feedChunksOf :: Int -> (B.ByteString -> E.Decoding) -> B.ByteString
             -> [(T.Text, B.ByteString)]
feedChunksOf n f bs
  | B.null bs  = []
  | otherwise  = let (x,y) = B.splitAt n bs
                     E.Some t b f' = f x
                 in (t,b) : feedChunksOf n f' y

t_utf8_undecoded t =
  let b = E.encodeUtf8 t
      ls = concatMap (leftover . E.encodeUtf8 . T.singleton) . T.unpack $ t
      leftover = (++ [B.empty]) . init . drop 1 . B.inits
  in (map snd . feedChunksOf 1 E.streamDecodeUtf8) b === ls

data InvalidUtf8 = InvalidUtf8
  { iu8Prefix  :: T.Text
  , iu8Invalid :: B.ByteString
  , iu8Suffix  :: T.Text
  } deriving (Eq)

instance Show InvalidUtf8 where
  show i = "InvalidUtf8 {prefix = "  ++ show (iu8Prefix i)
                   ++ ", invalid = " ++ show (iu8Invalid i)
                   ++ ", suffix = "  ++ show (iu8Suffix i)
                   ++ ", asBS = "    ++ show (toByteString i)
                   ++ ", length = "  ++ show (B.length (toByteString i))
                   ++ "}"

toByteString :: InvalidUtf8 -> B.ByteString
toByteString (InvalidUtf8 a b c) =
  E.encodeUtf8 a `B.append` b `B.append` E.encodeUtf8 c

instance Arbitrary InvalidUtf8 where
  arbitrary = oneof
    [ InvalidUtf8 <$> pure mempty <*> genInvalidUTF8 <*> pure mempty
    , InvalidUtf8 <$> pure mempty <*> genInvalidUTF8 <*> arbitrary
    , InvalidUtf8 <$> arbitrary <*> genInvalidUTF8 <*> pure mempty
    , InvalidUtf8 <$> arbitrary <*> genInvalidUTF8 <*> arbitrary
    ]
  shrink (InvalidUtf8 a b c)
    =  map (\c' -> InvalidUtf8 a b c') (shrink c)
    ++ map (\a' -> InvalidUtf8 a' b c) (shrink a)

t_utf8_err :: InvalidUtf8 -> DecodeErr -> Property
t_utf8_err bad de = forAll (Blind <$> genDecodeErr de) $ \(Blind onErr) -> ioProperty $ do
  let decoded = E.decodeUtf8With onErr (toByteString bad)
      len = T.length (E.decodeUtf8With onErr (toByteString bad))
  l <- Exception.try (Exception.evaluate len)
  pure $ case l of
    Left (err :: Exception.SomeException) -> counterexample (show err) $
      length (show err) >= 0
    Right _  -> counterexample (show (decoded, l)) $ de /= Strict

t_utf8_err' :: B.ByteString -> Bool
t_utf8_err' bs = case E.decodeUtf8' bs of
  Left err -> length (show err) >= 0
  Right t  -> T.length t >= 0

genInvalidUTF8 :: Gen B.ByteString
genInvalidUTF8 = B.pack <$> oneof [
    -- invalid leading byte of a 2-byte sequence
    (:) <$> choose (0xC0, 0xC1) <*> upTo 1 contByte
    -- invalid leading byte of a 4-byte sequence
  , (:) <$> choose (0xF5, 0xFF) <*> upTo 3 contByte
    -- 4-byte sequence greater than U+10FFFF
  , do k <- choose (0x11, 0x13)
       let w0 = 0xF0 + (k `Bits.shiftR` 2)
           w1 = 0x80 + ((k .&. 3) `Bits.shiftL` 4)
       ([w0,w1]++) <$> vectorOf 2 contByte
    -- continuation bytes without a start byte
  , listOf1 contByte
    -- short 2-byte sequence
  , (:[]) <$> choose (0xC2, 0xDF)
    -- short 3-byte sequence
  , (:) <$> choose (0xE0, 0xEF) <*> upTo 1 contByte
    -- short 4-byte sequence
  , (:) <$> choose (0xF0, 0xF4) <*> upTo 2 contByte
    -- overlong encoding
  , do k <- choose (0 :: Int, 0xFFFF)
       case k of
         _ | k < 0x80   -> elements [ord2_ k, ord3_ k, ord4_ k]
           | k < 0x7FF  -> elements [ord3_ k, ord4_ k]
           | otherwise  -> return (ord4_ k)
  ]
  where
    contByte = (0x80 +) <$> choose (0, 0x3f)
    upTo n gen = do
      k <- choose (0,n)
      vectorOf k gen
    -- Data.Text.Internal.Encoding.Utf8.ord{2,3,4} without sanity checks
    ord2_ n = map fromIntegral [(n `shiftR` 6) + 0xC0, (n .&. 0x3F) + 0x80]
    ord3_ n = map fromIntegral [(n `shiftR` 12) + 0xE0, ((n `shiftR` 6) .&. 0x3F) + 0x80, (n .&. 0x3F) + 0x80]
    ord4_ n = map fromIntegral [(n `shiftR` 18) + 0xF0, ((n `shiftR` 12) .&. 0x3F) + 0x80, ((n `shiftR` 6) .&. 0x3F) + 0x80, (n .&. 0x3F) + 0x80]

decodeLL :: BL.ByteString -> TL.Text
decodeLL = EL.decodeUtf8With E.lenientDecode

decodeL :: B.ByteString -> T.Text
decodeL = E.decodeUtf8With E.lenientDecode

-- The lenient decoding of lazy bytestrings should not depend on how they are chunked,
-- and it should behave the same as decoding of strict bytestrings.
t_decode_utf8_lenient :: Property
t_decode_utf8_lenient = forAllShrinkShow arbitrary shrink (show . BL.toChunks) $ \bs ->
    decodeLL bs === (TL.fromStrict . decodeL . B.concat . BL.toChunks) bs

-- See http://unicode.org/faq/utf_bom.html#gen8
-- A sequence such as <110xxxxx2 0xxxxxxx2> is illegal ...
-- When faced with this illegal byte sequence ... a UTF-8 conformant process
-- must treat the first byte 110xxxxx2 as an illegal termination error
-- (e.g. filter it out or replace by 0xFFFD) ...
-- ... and continue processing at the second byte 0xxxxxxx2
t_decode_with_error2 =
  E.decodeUtf8With (\_ _ -> Just 'x') (B.pack [0xC2, 97]) === "xa"
t_decode_with_error3 =
  E.decodeUtf8With (\_ _ -> Just 'x') (B.pack [0xE0, 97, 97]) === "xaa"
t_decode_with_error4 =
  E.decodeUtf8With (\_ _ -> Just 'x') (B.pack [0xF0, 97, 97, 97]) === "xaaa"

t_decode_with_error1' = do
  E.Some x1 bs1 f1 <- pure $ E.streamDecodeUtf8With (\_ _ -> Just 'x') (B.pack [0xc2])
  x1 @?= ""
  bs1 @?= B.pack [0xc2]
  E.Some x2 bs2 _ <- pure $ f1 $ B.pack [0x80, 0x80]
  x2 @?= "\x80x"
  bs2 @?= mempty
t_decode_with_error2' =
  case E.streamDecodeUtf8With (\_ _ -> Just 'x') (B.pack [0xC2, 97]) of
    E.Some x _ _ -> x @?= "xa"
t_decode_with_error3' =
  case E.streamDecodeUtf8With (\_ _ -> Just 'x') (B.pack [0xC2, 97, 97]) of
    E.Some x _ _ -> x @?= "xaa"
t_decode_with_error4' =
  case E.streamDecodeUtf8With (\_ _ -> Just 'x') (B.pack [0xC2, 97, 97, 97]) of
    E.Some x _ _ -> x @?= "xaaa"
t_decode_with_error5' = do
  ret <- Exception.try $ Exception.evaluate $ E.streamDecodeUtf8 (B.pack [0x81])
  case ret of
    Left (_ :: E.UnicodeException) -> pure ()
    Right{} -> assertFailure "Unexpected success"

t_infix_concat bs1 text bs2 =
  forAll (Blind <$> genDecodeErr Replace) $ \(Blind onErr) ->
  text `T.isInfixOf`
    E.decodeUtf8With onErr (B.concat [bs1, E.encodeUtf8 text, bs2])

t_textToStrictBuilder =
  (E.strictBuilderToText . E.textToStrictBuilder) `eq` id



anyChar :: Gen Char
anyChar = do
  w <- arbitrary :: Gen Word8
  fmap unsafeChr $
    case w .&. 0x3 of
      0 -> chooseInt (0x0,  0x7F)
      1 -> chooseInt (0x80, 0x7FF)
      2 -> do i <- chooseInt (0x800, 0xF7FF)
              pure $ if i >= 0xD800
                       then i + 0x800
                       else i
      _ -> chooseInt (0x10000, 0x10FFF)



data Built (n :: Nat) = Built B.Builder T.Builder
                        deriving Show

instance KnownNat n => Arbitrary (Built n) where
  arbitrary = do
    cs <- vectorOf (fromIntegral $ natVal (Proxy :: Proxy n)) anyChar
    pure $ Built (B.stringUtf8 cs) (fromString cs)



utf8_conformance
  :: (IsString a, IsString b, Testable prop) => (a -> b -> prop) -> [TestTree]
utf8_conformance f =
  [ testGroup "Unicode standard v6"
      [ testProperty "Maximal 1" $
          f "\x41\xC0\xAF\x41\xF4\x80\x80\x41"
            "\x41��\x41�\x41"

      , testProperty "Maximal 2" $
          f "\x41\xE0\x9F\x80\x41"
            "\x41���\x41"

      , testProperty "Table 3-8" $
          f "\x61\xF1\x80\x80\xE1\x80\xC2\x62\x80\x63\x80\xBF\x64"
            "\x61���\x62�\x63��\x64"
      ]

  , testGroup "Markus Kuhn's test"
      [ testProperty "1" $
          f "\xCE\xBA\xE1\xBD\xB9\xCF\x83\xCE\xBC\xCE\xB5"
            "κόσμε"

      , testProperty "2.1.2" $
          f "\xC2\x80"
            "\x80"

      , testProperty "2.1.3" $
          f "\xE0\xA0\x80"
            "\x800"

      , testProperty "2.1.4" $
          f "\xF0\x90\x80\x80"
            "\x10000"

      , testProperty "2.1.5" $
          f "\xF8\x88\x80\x80\x80"
            "�����"

      , testProperty "2.1.6" $
          f "\xFC\x84\x80\x80\x80\x80"
            "������"

      , testProperty "2.2.1" $
          f "\x7F"
            "\x7F"

      , testProperty "2.2.2" $
          f "\xDF\xBF"
            "\x7FF"

      , testProperty "2.2.3" $
          f "\xEF\xBF\xBF"
            "\xFFFF"

      , testProperty "2.2.4" $
          f "\xF7\xBF\xBF\xBF"
            "����"

      , testProperty "2.2.5" $
          f "\xFB\xBF\xBF\xBF\xBF"
            "�����"

      , testProperty "2.2.6" $
          f "\xFD\xBF\xBF\xBF\xBF\xBF"
            "������"

      , testProperty "2.3.1" $
          f "\xED\x9F\xBF"
            "\xD7FF"

      , testProperty "2.3.2" $
          f "\xEE\x80\x80"
            "\xE000"

      , testProperty "2.3.3" $
          f "\xEF\xBF\xBD"
            "�"

      , testProperty "2.3.4" $
          f "\xF4\x8F\xBF\xBF"
            "\x10FFFF"

      , testProperty "2.3.5" $
          f "\xF4\x90\x80\x80"
            "����"

      , testProperty "3.1.1" $
          f "\x80"
            "�"

      , testProperty "3.1.2" $
          f "\xBF"
            "�"

      , testProperty "3.1.3" $
          f "\x80\xBF"
            "��"

      , testProperty "3.1.9" $
          f "\x80\x81\x82\x83\x84\x85\x86\x87\x88\x89\x8A\x8B\x8C\x8D\x8E\x8F\x90\x91\x92\x93\x94\x95\x96\x97\x98\x99\x9A\x9B\x9C\x9D\x9E\x9F\xA0\xA1\xA2\xA3\xA4\xA5\xA6\xA7\xA8\xA9\xAA\xAB\xAC\xAD\xAE\xAF\xB0\xB1\xB2\xB3\xB4\xB5\xB6\xB7\xB8\xB9\xBA\xBB\xBC\xBD\xBE\xBF"
            "����������������������������������������������������������������"

      , testProperty "3.2.1" $
          f "\xC0\x20\xC1\x20\xC2\x20\xC3\x20\xC4\x20\xC5\x20\xC6\x20\xC7\x20\xC8\x20\xC9\x20\xCA\x20\xCB\x20\xCC\x20\xCD\x20\xCE\x20\xCF\x20\xD0\x20\xD1\x20\xD2\x20\xD3\x20\xD4\x20\xD5\x20\xD6\x20\xD7\x20\xD8\x20\xD9\x20\xDA\x20\xDB\x20\xDC\x20\xDD\x20\xDE\x20\xDF\x20"
            "� � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � "

      , testProperty "3.2.2" $
          f "\xE0\x20\xE1\x20\xE2\x20\xE3\x20\xE4\x20\xE5\x20\xE6\x20\xE7\x20\xE8\x20\xE9\x20\xEA\x20\xEB\x20\xEC\x20\xED\x20\xEE\x20\xEF\x20"
            "� � � � � � � � � � � � � � � � "

      , testProperty "3.2.3" $
          f "\xF0\x20\xF1\x20\xF2\x20\xF3\x20\xF4\x20\xF5\x20\xF6\x20\xF7\x20"
            "� � � � � � � � "

      , testProperty "3.2.3" $
          f "\xF0\x20\xF1\x20\xF2\x20\xF3\x20\xF4\x20\xF5\x20\xF6\x20\xF7\x20"
            "� � � � � � � � "

      , testProperty "3.2.4" $
          f "\xF8\x20\xF9\x20\xFA\x20\xFB\x20"
            "� � � � "

      , testProperty "3.2.4" $
          f "\xFC\x20\xFD\x20"
            "� � "

      , testProperty "3.3.1" $
          f "\xC0"
            "�"

      , testProperty "3.3.2" $
          f "\xE0\x80"
            "��"

      , testProperty "3.3.3" $
          f "\xF0\x80\x80"
            "���"

      , testProperty "3.3.4" $
          f "\xF8\x80\x80\x80"
            "����"

      , testProperty "3.3.5" $
          f "\xFC\x80\x80\x80\x80"
            "�����"

      , testProperty "3.3.6" $
          f "\xDF"
            "�"

      , testProperty "3.3.7" $
          f "\xEF\xBF"
            "�"

      , testProperty "3.3.8" $
          f "\xF7\xBF\xBF"
            "���"

      , testProperty "3.3.9" $
          f "\xFB\xBF\xBF\xBF"
            "����"

      , testProperty "3.3.10" $
          f "\xFD\xBF\xBF\xBF\xBF"
            "�����"

      , testProperty "3.5.1" $
          f "\xFE"
            "�"

      , testProperty "3.5.2" $
          f "\xFF"
            "�"

      , testProperty "3.5.3" $
          f "\xFE\xFE\xFF\xFF"
            "����"

      , testProperty "4.1.1" $
          f "\xC0\xAF"
            "��"

      , testProperty "4.1.2" $
          f "\xE0\x80\xAF"
            "���"

      , testProperty "4.1.3" $
          f "\xF0\x80\x80\xAF"
            "����"

      , testProperty "4.1.4" $
          f "\xF8\x80\x80\x80\xAF"
            "�����"

      , testProperty "4.1.5" $
          f "\xFC\x80\x80\x80\x80\xAF"
            "������"

      , testProperty "4.2.1" $
          f "\xC1\xBF"
            "��"

      , testProperty "4.2.2" $
          f "\xE0\x9F\xBF"
            "���"

      , testProperty "4.2.3" $
          f "\xF0\x8F\xBF\xBF"
            "����"

      , testProperty "4.2.4" $
          f "\xF8\x87\xBF\xBF\xBF"
            "�����"

      , testProperty "4.2.5" $
          f "\xFC\x83\xBF\xBF\xBF\xBF"
            "������"

      , testProperty "4.3.1" $
          f "\xC0\x80"
            "��"

      , testProperty "4.3.2" $
          f "\xE0\x80\x80"
            "���"

      , testProperty "4.3.3" $
          f "\xF0\x80\x80\x80"
            "����"

      , testProperty "4.3.4" $
          f "\xF8\x80\x80\x80\x80"
            "�����"

      , testProperty "4.3.5" $
          f "\xFC\x80\x80\x80\x80\x80"
            "������"

      , testProperty "5.1.1" $
          f "\xED\xA0\x80"
            "���"

      , testProperty "5.1.2" $
          f "\xED\xAD\xBF"
            "���"

      , testProperty "5.1.3" $
          f "\xED\xAE\x80"
            "���"

      , testProperty "5.1.4" $
          f "\xED\xAF\xBF"
            "���"

      , testProperty "5.1.5" $
          f "\xED\xB0\x80"
            "���"

      , testProperty "5.1.6" $
          f "\xED\xBE\x80"
            "���"

      , testProperty "5.1.7" $
          f "\xED\xBF\xBF"
            "���"

      , testProperty "5.2.1" $
          f "\xED\xA0\x80\xED\xB0\x80"
            "������"

      , testProperty "5.2.2" $
          f "\xED\xA0\x80\xED\xBF\xBF"
            "������"

      , testProperty "5.2.3" $
          f "\xED\xAD\xBF\xED\xB0\x80"
            "������"

      , testProperty "5.2.4" $
          f "\xED\xAD\xBF\xED\xBF\xBF"
            "������"

      , testProperty "5.2.5" $
          f "\xED\xAE\x80\xED\xB0\x80"
            "������"

      , testProperty "5.2.6" $
          f "\xED\xAE\x80\xED\xBF\xBF"
            "������"

      , testProperty "5.2.7" $
          f "\xED\xAF\xBF\xED\xB0\x80"
            "������"

      , testProperty "5.2.8" $
          f "\xED\xAF\xBF\xED\xBF\xBF"
            "������"

      , testProperty "5.3.1" $
          f "\xEF\xBF\xBE"
            "\xFFFE"

      , testProperty "5.3.2" $
          f "\xEF\xBF\xBF"
            "\xFFFF"

      , testProperty "5.3.3" $
          f "\xEF\xB7\x90\xEF\xB7\x91\xEF\xB7\x92\xEF\xB7\x93\xEF\xB7\x94\xEF\xB7\x95\xEF\xB7\x96\xEF\xB7\x97\xEF\xB7\x98\xEF\xB7\x99\xEF\xB7\x9A\xEF\xB7\x9B\xEF\xB7\x9C\xEF\xB7\x9D\xEF\xB7\x9E\xEF\xB7\x9F\xEF\xB7\xA0\xEF\xB7\xA1\xEF\xB7\xA2\xEF\xB7\xA3\xEF\xB7\xA4\xEF\xB7\xA5\xEF\xB7\xA6\xEF\xB7\xA7\xEF\xB7\xA8\xEF\xB7\xA9\xEF\xB7\xAA\xEF\xB7\xAB\xEF\xB7\xAC\xEF\xB7\xAD\xEF\xB7\xAE\xEF\xB7\xAF"
            "\xFDD0\xFDD1\xFDD2\xFDD3\xFDD4\xFDD5\xFDD6\xFDD7\xFDD8\xFDD9\xFDDA\xFDDB\xFDDC\xFDDD\xFDDE\xFDDF\xFDE0\xFDE1\xFDE2\xFDE3\xFDE4\xFDE5\xFDE6\xFDE7\xFDE8\xFDE9\xFDEA\xFDEB\xFDEC\xFDED\xFDEE\xFDEF"

      , testProperty "5.3.4" $
          f "\xF0\x9F\xBF\xBE\xF0\x9F\xBF\xBF\xF0\xAF\xBF\xBE\xF0\xAF\xBF\xBF\xF0\xBF\xBF\xBE\xF0\xBF\xBF\xBF\xF1\x8F\xBF\xBE\xF1\x8F\xBF\xBF\xF1\x9F\xBF\xBE\xF1\x9F\xBF\xBF\xF1\xAF\xBF\xBE\xF1\xAF\xBF\xBF\xF1\xBF\xBF\xBE\xF1\xBF\xBF\xBF\xF2\x8F\xBF\xBE\xF2\x8F\xBF\xBF\xF2\x9F\xBF\xBE\xF2\x9F\xBF\xBF\xF2\xAF\xBF\xBE\xF2\xAF\xBF\xBF\xF2\xBF\xBF\xBE\xF2\xBF\xBF\xBF\xF3\x8F\xBF\xBE\xF3\x8F\xBF\xBF\xF3\x9F\xBF\xBE\xF3\x9F\xBF\xBF\xF3\xAF\xBF\xBE\xF3\xAF\xBF\xBF\xF3\xBF\xBF\xBE\xF3\xBF\xBF\xBF\xF4\x8F\xBF\xBE\xF4\x8F\xBF\xBF"
            "\x1FFFE\x1FFFF\x2FFFE\x2FFFF\x3FFFE\x3FFFF\x4FFFE\x4FFFF\x5FFFE\x5FFFF\x6FFFE\x6FFFF\x7FFFE\x7FFFF\x8FFFE\x8FFFF\x9FFFE\x9FFFF\xAFFFE\xAFFFF\xBFFFE\xBFFFF\xCFFFE\xCFFFF\xDFFFE\xDFFFF\xEFFFE\xEFFFF\xFFFFE\xFFFFF\x10FFFE\x10FFFF"
      ]
  ]



chunk :: Int64 -> BL.ByteString -> BL.ByteString
chunk n = BL.fromChunks . go
  where
    go bs =
      if BL.length bs <= n
        then [BL.toStrict bs]
        else let ~(l, r) = BL.splitAt n bs
             in BL.toStrict l : go r



testTranscoding :: TestTree
testTranscoding =
  testGroup "transcoding" [
    testProperty "t_ascii" t_ascii,
    testProperty "tl_ascii" tl_ascii,
    testProperty "t_latin1" t_latin1,
    testProperty "tl_latin1" tl_latin1,
    testProperty "t_utf8" t_utf8,
    testProperty "t_utf8'" t_utf8',
    testProperty "t_utf8_undecoded" t_utf8_undecoded,
    testProperty "t_utf8_incr" t_utf8_incr,
    testProperty "tl_utf8" tl_utf8,
    testProperty "tl_utf8'" tl_utf8',
    testProperty "t_utf16LE" t_utf16LE,
    testProperty "tl_utf16LE" tl_utf16LE,
    testProperty "t_utf16BE" t_utf16BE,
    testProperty "tl_utf16BE" tl_utf16BE,
    testProperty "t_utf32LE" t_utf32LE,
    testProperty "tl_utf32LE" tl_utf32LE,
    testProperty "t_utf32BE" t_utf32BE,
    testProperty "tl_utf32BE" tl_utf32BE,
    testGroup "builder" [
      testProperty "t_encodeUtf8Builder" t_encodeUtf8Builder,
      testProperty "t_encodeUtf8Builder_nonZeroOffset" t_encodeUtf8Builder_nonZeroOffset,
      testProperty "t_encodeUtf8BuilderEscaped" t_encodeUtf8BuilderEscaped,
      testProperty "t_encodeUtf8BuilderEscaped_nonZeroOffset" t_encodeUtf8BuilderEscaped_nonZeroOffset,
      testProperty "t_encodeUtf8Builder_sanity" t_encodeUtf8Builder_sanity
    ],
    testGroup "errors" [
      testProperty "t_utf8_err" t_utf8_err,
      testProperty "t_utf8_err'" t_utf8_err'
    ],
    testGroup "error recovery" [
      testProperty "t_decode_utf8_lenient" t_decode_utf8_lenient,
      testProperty "t_decode_with_error2" t_decode_with_error2,
      testProperty "t_decode_with_error3" t_decode_with_error3,
      testProperty "t_decode_with_error4" t_decode_with_error4,
      testCase "t_decode_with_error1'" t_decode_with_error1',
      testCase "t_decode_with_error2'" t_decode_with_error2',
      testCase "t_decode_with_error3'" t_decode_with_error3',
      testCase "t_decode_with_error4'" t_decode_with_error4',
      testCase "t_decode_with_error5'" t_decode_with_error5',
      testProperty "t_infix_concat" t_infix_concat
    ],

    testGroup "validation"
      [ testProperty "fast" $ \ (Built @256 bl _tl) ->
          E.validateChunk (BL.toStrict $ B.toLazyByteString bl) 0
            == fromIntegral (BL.length $ B.toLazyByteString bl)

      , testProperty "slow" $ \ (Built @256 bl _tl) ->
          E.validateChunkSlow (BL.toStrict $ B.toLazyByteString bl) 0
            == fromIntegral (BL.length $ B.toLazyByteString bl)
      ],

    testGroup "utf8"
      [ testGroup "strict" $
          utf8_conformance $ \a b ~(Built @16 bl tl, Built @16 br tr) -> do
            E.decodeUtf8With E.lenientDecode
                        (BL.toStrict . B.toLazyByteString $ bl <> B.string8 a <> br)
              === TL.toStrict (T.toLazyText $ tl <> T.fromText b <> tr)

      , testGroup "lazy" $
          utf8_conformance $ \a b ~(Built @16 bl tl, Built @16 br tr) -> do
            EL.decodeUtf8With E.lenientDecode
                         (chunk 16 . B.toLazyByteString $ bl <> B.string8 a <> br)
              === T.toLazyText (tl <> b <> tr)
      ],

    testGroup "strictBuilder" [
      testProperty "textToStrictBuilder" t_textToStrictBuilder
    ]
  ]
