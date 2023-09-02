-- | Tests for encoding and decoding

{-# LANGUAGE CPP, OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Tests.Properties.Transcoding
    ( testTranscoding
    ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as E
import qualified Data.Text.Encoding.Error as E
import Data.Text.Lazy ()
import qualified Data.Text.Lazy.Encoding as EL
import Data.String (IsString)


conformance :: (IsString a, IsString b) => (a -> b -> Assertion) -> [TestTree]
conformance f =
  [ testGroup "Unicode standard v6"
      [ testCase "Maximal 1" $
          f "\x41\xC0\xAF\x41\xF4\x80\x80\x41"
            "\x41��\x41�\x41"

      , testCase "Maximal 2" $
          f "\x41\xE0\x9F\x80\x41"
            "\x41���\x41"

      , testCase "Table 3-8" $
          f "\x61\xF1\x80\x80\xE1\x80\xC2\x62\x80\x63\x80\xBF\x64"
            "\x61���\x62�\x63��\x64"
      ]

  , testGroup "Markus Kuhn's test"
      [ testCase "1" $
          f "\xCE\xBA\xE1\xBD\xB9\xCF\x83\xCE\xBC\xCE\xB5"
            "κόσμε"

      , testCase "2.1.2" $
          f "\xC2\x80"
            "\x80"

      , testCase "2.1.3" $
          f "\xE0\xA0\x80"
            "\x800"

      , testCase "2.1.4" $
          f "\xF0\x90\x80\x80"
            "\x10000"

      , testCase "2.1.5" $
          f "\xF8\x88\x80\x80\x80"
            "�����"

      , testCase "2.1.6" $
          f "\xFC\x84\x80\x80\x80\x80"
            "������"

      , testCase "2.2.1" $
          f "\x7F"
            "\x7F"

      , testCase "2.2.2" $
          f "\xDF\xBF"
            "\x7FF"

      , testCase "2.2.3" $
          f "\xEF\xBF\xBF"
            "\xFFFF"

      , testCase "2.2.4" $
          f "\xF7\xBF\xBF\xBF"
            "����"

      , testCase "2.2.5" $
          f "\xFB\xBF\xBF\xBF\xBF"
            "�����"

      , testCase "2.2.6" $
          f "\xFD\xBF\xBF\xBF\xBF\xBF"
            "������"

      , testCase "2.3.1" $
          f "\xED\x9F\xBF"
            "\xD7FF"

      , testCase "2.3.2" $
          f "\xEE\x80\x80"
            "\xE000"

      , testCase "2.3.3" $
          f "\xEF\xBF\xBD"
            "�"

      , testCase "2.3.4" $
          f "\xF4\x8F\xBF\xBF"
            "\x10FFFF"

      , testCase "2.3.5" $
          f "\xF4\x90\x80\x80"
            "����"

      , testCase "3.1.1" $
          f "\x80"
            "�"

      , testCase "3.1.2" $
          f "\xBF"
            "�"

      , testCase "3.1.3" $
          f "\x80\xBF"
            "��"

      , testCase "3.1.9" $
          f "\x80\x81\x82\x83\x84\x85\x86\x87\x88\x89\x8A\x8B\x8C\x8D\x8E\x8F\x90\x91\x92\x93\x94\x95\x96\x97\x98\x99\x9A\x9B\x9C\x9D\x9E\x9F\xA0\xA1\xA2\xA3\xA4\xA5\xA6\xA7\xA8\xA9\xAA\xAB\xAC\xAD\xAE\xAF\xB0\xB1\xB2\xB3\xB4\xB5\xB6\xB7\xB8\xB9\xBA\xBB\xBC\xBD\xBE\xBF"
            "����������������������������������������������������������������"

      , testCase "3.2.1" $
          f "\xC0\x20\xC1\x20\xC2\x20\xC3\x20\xC4\x20\xC5\x20\xC6\x20\xC7\x20\xC8\x20\xC9\x20\xCA\x20\xCB\x20\xCC\x20\xCD\x20\xCE\x20\xCF\x20\xD0\x20\xD1\x20\xD2\x20\xD3\x20\xD4\x20\xD5\x20\xD6\x20\xD7\x20\xD8\x20\xD9\x20\xDA\x20\xDB\x20\xDC\x20\xDD\x20\xDE\x20\xDF\x20"
            "� � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � "

      , testCase "3.2.2" $
          f "\xE0\x20\xE1\x20\xE2\x20\xE3\x20\xE4\x20\xE5\x20\xE6\x20\xE7\x20\xE8\x20\xE9\x20\xEA\x20\xEB\x20\xEC\x20\xED\x20\xEE\x20\xEF\x20"
            "� � � � � � � � � � � � � � � � "

      , testCase "3.2.3" $
          f "\xF0\x20\xF1\x20\xF2\x20\xF3\x20\xF4\x20\xF5\x20\xF6\x20\xF7\x20"
            "� � � � � � � � "

      , testCase "3.2.3" $
          f "\xF0\x20\xF1\x20\xF2\x20\xF3\x20\xF4\x20\xF5\x20\xF6\x20\xF7\x20"
            "� � � � � � � � "

      , testCase "3.2.4" $
          f "\xF8\x20\xF9\x20\xFA\x20\xFB\x20"
            "� � � � "

      , testCase "3.2.4" $
          f "\xFC\x20\xFD\x20"
            "� � "

      , testCase "3.3.1" $
          f "\xC0"
            "�"

      , testCase "3.3.2" $
          f "\xE0\x80"
            "��"

      , testCase "3.3.3" $
          f "\xF0\x80\x80"
            "���"

      , testCase "3.3.4" $
          f "\xF8\x80\x80\x80"
            "����"

      , testCase "3.3.5" $
          f "\xFC\x80\x80\x80\x80"
            "�����"

      , testCase "3.3.6" $
          f "\xDF"
            "�"

      , testCase "3.3.7" $
          f "\xEF\xBF"
            "�"

      , testCase "3.3.8" $
          f "\xF7\xBF\xBF"
            "���"

      , testCase "3.3.9" $
          f "\xFB\xBF\xBF\xBF"
            "����"

      , testCase "3.3.10" $
          f "\xFD\xBF\xBF\xBF\xBF"
            "�����"

      , testCase "3.5.1" $
          f "\xFE"
            "�"

      , testCase "3.5.2" $
          f "\xFF"
            "�"

      , testCase "3.5.3" $
          f "\xFE\xFE\xFF\xFF"
            "����"

      , testCase "4.1.1" $
          f "\xC0\xAF"
            "��"

      , testCase "4.1.2" $
          f "\xE0\x80\xAF"
            "���"

      , testCase "4.1.3" $
          f "\xF0\x80\x80\xAF"
            "����"

      , testCase "4.1.4" $
          f "\xF8\x80\x80\x80\xAF"
            "�����"

      , testCase "4.1.5" $
          f "\xFC\x80\x80\x80\x80\xAF"
            "������"

      , testCase "4.2.1" $
          f "\xC1\xBF"
            "��"

      , testCase "4.2.2" $
          f "\xE0\x9F\xBF"
            "���"

      , testCase "4.2.3" $
          f "\xF0\x8F\xBF\xBF"
            "����"

      , testCase "4.2.4" $
          f "\xF8\x87\xBF\xBF\xBF"
            "�����"

      , testCase "4.2.5" $
          f "\xFC\x83\xBF\xBF\xBF\xBF"
            "������"

      , testCase "4.3.1" $
          f "\xC0\x80"
            "��"

      , testCase "4.3.2" $
          f "\xE0\x80\x80"
            "���"

      , testCase "4.3.3" $
          f "\xF0\x80\x80\x80"
            "����"

      , testCase "4.3.4" $
          f "\xF8\x80\x80\x80\x80"
            "�����"

      , testCase "4.3.5" $
          f "\xFC\x80\x80\x80\x80\x80"
            "������"

      , testCase "5.1.1" $
          f "\xED\xA0\x80"
            "���"

      , testCase "5.1.2" $
          f "\xED\xAD\xBF"
            "���"

      , testCase "5.1.3" $
          f "\xED\xAE\x80"
            "���"

      , testCase "5.1.4" $
          f "\xED\xAF\xBF"
            "���"

      , testCase "5.1.5" $
          f "\xED\xB0\x80"
            "���"

      , testCase "5.1.6" $
          f "\xED\xBE\x80"
            "���"

      , testCase "5.1.7" $
          f "\xED\xBF\xBF"
            "���"

      , testCase "5.2.1" $
          f "\xED\xA0\x80\xED\xB0\x80"
            "������"

      , testCase "5.2.2" $
          f "\xED\xA0\x80\xED\xBF\xBF"
            "������"

      , testCase "5.2.3" $
          f "\xED\xAD\xBF\xED\xB0\x80"
            "������"

      , testCase "5.2.4" $
          f "\xED\xAD\xBF\xED\xBF\xBF"
            "������"

      , testCase "5.2.5" $
          f "\xED\xAE\x80\xED\xB0\x80"
            "������"

      , testCase "5.2.6" $
          f "\xED\xAE\x80\xED\xBF\xBF"
            "������"

      , testCase "5.2.7" $
          f "\xED\xAF\xBF\xED\xB0\x80"
            "������"

      , testCase "5.2.8" $
          f "\xED\xAF\xBF\xED\xBF\xBF"
            "������"

      , testCase "5.3.1" $
          f "\xEF\xBF\xBE"
            "\xFFFE"

      , testCase "5.3.2" $
          f "\xEF\xBF\xBF"
            "\xFFFF"

      , testCase "5.3.3" $
          f "\xEF\xB7\x90\xEF\xB7\x91\xEF\xB7\x92\xEF\xB7\x93\xEF\xB7\x94\xEF\xB7\x95\xEF\xB7\x96\xEF\xB7\x97\xEF\xB7\x98\xEF\xB7\x99\xEF\xB7\x9A\xEF\xB7\x9B\xEF\xB7\x9C\xEF\xB7\x9D\xEF\xB7\x9E\xEF\xB7\x9F\xEF\xB7\xA0\xEF\xB7\xA1\xEF\xB7\xA2\xEF\xB7\xA3\xEF\xB7\xA4\xEF\xB7\xA5\xEF\xB7\xA6\xEF\xB7\xA7\xEF\xB7\xA8\xEF\xB7\xA9\xEF\xB7\xAA\xEF\xB7\xAB\xEF\xB7\xAC\xEF\xB7\xAD\xEF\xB7\xAE\xEF\xB7\xAF"
            "\xFDD0\xFDD1\xFDD2\xFDD3\xFDD4\xFDD5\xFDD6\xFDD7\xFDD8\xFDD9\xFDDA\xFDDB\xFDDC\xFDDD\xFDDE\xFDDF\xFDE0\xFDE1\xFDE2\xFDE3\xFDE4\xFDE5\xFDE6\xFDE7\xFDE8\xFDE9\xFDEA\xFDEB\xFDEC\xFDED\xFDEE\xFDEF"

      , testCase "5.3.4" $
          f "\xF0\x9F\xBF\xBE\xF0\x9F\xBF\xBF\xF0\xAF\xBF\xBE\xF0\xAF\xBF\xBF\xF0\xBF\xBF\xBE\xF0\xBF\xBF\xBF\xF1\x8F\xBF\xBE\xF1\x8F\xBF\xBF\xF1\x9F\xBF\xBE\xF1\x9F\xBF\xBF\xF1\xAF\xBF\xBE\xF1\xAF\xBF\xBF\xF1\xBF\xBF\xBE\xF1\xBF\xBF\xBF\xF2\x8F\xBF\xBE\xF2\x8F\xBF\xBF\xF2\x9F\xBF\xBE\xF2\x9F\xBF\xBF\xF2\xAF\xBF\xBE\xF2\xAF\xBF\xBF\xF2\xBF\xBF\xBE\xF2\xBF\xBF\xBF\xF3\x8F\xBF\xBE\xF3\x8F\xBF\xBF\xF3\x9F\xBF\xBE\xF3\x9F\xBF\xBF\xF3\xAF\xBF\xBE\xF3\xAF\xBF\xBF\xF3\xBF\xBF\xBE\xF3\xBF\xBF\xBF\xF4\x8F\xBF\xBE\xF4\x8F\xBF\xBF"
            "\x1FFFE\x1FFFF\x2FFFE\x2FFFF\x3FFFE\x3FFFF\x4FFFE\x4FFFF\x5FFFE\x5FFFF\x6FFFE\x6FFFF\x7FFFE\x7FFFF\x8FFFE\x8FFFF\x9FFFE\x9FFFF\xAFFFE\xAFFFF\xBFFFE\xBFFFF\xCFFFE\xCFFFF\xDFFFE\xDFFFF\xEFFFE\xEFFFF\xFFFFE\xFFFFF\x10FFFE\x10FFFF"
      ]
  ]



chunk5 :: BL.ByteString -> BL.ByteString
chunk5 = BL.fromChunks . go
  where
    go bs =
      if BL.length bs <= 5
        then [BL.toStrict bs]
        else BL.toStrict (BL.take 5 bs) : go (BL.drop 5 bs)



testTranscoding :: TestTree
testTranscoding =
  testGroup "transcoding"
    [ testGroup "Strict" $
        conformance $ \a b -> do
          E.decodeUtf8With E.lenientDecode a @?= b

    , testGroup "Lazy" $
        conformance $ \a b -> do
          EL.decodeUtf8With E.lenientDecode (chunk5 a) @?= b
    ]
