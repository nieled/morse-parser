{-# LANGUAGE OverloadedStrings #-}
module LibSpec (spec) where

import           Lib
import           Test.Hspec
import           Test.Hspec.Megaparsec (shouldParse)
import           Text.Megaparsec       (parse)
import           Text.Megaparsec.Char  ()

type MorseChar = [Char]

spec :: Spec
spec = do
  -- TODO: Test this with QuickCheck
  describe "parseMorse" $ do
    context "when there is no input" $
      it "returns an empty Paragraph" $
        parse parseMorse "" "" `shouldParse` Paragraph []
    context "when there is a morse character" $
      it "returns the expected character" $
        parse parseMorse "" ".-.." `shouldParse` Paragraph [".-.."]
    context "when the input is incorrect" $ do
      it "skips invalid morse chars" $
        parse parseMorse "" ".-...-.-.-------..." `shouldParse` Paragraph []
      it "skips invalid morse chars" $
        parse parseMorse "" "-- --- .-. ... ...---... ." `shouldParse` Paragraph ["--","---",".-.","...","."]
  describe "decodeChar" $ do
    context "when the input represents a valid Morse char" $
      it "returns the alphanumeric equivalence" $
        decodeChar ".-" `shouldBe` 'A'
    context "when the input represents an incorrect Morse char" $
      it "returns a whitespace" $
        decodeChar ".-.-.-.-" `shouldBe` ' '
