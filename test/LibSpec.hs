{-# LANGUAGE OverloadedStrings #-}
module LibSpec (spec) where

import           Lib
import           Test.Hspec
import           Test.Hspec.Megaparsec (shouldParse)
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Text.Megaparsec       (parse)
import           Text.Megaparsec.Char  ()

type MorseChar = [Char]

spec :: Spec
spec = do
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
  prop "char -> morse -> char" $
    forAll charGen
    (\c -> (charToMorse c >>= morseToChar) == Just c)
  prop "morse -> char -> morse" $
    forAll morseCharGen
    (\c -> (morseToChar c >>= charToMorse) == Just c)
  -- prop "decodeChar QC" $
  --   property $ \a' b' c' -> do
  --     let a = charGen a'
  --     prs p s `shouldParse` Paragraph [".-.."]

allowedChars :: String
allowedChars = map snd dict

allowedMorseChars :: [MorseChar]
allowedMorseChars = map fst dict

charGen :: Gen Char
charGen = elements allowedChars

morseCharGen :: Gen MorseChar
morseCharGen = elements allowedMorseChars

-- propX :: Property
-- propX =
--   forAll morseCharGen
--     (\mc -> do
--       m <- parse pMorseChar "" mc
--       let decoded = decodeChar m
--       return $ decoded
--     ) == mc
