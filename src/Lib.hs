{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Lib
  ( parseMorse
  , dict
  , decodeChar
  , pMorseChar
  , decodeParagraph
  , Paragraph(..)
  )
where

import           Control.Applicative  (Alternative (some, (<|>)))
import           Data.Maybe           (catMaybes, fromMaybe)
import           Data.Text            (Text)
import           Data.Void            (Void)
import           Text.Megaparsec      (MonadParsec (eof), Parsec, sepBy)
import           Text.Megaparsec.Char (char, string)

type Parser = Parsec Void Text

type MorseChar = [Char]
newtype Paragraph = Paragraph [String] deriving (Eq, Show)

dict :: [(MorseChar, Char)]
dict =
  [ (".-", 'A')
  , ("-...", 'B')
  , ("-.-.", 'C')
  , ("-..", 'D')
  , (".", 'E')
  , ("..-.", 'F')
  , ("--.", 'G')
  , ("....", 'H')
  , ("..", 'I')
  , (".---", 'J')
  , ("-.-", 'K')
  , (".-..", 'L')
  , ("--", 'M')
  , ("-.", 'N')
  , ("---", 'O')
  , (".--.", 'P')
  , ("--.-", 'Q')
  , (".-.", 'R')
  , ("...", 'S')
  , ("-", 'T')
  , ("..-", 'U')
  , ("...-", 'V')
  , (".--", 'W')
  , ("-..-", 'X')
  , ("-.--", 'Y')
  , ("--..", 'Z')
  , (".......", ' ')
  , (".----", '1')
  , ("..---", '2')
  , ("...--", '3')
  , ("....-", '4')
  , (".....", '5')
  , ("-....", '6')
  , ("--...", '7')
  , ("---..", '8')
  , ("----.", '9')
  , ("-----", '0')
  ]

pMorseChar :: Parser (Maybe MorseChar)
pMorseChar = do
  candidate <- some (char '.' <|> char '-')
  if candidate `elem` map fst dict
    then return (Just candidate)
    else return Nothing

pParagraph :: Parser Paragraph
pParagraph = do
  r <- pMorseChar `sepBy` string " "
  return (Paragraph (catMaybes r))

decodeChar :: MorseChar -> Char
decodeChar x = fromMaybe ' ' $ lookup x dict

decodeParagraph :: [MorseChar] -> [Char]
decodeParagraph = map decodeChar

parseMorse :: Parser Paragraph
parseMorse = pParagraph <* eof
