{-# LANGUAGE OverloadedStrings #-}

module LogAnalysis.KnowledgeCSVParser
       ( parseKnowLedgeBase
       ) where

import           Universum

import           Data.Attoparsec.Text.Lazy as ALT
import qualified Data.Text.Lazy as LT

import           LogAnalysis.Types (ErrorCode (..), Knowledge (..))

-- | Take any string that is inside quotes
insideQuotes :: Parser LText
insideQuotes =
    LT.append <$> (LT.fromStrict <$> ALT.takeWhile (/= '"'))
              <*> (LT.concat <$> many (LT.cons <$> dquotes <*> insideQuotes))
    <?> "inside of double quotes"
  where
    dquotes = string "\"\"" >> return '"'
             <?> "paired double quotes"

-- | Parse quoted field
quotedField :: Parser LText
quotedField =
    char '"' *> insideQuotes <* char '"'
    <?> "quoted field"

--- | Parse ErrorCode
parseErrorCode :: Parser ErrorCode
parseErrorCode =
        (string "ShortStorage"      >> return ShortStorage)
    <|> (string "UserNameError"     >> return UserNameError)
    <|> (string "TimeSync"          >> return TimeSync)
    <|> (string "FileNotFound"      >> return FileNotFound)
    <|> (string "StaleLockFile"     >> return StaleLockFile)
    <|> (string "DBPath"            >> return DBPath)
    <|> (string "CannotGetDBSize"   >> return CannotGetDBSize)
    <|> (string "DBError"           >> return DBError)
    <|> (string "BalanceError"      >> return BalanceError)
    <|> (string "NetworkError"      >> return NetworkError)
    <|> (string "ConnectionRefused" >> return ConnectionRefused)
    <|> (string "ResourceVanished"  >> return ResourceVanished)
    <|> (string "Unknown"           >> return Unknown)
    <|> (string "Error"             >> return Error)

-- | Parse each csv records
parseKnowledge :: Parser Knowledge -- not really clean code..
parseKnowledge = do
    e <- quotedField
    _ <- char ','
    _ <- char '"'
    c <- parseErrorCode
    _ <- char '"'
    _ <- char ','
    p <- quotedField
    _ <- char ','
    s <- quotedField
    return $ Knowledge e c p s

-- | Parse CSV file and create knowledgebase
parseKnowLedgeBase :: Parser [ Knowledge ]
parseKnowLedgeBase = many $ parseKnowledge <* endOfLine
