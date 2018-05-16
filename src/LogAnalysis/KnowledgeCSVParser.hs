{-# LANGUAGE OverloadedStrings #-}

module LogAnalysis.KnowledgeCSVParser
       ( parseKnowLedgeBase
       ) where

import           Universum

import           Data.Attoparsec.Text (Parser, char, endOfLine, string, takeText, (<?>))

import           LogAnalysis.Types (ErrorCode (..), Knowledge (..))

-- | Parse quoted text field
quotedText :: Parser Text
quotedText =
    char '"' *> takeText <* char '"'
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
parseKnowledge :: Parser Knowledge  -- not really clean code..
parseKnowledge = do
    e <- quotedText
    _ <- char ','
    _ <- char '"'
    c <- parseErrorCode
    _ <- char '"'
    _ <- char ','
    p <- quotedText
    _ <- char ','
    s <- quotedText
    _ <- char ','
    f <- quotedText
    return $ Knowledge
        {  kErrorText = e
        ,  kErrorCode = c
        ,  kProblem   = p
        ,  kSolution  = s
        ,  kFAQNumber = f
        }

-- | Parse CSV file and create knowledgebase
parseKnowLedgeBase :: Parser [ Knowledge ]
parseKnowLedgeBase = many $ parseKnowledge <* endOfLine
