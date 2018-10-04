{-# LANGUAGE OverloadedStrings #-}

module LogAnalysis.KnowledgeCSVParser
       ( parseKnowLedgeBase
       ) where

import           Universum

import           Data.Attoparsec.Text (Parser, char, endOfLine, takeTill, string)

import           LogAnalysis.Types (ErrorCode (..), Knowledge (..))


{-

stack ghci
fContent <- readFile "./knowledgebase/knowledge.csv"
parseOnly parseKnowLedgeBase fContent

-}

-- | Parse quoted text field
quotedText :: Parser Text
quotedText = do
    _       <- char '"'
    result  <- takeTill (=='\"')
    _       <- char '"'
    pure result

--- | Parse ErrorCode
parseErrorCode :: Parser ErrorCode
parseErrorCode =
        (string "IOHKS-45"      >> return ShortStorage)
    <|> (string "IOHKS-8"          >> return TimeSync)
    <|> (string "IOHKS-35"      >> return FileNotFound)
    <|> (string "IOHKS-48"     >> return StaleLockFile)
    <|> (string "IOHKS-41"           >> return DBError)
    <|> (string "IOHKS-36"      >> return NetworkError)
    <|> (string "IOHKS-43" >> return ConnectionRefused)
    <|> (string "IOHKS-12"  >> return ResourceVanished)
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

