{-# LANGUAGE OverloadedStrings #-}

module LogAnalysis.KnowledgeCSVParser
       ( parseKnowLedgeBase
       ) where

import           Universum

import           Data.Attoparsec.Text (Parser, char, endOfLine, string, takeTill)

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
        (string "TLSCert"              >> return IOHKS_37)
    <|> (string "WinReg"               >> return IOHKS_65)
    <|> (string "openLock"             >> return IOHKS_39)
    <|> (string "WalletNotSync"        >> return IOHKS_31)
    <|> (string "PermCreateFile"       >> return IOHKS_30)
    <|> (string "ConnectLoadHeaders"   >> return IOHKS_10)
    <|> (string "PermDenied"           >> return IOHKS_7)
    <|> (string "DBCorruptIO"          >> return IOHKS_47)
    <|> (string "BlockDataCorrupt"     >> return IOHKS_29)
    <|> (string "CannotConnectAfter"   >> return IOHKS_78)
    <|> (string "CannotConnect"        >> return IOHKS_79)
    <|> (string "FileNotFound"         >> return IOHKS_35)
    <|> (string "DBError"              >> return IOHKS_41)
    <|> (string "ConnectionRefused"    >> return IOHKS_43)
    <|> (string "ShortStorage"         >> return IOHKS_45)
    <|> (string "StaleLockFile"        >> return IOHKS_48)
    <|> (string "ResourceVanished"     >> return IOHKS_12)
    <|> (string "TimeSync"             >> return IOHKS_8)
    <|> (string "NetworkError"         >> return IOHKS_36)
    <|> (string "Unknown"              >> return Unknown)
    <|> (string "Error"                >> return Error)

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

