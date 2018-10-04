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
        (string "TLSCert"   >> return TLSCert)
    <|> (string "WinReg"   >> return WinReg)
    <|> (string "openLock"   >> return OpenLock)
    <|> (string "WalletNotSync"   >> return WalletNotSync)
    <|> (string "PermCreateFile"   >> return PermCreateFile)
    <|> (string "ConnectLoadHeaders"   >> return ConnectLoadHeaders)
    <|> (string "PermDenied"   >> return PermDenied)
    <|> (string "DBCorruptIO"   >> return DBCorruptIO)
    <|> (string "BlockDataCorrupt"   >> return BlockDataCorrupt)
    <|> (string "CannotConnectAfter"   >> return CannotConnectAfter)
    <|> (string "CannotConnect"   >> return CannotConnect)
    <|> (string "FileNotFound"   >> return FileNotFound)
    <|> (string "DBError"   >> return DBError)
    <|> (string "ConnectionRefused"   >> return ConnectionRefused)
    <|> (string "ShortStorage"   >> return ShortStorage)
    <|> (string "StaleLockFile"   >> return StaleLockFile)
    <|> (string "ResourceVanished"   >> return ResourceVanished)
    <|> (string "TimeSync"   >> return TimeSync)
    <|> (string "NetworkError"   >> return NetworkError)
    <|> (string "Unknown"    >> return Unknown)
    <|> (string "Error"      >> return Error)

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

