{-# LANGUAGE OverloadedStrings #-}

module LogAnalysis.KnowledgeCSVParser
       ( parseKnowLedgeBase
       ) where

import           Universum

import           Data.Attoparsec.Text (Parser, char, endOfLine, takeTill, string)

import           LogAnalysis.Types (Knowledge (..))


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


-- | Parse each csv records
{-parseCSVHeader :: Parser CSVHeader
parseCSVHeader = do
    return $ iterateHeader <* endOfLine
    where
        iterateHeader :: a -> [Text]
        iterateHeader (x : xs) = do
            x' <- quotedText x
            _ <- char ','
            return $ [x' : parseCSVHeader xs]
-}
parseKnowledge :: Parser Knowledge
parseKnowledge = do
    _ <- char '"'
    issueProj <- takeTill (=='-')
    _ <- char '-'
    issueID <- takeTill (=='"') 
    _ <- char '"'
    _ <- char ','
    project <- quotedText
    _ <- char ','
    tags <- quotedText
    _ <- char ','
    summary <- quotedText
    _ <- char ','
    reporter <- quotedText
    _ <- char ','
    created <- quotedText
    _ <- char ','
    updated <- quotedText
    _ <- char ','
    resolved <- quotedText
    _ <- char ','
    priority <- quotedText
    _ <- char ','
    typeCat <- quotedText
    _ <- char ','
    state <- quotedText
    _ <- char ','
    assignee <- quotedText
    _ <- char ','
    subsystem <- quotedText
    _ <- char ','
    fixVer <- quotedText
    _ <- char ','
    affVer <- quotedText
    _ <- char ','
    tarVer <- quotedText
    _ <- char ','
    group <- quotedText
    _ <- char ','
    resolution <- quotedText
    _ <- char ','
    platform <- quotedText
    _ <- char ','
    numUserAff <- quotedText
    _ <- char ','
    zenDebugger <- quotedText
    _ <- char ','
    zenTicketStat <- quotedText
    _ <- char ','
    zenFAQNum <- quotedText
    _ <- char ','
    zenIdentText <- quotedText
    _ <- char ','
    zenProbURL <- quotedText
    _ <- char ','
    zenDebSol <- quotedText
    _ <- char ','
    zenDebStatus <- quotedText

    return $ Knowledge
        {  kErrorText = zenIdentText
        ,  kIssueID = IssueID issueProj issueID
        ,  kFAQNumber = zenFAQNum
        }

-- | Parse CSV file and create knowledgebase
parseKnowLedgeBase :: Parser [ Knowledge ]
parseKnowLedgeBase = many $ parseKnowledge <* endOfLine
