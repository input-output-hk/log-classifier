{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Statistics
    ( statsExample ) where

import           Universum
import           Types (Attachment (..), Comment (..), Ticket (..), TicketId, TicketInfo (..),
                        TicketList (..), TicketTag (..), ZendeskResponse (..), parseAgentId,
                        parseComments, parseTickets, renderTicketStatus)

-- | Filepath to token file
tokenPath :: FilePath
tokenPath = "./tmp-secrets/token"

-- | Filepath to assign_to file
assignToPath :: FilePath
assignToPath = "./tmp-secrets/assign_to"

statsExample :: IO ()
statsExample = do
  token <- (readFile tokenPath)  -- Zendesk token
  assignFile <- readFile assignToPath  -- Select assignee
  putTextLn "end of stats"
