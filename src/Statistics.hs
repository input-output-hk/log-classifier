{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Statistics
    (filterTicketsByStatus) where

import           Universum
import           Zendesk (App, Attachment (..), Comment (..), Config (..), IOLayer (..),
                          RequestType (..), TicketId, TicketInfo (..), TicketTag (..),
                          ZendeskLayer (..), ZendeskResponse (..), asksIOLayer, asksZendeskLayer,
                          assignToPath, defaultConfig, knowledgebasePath, renderTicketStatus,
                          runApp, tokenPath)


filterTicketsByStatus :: [TicketInfo] -> Text -> [TicketInfo]
filterTicketsByStatus tickets status = filter ((== status) . ticketStatus) tickets
