{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Statistics
    (filterTicketsByStatus) where

import           Universum
import           Types (Attachment (..), Comment (..), Ticket (..), TicketId, TicketInfo (..),
                        TicketList (..), TicketTag (..), ZendeskResponse (..), parseAgentId,
                        parseComments, parseTickets, renderTicketStatus)

filterTicketsByStatus :: [TicketInfo] -> Text -> [TicketInfo]
filterTicketsByStatus tickets status =  filter ((== status) . ticketStatus) tickets

