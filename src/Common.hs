module Common
  (filterTicketsByStatus, filterTicketsWithAttachments, filterAnalyzedTickets, sortTickets) where

import           Universum
import           Zendesk (App, Comment (..),  TicketInfo (..), TicketTag (..),
                          ZendeskLayer (..), asksZendeskLayer, renderTicketStatus)

filterTicketsByStatus :: [TicketInfo] -> Text -> [TicketInfo]
filterTicketsByStatus tickets status =  do
    filter ((== status) . ticketStatus) tickets

-- | Remove tickets without Attachments
filterTicketsWithAttachments :: [TicketInfo] -> App [TicketInfo]
filterTicketsWithAttachments tickets = do
    let checkTicketForAttachments :: TicketInfo -> App Bool
        checkTicketForAttachments ticket = do
          getTicketComments <- asksZendeskLayer zlGetTicketComments
          comments <- getTicketComments (ticketId ticket)
          commentsWithAttachments <- pure $ (filter (\comment -> length (cAttachments comment) > 0) comments)
          pure $ not (null commentsWithAttachments)
    filterM (\ticket -> checkTicketForAttachments ticket) tickets

-- | Filter analyzed tickets
filterAnalyzedTickets :: [TicketInfo] -> [TicketInfo]
filterAnalyzedTickets ticketsInfo =
    filter ticketsFilter ticketsInfo
  where
    ticketsFilter :: TicketInfo -> Bool
    ticketsFilter ticketInfo =
        isTicketAnalyzed ticketInfo && isTicketOpen ticketInfo && isTicketBlacklisted ticketInfo

    isTicketAnalyzed :: TicketInfo -> Bool
    isTicketAnalyzed TicketInfo{..} = (renderTicketStatus AnalyzedByScriptV1_0) `notElem` ticketTags

    isTicketOpen :: TicketInfo -> Bool
    isTicketOpen TicketInfo{..} = ticketStatus == "open" -- || ticketStatus == "new"

    -- | If we have a ticket we are having issues with...
    isTicketBlacklisted :: TicketInfo -> Bool
    isTicketBlacklisted TicketInfo{..} = ticketId `notElem` [9377,10815]

-- | Sort the ticket so we can see the statistics
sortTickets :: [TicketInfo] -> [(Text, Int)]
sortTickets tickets =
    let extractedTags = foldr (\TicketInfo{..} acc -> ticketTags <> acc) [] tickets  -- Extract tags from tickets
        tags2Filter   = ["s3", "s2", "cannot-sync", "closed-by-merge"
                        , "web_widget", "analyzed-by-script"]
        filteredTags  = filter (`notElem` tags2Filter) extractedTags  -- Filter tags
        groupByTags :: [ Text ] -> [(Text, Int)]
        groupByTags ts = map (\l@(x:_) -> (x, length l)) (group $ sort ts)  -- Group them
    in  groupByTags filteredTags
