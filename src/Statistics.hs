{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Statistics
    (showStatistics) where

import           Universum
import           Zendesk (App, Attachment (..), Comment (..), Config (..),
                          RequestType (..), TicketInfo (..), TicketTag (..),
                          ZendeskLayer (..), asksZendeskLayer, renderTicketStatus)

------------------------------------------------------------
-- Functions
------------------------------------------------------------

-- | Show ticket statistics
showStatistics :: App ()
showStatistics = do
    cfg <- ask
    putTextLn $ "Classifier is going to gather ticket information assigned to: " <> cfgEmail cfg
    tickets <- assignedTickets
    showTicketCategoryCount tickets
    liftIO $ printTicketCountMessage tickets (cfgEmail cfg)
    showTicketWithAttachments tickets

-- | Show all Tickets with Attachments
showTicketWithAttachments :: [TicketInfo] -> App ()
showTicketWithAttachments tickets = do
    ticketsWithAttachments <- filterTicketsWithAttachments tickets
    putTextLn $ "  Tickets with Attachments: " <> show (length ticketsWithAttachments)
    mapM_ showTicketAttachments ticketsWithAttachments

-- | Display total, open, and closed tickets
showTicketCategoryCount :: [TicketInfo] -> App ()
showTicketCategoryCount tickets = do
    putTextLn $ "--Tickets--"
    -- How many tickets are there
    putTextLn $ "Total: " <> show (length tickets)
    -- How many tickets are open
    openTickets <- pure $ filterTicketsByStatus tickets "open"
    putTextLn $ "Open: " <> show (length openTickets)
    -- How many tickets are closed
    closedTickets <- pure $ filterTicketsByStatus (tickets) "closed"
    putTextLn $ "Closed: " <> show (length closedTickets)

-- | Show attachment info (Size - URL)
showAttachmentInfo :: Attachment -> App ()
showAttachmentInfo attachment = do
    putText "  Attachment: "
    (putText . show) (aSize attachment)
    putText " - "
    putTextLn $ aURL attachment

-- | Show attachments of a comment
showCommentAttachments :: Comment -> App ()
showCommentAttachments comment = do
    mapM_ showAttachmentInfo (cAttachments comment)

-- | Show attachments of a ticket
showTicketAttachments :: TicketInfo -> App ()
showTicketAttachments ticket = do
    putText "Ticket #: "
    (putTextLn . show) (ticketId ticket)
    getTicketComments <- asksZendeskLayer zlGetTicketComments
    comments <-  getTicketComments (ticketId ticket)
    mapM_ showCommentAttachments (comments)

-- | Print how many tickets are assinged, analyzed, and unanalyzed
printTicketCountMessage :: [TicketInfo] -> Text -> IO ()
printTicketCountMessage tickets email = do
    let ticketCount = length tickets
    putTextLn $ "There are currently " <> show ticketCount
        <> " tickets in the system assigned to " <> email
    let filteredTicketCount = length $ filterAnalyzedTickets tickets
    putTextLn $ show (ticketCount - filteredTicketCount)
        <> " tickets has been analyzed by the classifier."
    putTextLn $ show filteredTicketCount <> " tickets are not analyzed."
    putTextLn "Below are statistics:"
    let tagGroups = sortTickets tickets
    mapM_ (\(tag, count) -> putTextLn $ tag <> ": " <> show count) tagGroups

-- | Get assigned tickets
assignedTickets :: App [TicketInfo]
assignedTickets = do
    listTickets <- asksZendeskLayer zlListTickets
    listTickets Assigned

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
