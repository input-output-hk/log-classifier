module Statistics
    (showStatistics) where

import           Universum
import           DataSource (App, Attachment (..), Comment (..),
                             TicketInfo (..),
                             ZendeskLayer (..),
                             asksZendeskLayer
                             )
-----------------------------------------------------------
-- Functions
------------------------------------------------------------

-- | Show ticket statistics
showStatistics :: App [TicketInfo] -> App ()
showStatistics tickets = do
    tickets >>= showTicketCategoryCount
    tickets >>= showTicketWithAttachments

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
    (putTextLn . show) (tiId ticket)
    getTicketComments <- asksZendeskLayer zlGetTicketComments
    comments <-  getTicketComments (tiId ticket)
    mapM_ showCommentAttachments (comments)

filterTicketsByStatus :: [TicketInfo] -> Text -> [TicketInfo]
filterTicketsByStatus tickets status =  do
    filter ((== status) . tiStatus) tickets

-- | Remove tickets without Attachments
filterTicketsWithAttachments :: [TicketInfo] -> App [TicketInfo]
filterTicketsWithAttachments tickets = do
    let checkTicketForAttachments :: TicketInfo -> App Bool
        checkTicketForAttachments ticket = do
          getTicketComments <- asksZendeskLayer zlGetTicketComments
          comments <- getTicketComments (tiId ticket)
          commentsWithAttachments <- pure $ (filter (\comment -> length (cAttachments comment) > 0) comments)
          pure $ not (null commentsWithAttachments)
    filterM (\ticket -> checkTicketForAttachments ticket) tickets