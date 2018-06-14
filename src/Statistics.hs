module Statistics
    (showStatistics) where

import           Universum
import           Zendesk (App, Attachment (..), Comment (..), Config (..),
                          TicketInfo (..),
                          ZendeskLayer (..), asksZendeskLayer)
import           Common (filterTicketsByStatus,
                         filterTicketsWithAttachments, filterAnalyzedTickets,
                         sortTickets)
-----------------------------------------------------------
-- Functions
------------------------------------------------------------

-- | Show ticket statistics
showStatistics :: [TicketInfo] -> App ()
showStatistics tickets = do
    cfg <- ask
    putTextLn $ "Classifier has gathered ticket information assigned to: " <> cfgEmail cfg
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
