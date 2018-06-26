module Statistics
  ( showStatistics
  , filterTicketsWithAttachments
  , showTicketWithAttachments
  ) where

import DataSource
  ( App
  , Attachment(..)
  , Comment(..)
  , TicketInfo(..)
  , TicketId(..)
  , TicketStatus(..)
  , ZendeskLayer(..)
  , asksZendeskLayer
  )
import Universum

-----------------------------------------------------------
-- Functions
------------------------------------------------------------

-- | Show ticket statistics
showStatistics :: [TicketInfo] -> App [Text] 
showStatistics tickets = do
    let ticketCatCountIO = showTicketCategoryCount tickets
    ticketWithAttachIO <- showTicketWithAttachments tickets
    return $ concat [ticketWithAttachIO, ticketWithAttachIO]

-- | Show all Tickets with Attachments
showTicketWithAttachments :: [TicketInfo] -> App [Text]
showTicketWithAttachments tickets = do
    ticketsWithAttachments <- filterTicketsWithAttachments tickets
    let ticketsCountIO = ("Tickets with Attachments: " <> show (length ticketsWithAttachments) :: Text)
    ticketsWithAttachIO <- concat <$> mapM (showTicketAttachments) ticketsWithAttachments
    return $ ticketsCountIO : ticketsWithAttachIO

-- | Display total, open, and closed tickets
showTicketCategoryCount :: [TicketInfo] -> [Text]
showTicketCategoryCount tickets = do
    let headerIO  =  "--Tickets--" :: Text
    let totalIO   = "Total: " <> show (length tickets) :: Text
    let openTickets = filterTicketsByStatus tickets "open"
    let openIO    =  "Open: " <> show (length openTickets) :: Text
    let closedTickets = filterTicketsByStatus tickets "closed"
    let closedIO  =  "Closed: " <> show (length closedTickets) :: Text
    headerIO : totalIO : openIO : [closedIO]
    
-- | Show attachment info (Size - URL)
showAttachmentInfo :: Attachment -> Text
showAttachmentInfo attachment =
  ("  Attachment: " :: Text) <> (show $ aSize attachment) <> " - " <> (aURL attachment)

-- | Show attachments of a comment
showCommentAttachments :: Comment -> [Text]
showCommentAttachments comment = fmap (showAttachmentInfo) (cAttachments comment)

-- | Show attachments of a ticket
showTicketAttachments :: TicketInfo -> App [Text]
showTicketAttachments ticket = let 
        ticketNumIO = (" Ticket #" <> (show $ tiId ticket) <> " : " :: Text)
    in
          getCommentsFromTicket ticket >>= \comments -> (return $ ticketNumIO : concat (fmap showCommentAttachments comments))

-- | Filter Tickets that have a specified status
filterTicketsByStatus :: [TicketInfo] -> Text -> [TicketInfo]
filterTicketsByStatus tickets status =
    filter (\ticket -> ticketsFilter ticket status) tickets
  where
    ticketsFilter :: TicketInfo -> Text -> Bool
    ticketsFilter ticket status =
        ((== TicketStatus status) . tiStatus) ticket

getCommentsFromTicket :: TicketInfo -> App [Comment]
getCommentsFromTicket ticket = do
    getTicketComments <- asksZendeskLayer zlGetTicketComments
    getTicketComments (tiId ticket)
    
-- | Remove tickets without Attachments
filterTicketsWithAttachments :: [TicketInfo] -> App [TicketInfo]
filterTicketsWithAttachments tickets = do
    filterM ticketsFilter tickets
  where
    ticketsFilter :: TicketInfo -> App Bool
    ticketsFilter ticket =
        doesTicketHaveAttachments ticket

    commentHasAttachment :: Comment -> Bool
    commentHasAttachment comment = length (cAttachments comment) > 0

    doesTicketHaveAttachments :: TicketInfo -> App Bool
    doesTicketHaveAttachments ticket = do
      comments <- (getCommentsFromTicket ticket)
      return (any commentHasAttachment comments)
