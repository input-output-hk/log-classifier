module Statistics
  ( showStatistics
  , filterTicketsWithAttachments
  , showTicketWithAttachments
  , filterTicketsByStatus
  , showAttachmentInfo
  , showCommentAttachments
  , showTicketAttachments
  ) where

import           Universum

import           DataSource (Attachment (..), Comment (..), DataLayer (..), TicketInfo (..),
                             TicketStatus (..))

-----------------------------------------------------------
-- Functions
------------------------------------------------------------

-- | Show ticket statistics
showStatistics :: forall m. (Monad m) => DataLayer m -> [TicketInfo] -> m [Text]
showStatistics dataLayer tickets = do
    let ticketCatCountIO = showTicketCategoryCount tickets
    ticketWithAttachIO <- showTicketWithAttachments dataLayer tickets
    return $ ticketCatCountIO <> ticketWithAttachIO

-- | Show all Tickets with Attachments
showTicketWithAttachments :: forall m. (Monad m) => DataLayer m -> [TicketInfo] -> m [Text]
showTicketWithAttachments dataLayer tickets = do
    ticketsWithAttachments <- filterTicketsWithAttachments dataLayer tickets
    let ticketsCountIO  = "Tickets with Attachments: " <> show (length ticketsWithAttachments) :: Text
    ticketsWithAttachIO <- concatMapM (showTicketAttachments dataLayer) ticketsWithAttachments
    return $ ticketsCountIO : ticketsWithAttachIO

-- | Display total, open, and closed tickets
showTicketCategoryCount :: [TicketInfo] -> [Text]
showTicketCategoryCount tickets = do
    let headerIO        =  "--Tickets--" :: Text
    let totalIO         = "Total: " <> show @Text (length tickets)
    let openTickets     = filterTicketsByStatus tickets "open"
    let openIO          =  "Open: " <> show @Text (length openTickets)
    let closedTickets   = filterTicketsByStatus tickets "closed"
    let closedIO        =  "Closed: " <> show @Text (length closedTickets)
    headerIO : totalIO : openIO : [closedIO]

-- | Show attachment info (Size - URL)
showAttachmentInfo :: Attachment -> Text
showAttachmentInfo attachment =
  "  Attachment: " <> (show $ aSize attachment) <> " - " <> aURL attachment :: Text

-- | Show attachments of a comment
showCommentAttachments :: Comment -> [Text]
showCommentAttachments comment = showAttachmentInfo <$> cAttachments comment

-- | Show attachments of a ticket
showTicketAttachments :: forall m. (Monad m) => DataLayer m -> TicketInfo -> m [Text]
showTicketAttachments dataLayer ticket = do
    let getTicketComments = zlGetTicketComments dataLayer
    getTicketComments (tiId ticket) >>= \comments -> return $ ticketNumIO : concatMap showCommentAttachments comments
    where
        ticketNumIO = " Ticket #" <> (show $ tiId ticket) <> " : " :: Text

-- | Filter Tickets that have a specified status
filterTicketsByStatus :: [TicketInfo] -> Text -> [TicketInfo]
filterTicketsByStatus tickets status =
    filter (`ticketsFilter` status) tickets
  where
    ticketsFilter :: TicketInfo -> Text -> Bool
    ticketsFilter ticket statusText =
        ((== TicketStatus statusText) . tiStatus) ticket

-- | Remove tickets without Attachments
filterTicketsWithAttachments :: forall m. (Monad m) => DataLayer m -> [TicketInfo] -> m [TicketInfo]
filterTicketsWithAttachments dataLayer = filterM ticketsFilter
  where
    ticketsFilter :: TicketInfo -> m Bool
    ticketsFilter ticket = do
        let getTicketComments = zlGetTicketComments dataLayer
        any commentHasAttachment <$> getTicketComments (tiId ticket)
      where
        commentHasAttachment :: Comment -> Bool
        commentHasAttachment comment = not $ null (cAttachments comment)
