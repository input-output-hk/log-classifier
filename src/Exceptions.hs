module Exceptions
    ( ProcessTicketExceptions (..)
    , ZipFileExceptions (..)
    ) where

import Universum

import DataSource (TicketId(..))

import Prelude (Show(..))

data ProcessTicketExceptions
    = AttachmentNotFound TicketId
    -- ^ Could not fetch the attachment even though ticket info has the url of the attachment
    | InvalidTicketInfo TicketId
    -- ^ TicketInfo is invalid (both attachment and comment were not found)
    | TicketInfoNotFound TicketId
    -- ^ TicketInfo could not be fetched

data ZipFileExceptions
    = ReadZipFileException
    -- ^ Could not read the zip file because it was corrupted
    | DecompressionException
    -- ^ Decompresson of a zip file was not sucessful
    deriving Show

instance Exception ProcessTicketExceptions
instance Exception ZipFileExceptions

instance Show ProcessTicketExceptions where
    show (AttachmentNotFound tid) = "Attachment was not found on ticket ID: " <> showTicketId tid
    show (InvalidTicketInfo tid)  = "Ticket information is invalid on ticket ID: " <> showTicketId tid
    show (TicketInfoNotFound tid) = "Ticket information was not found on ticket ID: " <> showTicketId tid

showTicketId :: TicketId -> String
showTicketId tid = Universum.show (getTicketId tid)