module Exceptions
    ( ProcessTicketExceptions (..)
    , ZipFileExceptions (..)
    ) where

import Universum

import DataSource (TicketId)

data ProcessTicketExceptions
    = AttachmentNotFound TicketId
    -- ^ Could not fetch the attachment even though ticket info has the url of the attachment
    | InvalidTicketInfo TicketId
    -- ^ TicketInfo is invalid (both attachment and comment were not found)
    | TicketInfoNotFound TicketId
    -- ^ TicketInfo could not be fetched
    deriving Show

data ZipFileExceptions
    = ReadZipFileException
    -- ^ Could not read the zip file because it was corrupted
    | DecompressionException
    -- ^ Decompresson of a zip file was not sucessful
    deriving Show

instance Exception ProcessTicketExceptions
instance Exception ZipFileExceptions