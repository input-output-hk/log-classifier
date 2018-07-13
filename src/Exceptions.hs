module Exceptions
    ( TicketInfoExceptions (..)
    , ZipFileExceptions (..)
    ) where

import Universum

import DataSource (TicketId)

data TicketInfoExceptions
    = AttachmentNotFound TicketId
    -- ^ Could not fetch the attachment
    | InvalidTicketInfoException TicketId
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

instance Exception TicketInfoExceptions
instance Exception ZipFileExceptions