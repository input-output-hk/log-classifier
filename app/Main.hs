module Main where

import           Universum

import           CLI (CLI (..), getCliArgs)
import           DataSource (TicketId (..), runApp)
import           Lib (createBasicDataLayerIO, createConfig, exportZendeskDataToLocalDB,
                      processTicketSafe, processTickets)
import           Web.Server (mainServer)

-- | Main entry point.
main :: IO ()
main = mainServer

mainCLI :: IO ()
mainCLI = do

    args        <- getCliArgs

    cfg         <- createConfig
    dataLayer   <- createBasicDataLayerIO cfg

    case args of
        (ProcessTicket ticketId)    -> void $ runApp (processTicketSafe dataLayer (TicketId ticketId)) cfg
        ProcessTickets              -> void $ runApp (processTickets dataLayer) cfg
        ExportData fromTime         -> void $ runApp (exportZendeskDataToLocalDB dataLayer fromTime) cfg
        _                           -> error "Operation not supported at the moment!"

