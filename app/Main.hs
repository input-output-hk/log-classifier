module Main where

import           Universum

import           CLI (CLI (..), getCliArgs)
import           DataSource (TicketId (..), runApp)
import           Lib (createBasicDataLayerIO, createConfig,
                      exportZendeskDataToLocalDB, fetchAgents, fetchAndShowTickets,
                      fetchAndShowTicketsFrom, fetchTickets, inspectLocalZipAttachment,
                      processTicketSafe, processTickets, processTicketsFromTime, showStatistics)
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
        FetchAgents                     -> void $ runApp (fetchAgents dataLayer) cfg
        FetchTickets                    -> runApp (fetchAndShowTickets dataLayer) cfg
        FetchTicketsFromTime fromTime   -> runApp (fetchAndShowTicketsFrom dataLayer fromTime) cfg
        (ProcessTicket ticketId)        -> void $ runApp (processTicketSafe dataLayer (TicketId ticketId)) cfg
        ProcessTickets                  -> void $ runApp (processTickets dataLayer) cfg
        ProcessTicketsFromTime fromTime -> runApp (processTicketsFromTime dataLayer fromTime) cfg
        ShowStatistics                  ->
            void $ runApp (fetchTickets dataLayer >>= (showStatistics dataLayer)) cfg
        InspectLocalZip filePath        -> runApp (inspectLocalZipAttachment filePath) cfg
        ExportData fromTime             -> void $ runApp (exportZendeskDataToLocalDB dataLayer fromTime) cfg

