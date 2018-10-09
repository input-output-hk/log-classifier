module Main where

import           Universum

import           DataSource (TicketId (..), runApp)
import           Lib (createBasicDataLayerIO, createConfig, exportZendeskDataToLocalDB, fetchAgents,
                      fetchAndShowTickets, fetchAndShowTicketsFrom, fetchTickets,
                      inspectLocalZipAttachment, processTicketSafe, processTickets,
                      processTicketsFromTime, showStatistics)
import           Web.Server (mainServer)

-- | Main entry point.
main :: IO ()
main = mainServer

