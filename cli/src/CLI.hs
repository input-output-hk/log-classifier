module CLI
    ( CLI(..)
    , getCliArgs
    ) where

import           Universum

import           Options.Applicative (Parser, ParserInfo, ReadM, argument, auto, command,
                                      eitherReader, execParser, fullDesc, header, help, helper,
                                      hsubparser, info, infoOption, long, metavar, option, progDesc,
                                      strOption, (<**>))
import           Paths_cli (version)

import           DataSource (ExportFromTime (..))

import           Data.Time (UTCTime (..), defaultTimeLocale, parseTimeM)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

-- | List operations.
data CLI
    = FetchAgents
    -- ^ List agents
    | FetchTickets
    -- ^ Fetch all the tickets in Zendesk
    | FetchTicketsFromTime ExportFromTime
    -- ^ Fetch all the tickets in Zendesk from specific time
    | ProcessTicket Int
    -- ^ Process ticket of an given ticket id
    | ProcessTickets
    -- ^ Process all the tickets in Zendesk
    | ProcessTicketsFromTime ExportFromTime
    -- ^ Process tickets from a specific time
    | ShowStatistics
    -- ^ Show statistics
    | InspectLocalZip FilePath
    -- ^ Inspect local file
    | ExportData ExportFromTime
    -- ^ Export data. TODO(ks): From N days before!
    deriving (Show)

-- | Parser for ProcessTicket
cmdProcessTicket :: Parser CLI
cmdProcessTicket = ProcessTicket <$> argument auto
                       (metavar "TICKET_ID"
                       <> help "Ticket id to analyze")

-- | Parser for CLI commands
cli :: Parser CLI
cli = hsubparser $ mconcat
    [ command "fetch-agents" (info (pure FetchAgents)
        (progDesc "Fetch Zendesk agents"))
    , command "fetch-tickets" (info (pure FetchTickets)
        (progDesc "Fetch all the tickets from specific time."))
    , command "fetch-tickets-from" (info cmdFetchTicketsFrom
        (progDesc "Fetch all the tickets that need to be analyzes."))
    , command "process-tickets" (info (pure ProcessTickets)
        (progDesc "Process all the tickets i.e add comments, tags."))
    , command "process-tickets-from" (info cmdProcessTicketsFrom
        (progDesc "Process all the tickets i.e add comments, tags from a specific time."))
    , command "process-ticket" (info cmdProcessTicket
        (progDesc "Process Zendesk ticket of an given ticket id"))
    , command "show-stats" (info (pure ShowStatistics)
        (progDesc "Print list of ticket Ids that agent has been assigned"))
    , command "export-data" (info cmdExportDataFrom
        (progDesc "Export the data from the Zendesk API to the local database"))
    , command "inspect-local-zip" (info cmdInspectLocalZip
        (progDesc "Inspect a local zip with logs, no need to connect to Zendesk API."))
    ]
  where
    cmdInspectLocalZip :: Parser CLI
    cmdInspectLocalZip =
        InspectLocalZip <$> strOption (long "file-path"
            <> metavar "FILE_PATH"
            <> help "Path to local zip file for analysis")

    cmdFetchTicketsFrom :: Parser CLI
    cmdFetchTicketsFrom =
        FetchTicketsFromTime <$> option exportFromTimeReader (long "time"
            <> metavar "TIME"
            <> help "The time (dd.mm.YYYY) from which changed tickets are fetched.")

    cmdExportDataFrom :: Parser CLI
    cmdExportDataFrom =
        ExportData <$> option exportFromTimeReader (long "time"
            <> metavar "TIME"
            <> help "The time (dd.mm.YYYY) from which we export the changed tickets.")

    cmdProcessTicketsFrom :: Parser CLI
    cmdProcessTicketsFrom =
        ProcessTicketsFromTime <$> option exportFromTimeReader (long "time"
            <> metavar "TIME"
            <> help "The time (dd.mm.YYYY) from which we process the changed tickets.")
    exportFromTimeReader :: ReadM ExportFromTime
    exportFromTimeReader = eitherReader $ \arg -> do

        day <- case parseTimeM True defaultTimeLocale "%d.%m.%Y" arg of
            Nothing  -> Left ("Cannot parse date: " ++ arg)
            Just day -> Right day

        let utctime     = UTCTime day 0
        let posixTime   = utcTimeToPOSIXSeconds utctime

        pure $ ExportFromTime posixTime

-- | Get CLI arguments from command line
getCliArgs :: IO CLI
getCliArgs = execParser opts
  where
    opts ::  ParserInfo CLI
    opts = info (cli <**> helper <**> versionHelper)
        ( fullDesc
        <> header "Log classifier"
        <> progDesc "Client for peforming analysis on Zendesk"
        )
    versionHelper :: Parser (a -> a)
    versionHelper = infoOption
        ("Log classifier version" <> show version)
        (long "version" <> help "Show version")
