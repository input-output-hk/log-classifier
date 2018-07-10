module CLI
    ( CLI(..)
    , getCliArgs
    ) where

import           Universum

import           Options.Applicative (Parser, ParserInfo, argument, auto, command, execParser,
                                      fullDesc, header, help, helper, hsubparser, info, infoOption,
                                      long, metavar, progDesc, strOption, (<**>))
import           Paths_log_classifier (version)

-- | TODO(ks): Ideally we should drop this and
-- use direct function calls.
data CLI
    = CollectEmails             -- ^ Collect email addresses
    | FetchAgents               -- ^ List agents
    | FetchTickets              -- ^ Fetch all the tickets in Zendesk
    | ProcessTicket Int         -- ^ Process ticket of an given ticket id
    | ProcessTickets            -- ^ Process all the tickets in Zendesk
    | ShowStatistics            -- ^ Show statistics
    | InspectLocalZip FilePath  -- ^ Inspect local file
    | ExportData                -- ^ Export data. TODO(ks): From N days before!
    deriving (Show)

-- | Parser for ProcessTicket
cmdProcessTicket :: Parser CLI
cmdProcessTicket = ProcessTicket <$> argument auto
                       (metavar "TICKET_ID"
                       <> help "Ticket id to analyze")

-- | Parser for CLI commands
cli :: Parser CLI
cli = hsubparser $ mconcat
    [ command "collect-emails" (info (pure CollectEmails)
        (progDesc "Collect emails requested by single user"))
    , command "fetch-agents" (info (pure FetchAgents)
        (progDesc "Fetch Zendesk agents"))
    , command "fetch-tickets" (info (pure FetchTickets)
        (progDesc "Fetch all the tickets that need to be analyzes."))
    , command "process-tickets" (info (pure ProcessTickets)
        (progDesc "Process all the tickets i.e add comments, tags."))
    , command "process-ticket" (info cmdProcessTicket
        (progDesc "Process Zendesk ticket of an given ticket id"))
    , command "show-stats" (info (pure ShowStatistics)
        (progDesc "Print list of ticket Ids that agent has been assigned"))
    , command "export-data" (info (pure ExportData)
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
