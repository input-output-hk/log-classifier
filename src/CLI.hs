module CLI
    ( CLI(..)
    , getCliArgs
    ) where

import           Data.Semigroup ((<>))
import           Options.Applicative (Parser, argument, auto, command, execParser, fullDesc, header,
                                      help, helper, hsubparser, info, infoOption, long, metavar,
                                      progDesc, strOption, (<**>))
import           Paths_log_classifier (version)

data CLI
    = CollectEmails     -- ^ Collect email addresses
    | ProcessTicket Int -- ^ Process ticket of an given ticket id
    | ProcessTickets    -- ^ Procss all the tickets in the Zendesk
    | RawRequest String -- ^ Raw request to the given url
    | ShowStatistics    -- ^ Show statistics
    deriving (Show)

-- | Parser for ProcessTicket
cmdProcessTicket :: Parser CLI
cmdProcessTicket = ProcessTicket <$> argument auto
                       ( metavar "TICKET_ID"
                      <> help "Ticket id to analyze")

-- | Parser for RawRequest
cmdRawRequest :: Parser CLI
cmdRawRequest = RawRequest <$> strOption
                    ( metavar "URL"
                   <> help "Url to request")

-- | Parser for CLI commands
cli :: Parser CLI
cli = hsubparser $ mconcat
        [
          command "collectEmails" (info (pure CollectEmails)
            (progDesc "Collect emails requested by single user"))
        , command "processTickets" (info (pure ProcessTickets)
            (progDesc "Process all the tickets i.e add comments, tags."))
        , command "processTicket" (info cmdProcessTicket
            (progDesc "Process Zendesk ticket of an given ticket id"))
        , command "rawRequest" (info cmdRawRequest
            (progDesc "Raw request to the given url"))
        , command "showStats" (info (pure ShowStatistics)
            (progDesc "Print list of ticket Ids that agent has been assigned"))
        ]

getCliArgs :: IO CLI
getCliArgs = execParser opts
      where
        opts = info (cli <**> helper <**> versionHelper)
            ( fullDesc
            <> header "Log classifier"
            <> progDesc "Client for peforming analysis on Zendesk"
            )
        versionHelper = infoOption
            ("Log classifier version" <> show version)
            (long "version" <> help "Show version")
