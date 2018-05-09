module CLI
    ( CLI(..)
    , getCliArgs
    ) where

import           Data.Semigroup ((<>))
import           Options.Applicative (Parser, argument, auto, command, execParser, fullDesc, header,
                                      help, helper, info, infoOption, long, metavar, progDesc,
                                      strOption, subparser, (<**>))
import           Paths_log_classifier (version)

data CLI
    = CollectEmails
    | ProcessTicket Int
    | ProcessTickets
    | RawRequest String
    | ShowStatistics
    deriving (Eq, Show)

-- How do I dispaly helper..
cmdProcessTicket :: Parser CLI
cmdProcessTicket = ProcessTicket <$> argument auto
                       ( metavar "TICKET_ID"
                      <> help "Specify ticket id to analyze")

cmdRawRequest :: Parser CLI
cmdRawRequest = RawRequest <$> strOption
                    ( metavar "URL"
                   <> help "Specify url to request")

cli :: Parser CLI
cli = subparser $ mconcat
        [
          command "collectEmails" (info (pure CollectEmails)
            (progDesc "Collect emails requested by single user"))
        , command "processTickets" (info (pure ProcessTickets)
            (progDesc "Process all the tickets i.e add comments, tags."))
        , command "processTicket" (info cmdProcessTicket
            (progDesc "Process Zendesk ticket of an given <TICKET_ID>"))
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
        versionHelper =
            infoOption
              ("Log classifier version" <> show version)
              (long "version" <> help "Show version")

