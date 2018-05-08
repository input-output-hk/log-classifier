module CLI
    ( getCliArgument
    ) where

import           Data.Semigroup ((<>))
import           Options.Applicative
import           Paths_log_classifier (version)


data CLI
    = CollectEmails
    | ProcessTicket Int
    | ProcessTickets
    | ShowStatistics
    deriving (Eq, Show)

cmdProcessTicket :: Parser CLI
cmdProcessTicket = ProcessTicket <$> 
                  argument auto (metavar "TICKET_ID"
                              <> help "Specify ticket id to analyze")

cli :: Parser CLI
cli = subparser
        (
          command "collectEmails" (info (pure CollectEmails)
            (progDesc "Collect emails requested by single user"))
      <> command "processTickets" (info (pure ProcessTickets)
            (progDesc "Process all the tickets i.e add comments, tags."))
      <> command "processTicket" (info cmdProcessTicket
            (progDesc "Process Zendesk ticket of an given <TICKET_ID>"))
      <> command "showStats" (info (pure ShowStatistics)
            (progDesc "Print list of ticket Ids that agent has been assigned"))
        )

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
  