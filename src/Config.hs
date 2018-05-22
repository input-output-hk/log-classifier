module Config
    ( Config (..)
    , defaultConfig
    , knowledgebasePath
    , tokenPath
    , assignToPath
    ) where

import Universum

import LogAnalysis.Types (Knowledge)

-- | The basic configuration.
data Config = Config
    { cfgAgentId            :: !Integer
    -- ^ Zendesk agent id
    , cfgZendesk            :: !Text
    -- ^ URL to Zendesk
    , cfgToken              :: !Text
    -- ^ Zendesk token
    , cfgEmail              :: !Text
    -- ^ Email address of the user the classifier will process on
    , cfgAssignTo           :: !Integer
    -- ^ User that will be assigned to after the classifier has done the analysis
    , cfgKnowledgebase      :: ![Knowledge]
    -- ^ Knowledgebase
    , cfgNumOfLogsToAnalyze :: !Int
    -- ^ Number of files classifier will analyze
    , cfgIsCommentPublic    :: !Bool
    -- ^ If the comment is public or not, for a test run we use an internal comment.
    } deriving (Eq, Show)

defaultConfig :: Config
defaultConfig =
    Config
        { cfgAgentId            = 0
        , cfgZendesk            = "https://iohk.zendesk.com"
        , cfgToken              = ""
        , cfgEmail              = "daedalus-bug-reports@iohk.io"
        , cfgAssignTo           = 0
        , cfgKnowledgebase      = []
        , cfgNumOfLogsToAnalyze = 5
        , cfgIsCommentPublic    = True -- TODO(ks): For now, we need this in CLI.
        }

-- | Path to knowledgebase
knowledgebasePath :: FilePath
knowledgebasePath = "./knowledgebase/knowledge.csv"

-- | Filepath to token file
tokenPath :: FilePath
tokenPath = "./tmp-secrets/token"

-- | Filepath to assign_to file
assignToPath :: FilePath
assignToPath = "./tmp-secrets/assign_to"
