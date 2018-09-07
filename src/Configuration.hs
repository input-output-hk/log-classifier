-- Here we need to export just the public info.
module Configuration
    ( defaultConfig
    , basicIOLayer
    ) where

import           Universum

import           DataSource.DB
import           DataSource.Http
import           DataSource.Types
import           HttpLayer (basicHTTPNetworkLayer)


-- | The default configuration.
defaultConfig :: Config
defaultConfig = Config
    { cfgAgentId                = 0
    , cfgZendesk                = "https://iohk.zendesk.com"
    , cfgToken                  = ""
    , cfgEmail                  = "daedalus-bug-reports@iohk.io"
    , cfgAssignTo               = 0
    , cfgKnowledgebase          = []
    , cfgNumOfLogsToAnalyze     = 5
    , cfgIsCommentPublic        = True -- TODO(ks): For now, we need this in CLI.
    , cfgDataLayer              = basicDataLayer
    , cfgHTTPNetworkLayer       = basicHTTPNetworkLayer
    , cfgIOLayer                = basicIOLayer
    , cfgDBLayer                = connDBLayer
    }

-- | The @IO@ layer.
basicIOLayer :: (MonadIO m, MonadReader Config m) => IOLayer m
basicIOLayer = IOLayer
    { iolAppendFile             = appendFile
    , iolPrintText              = putTextLn -- iolPrintConsole
    , iolReadFile               = readFile
    , iolLogDebug               = putTextLn
    , iolLogInfo                = putTextLn
    }

