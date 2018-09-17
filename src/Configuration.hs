-- Here we need to export just the public info.
module Configuration
    ( emptyConfig
    , defaultConfig
    , basicIOLayer
    , emptyIOLayer
    ) where

import           Universum

import           DataSource.DB
import           DataSource.Http
import           DataSource.Types

import           Http.Layer (basicHTTPNetworkLayer, emptyHTTPNetworkLayer)
import           Http.Queue (ShedulerConfig)

-- | The default configuration.
defaultConfig :: ShedulerConfig -> Config
defaultConfig shedulerConfig = Config
    { cfgAgentId                = 0
    , cfgZendesk                = "https://iohk.zendesk.com"
    , cfgToken                  = ""
    , cfgEmail                  = "daedalus-bug-reports@iohk.io"
    , cfgAssignTo               = 0
    , cfgKnowledgebase          = []
    , cfgNumOfLogsToAnalyze     = 5
    , cfgIsCommentPublic        = False -- TODO(ks): For now, we need this in CLI.
    -- * Layers
    , cfgDataLayer              = basicDataLayer
    , cfgHTTPNetworkLayer       = basicHTTPNetworkLayer shedulerConfig
    , cfgIOLayer                = basicIOLayer
    , cfgDBLayer                = connDBLayer
    }

-- | The empty configuration. Used in tests.
emptyConfig :: Config
emptyConfig = Config
    { cfgAgentId                = 0
    , cfgZendesk                = mempty
    , cfgToken                  = mempty
    , cfgEmail                  = mempty
    , cfgAssignTo               = 0
    , cfgKnowledgebase          = mempty
    , cfgNumOfLogsToAnalyze     = 5
    , cfgIsCommentPublic        = True
    -- * Layers
    , cfgDataLayer              = emptyDataLayer
    , cfgHTTPNetworkLayer       = emptyHTTPNetworkLayer
    , cfgIOLayer                = emptyIOLayer
    , cfgDBLayer                = emptyDBLayer
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

-- | The empty @IO@ layer.
emptyIOLayer :: IOLayer App
emptyIOLayer = IOLayer
    { iolAppendFile             = \_ _   -> pure ()
    , iolPrintText              = \_     -> pure ()
    , iolReadFile               = \_     -> pure mempty
    , iolLogDebug               = \_     -> pure ()
    , iolLogInfo                = \_     -> pure ()
    }

