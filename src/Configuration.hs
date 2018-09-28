-- Here we need to export just the public info.
module Configuration
    ( emptyConfig
    , defaultConfig
    , basicIOLayer
    , emptyIOLayer
    ) where

import           Universum

import           DataSource.DB
import           DataSource.Types

-- | The default configuration.
-- TODO(ks): Abstract this away!
defaultConfig :: Config
defaultConfig = Config
    { cfgAgentId                = UserId 0
    , cfgZendesk                = "https://iohk.zendesk.com"
    , cfgToken                  = ""
    , cfgEmail                  = "daedalus-bug-reports@iohk.io"
    , cfgAssignTo               = 0
    , cfgKnowledgebase          = []
    , cfgNumOfLogsToAnalyze     = 5
    , cfgIsCommentPublic        = False -- TODO(ks): For now, we need this in CLI.
    -- Layers
    , cfgIOLayer                = basicIOLayer
    -- TODO(ks): Remove this.
    , cfgDBLayer                = connDBLayer
    }

-- | The empty configuration. Used in tests.
emptyConfig :: Config
emptyConfig = Config
    { cfgAgentId                = UserId 0
    , cfgZendesk                = mempty
    , cfgToken                  = mempty
    , cfgEmail                  = mempty
    , cfgAssignTo               = 0
    , cfgKnowledgebase          = mempty
    , cfgNumOfLogsToAnalyze     = 5
    , cfgIsCommentPublic        = True
    -- Layers
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

