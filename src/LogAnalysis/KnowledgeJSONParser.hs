{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module LogAnalysis.KnowledgeJSONParser
       ( parseKnowledgeBaseJSON
       , processJSON
       ) where

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.Aeson.Types
import           Data.Maybe (fromJust)
import           GHC.Generics
import           LogAnalysis.Types (Knowledge (..))
import           Network.HTTP.Simple
import           Network.HTTP.Types.Header
import           Universum

data Issue = Issue {
      comment  :: !Array
    , entityId :: !Text
    , field    :: !Array
    , id       :: !Text
    , tag      :: !Array
    } deriving (Generic, Show)

{-
instance FromJSON Issue where
    parseJSON = \o -> do
        comment  <- o .: "comment"
        entityId <- o .: "entityId"
        field    <- o .: "field"
        id       <- o .: "id"
        tag      <- o .: "tag"

        pure Issue
            { iComment  = comment
            , iEntityId = entityId
            , iField    = field
            , iId       = id
            , iTag      = tag
            }
-}

instance ToJSON Issue
instance FromJSON Issue

parseKnowledgeBaseJSON :: IO ()
parseKnowledgeBaseJSON = do
    key <- Universum.readFile "tmp-secrets/yt-token"
    let bKey = encodeUtf8 ("Bearer " <> key)

    let url = "https://iohk.myjetbrains.com/youtrack/rest/issue/byproject/IOHKS"

    req' <- parseRequest url
    let req
         = setRequestSecure True
         $ addRequestHeader hAccept "application/json"
         $ addRequestHeader hAuthorization bKey
         $ addRequestHeader hContentType "application/json"
         $ setRequestSecure True req'

    json <- getResponseBody <$> httpJSON req
    processJSON json

processJSON :: Object -> IO ()
processJSON json = do
    let out = fromJust $ parseMaybe (.: "id") json :: String


{-    Knowledge
        {  kErrorText = "TEXTEX"
        ,  kErrorCode = TimeSync
        ,  kProblem   = "PROB"
        ,  kSolution  = "SOL"
        ,  kFAQNumber = "204"
        } : []

-}
