{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module LogAnalysis.KnowledgeJSONParser
       ( parseYTJSON,
         printYT
       ) where

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.Aeson.Types
import           Data.Maybe (fromJust)
import           GHC.Generics
import           LogAnalysis.Types (ErrorCode (..), Knowledge (..))
import           Network.HTTP.Simple
import           Network.HTTP.Types.Header
import           Universum

data Issue = Issue
    { comment  :: !Array
    , entityId :: !Text
    , field    :: [Field]
    , iId      :: !Text
    , tag      :: !Array
    } deriving (Show)

data Field = Field
    { name  :: !Text
    , value :: !Text
    } deriving (Show)

instance FromJSON Field where
    parseJSON (Object v) = Field
        <$> v .: "name"
        <*> v .: "value"

instance FromJSON Issue where
    parseJSON (Object v) = Issue
        <$> v .: "comment"
        <*> v .: "entityId"
        <*> v .: "field"
        <*> v .: "id"
        <*> v .: "tag"
{-
      Knowledge
          <$> o .: "id" -- kErrorText
          <*> o .: "id" --kErrorCode
          <*> o .: "id" -- kProblem
          <*> o .: "id" -- kSolution
          <*> o .: "id" -- kFAQNUmber

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

printYT :: IO ()
printYT = parseYTJSON >>= print

parseYTJSON :: IO [Issue]
parseYTJSON = do
    key <- Universum.readFile "tmp-secrets/yt-token"
    let bKey = encodeUtf8 ("Bearer " <> key)
    let url = "https://iohk.myjetbrains.com/youtrack/rest/issue/byproject/IOHKS"

    req' <- parseRequest url
    let req
         = setRequestSecure True
         $ addRequestHeader hAccept "application/json"
         $ addRequestHeader hAuthorization bKey
         $ setRequestSecure True req'

    ytJSON <- getResponseBody <$> httpLBS req
    return $ fromJust (decode ytJSON :: Maybe [Issue])

