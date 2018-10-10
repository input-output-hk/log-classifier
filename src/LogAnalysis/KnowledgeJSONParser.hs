{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module LogAnalysis.KnowledgeJSONParser
       ( parseKnowledgeBaseJSON
       ) where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Network.HTTP.Simple
import           Network.HTTP.Types.Header
import qualified System.Environment as E
import           Universum

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

    res <- httpJSON req
    liftIO $ Universum.print (getResponseBody res :: Value) -- jsonBody --(Data.Aeson.decode jsonBody :: Maybe Value)
{-    Knowledge
        {  kErrorText = "TEXTEX"
        ,  kErrorCode = TimeSync
        ,  kProblem   = "PROB"
        ,  kSolution  = "SOL"
        ,  kFAQNumber = "204"
        } : []

    key <- E.getEnv "YT_KEY"

getRequestHeaders :: RequestHeaders
getRequestHeaders = [ ( "Content-Type" , "application/json")
                    , ( "Authorization" , "Bearer -AxLVv0fegyk-opT.2sG5JGQ3VNHUL08WpWkO3s7tVI")
                    ]
-}
