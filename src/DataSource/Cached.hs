module DataSource.Cached
    (
    ) where

import           Universum

import           Control.Applicative
import           Data.Text (split)
import           Data.Coerce (coerce)
import qualified Data.List.NonEmpty as NE

import           Database.Selda hiding (toText)
import           Database.Selda.Generic
import           Database.Selda.Backend

import           Database.Selda.SQLite

import           DataSource.Types


-- | The cached Zendesk layer. We are using a simple SQLite DB behind the scenes
-- that we need to sync occasionaly.
cachedZendeskLayer :: (MonadIO m, MonadReader Config m) => ZendeskLayer m
cachedZendeskLayer = ZendeskLayer
    { zlGetTicketInfo           = getTicketInfo
    , zlListTickets             = listTickets
    , zlPostTicketComment       = postTicketComment
    , zlGetAgentId              = getAgentId
    , zlGetAttachment           = getAttachment
    , zlGetTicketComments       = getTicketComments
    }

getTicketInfo = error "!"

-- The instances related to the database only.

instance SqlType TicketId where
  mkLit (TicketId tId)      = LCustom $ LInt tId
  sqlType _                 = TInt
  fromSql (SqlInt x)        = TicketId x
  fromSql v                 = error . toText $ "fromSql: int column with non-int value: " ++ show v
  defaultValue              = LCustom $ LInt (-1)

instance SqlType TicketURL where
  mkLit (TicketURL tURL)    = LCustom $ LText tURL
  sqlType _                 = TText
  fromSql (SqlString x)     = TicketURL x
  fromSql v                 = error . toText $ "fromSql: text column with non-text value: " ++ show v
  defaultValue              = LCustom $ LText mempty

instance SqlType TicketTags where
  mkLit (TicketTags tTags)  = LCustom $ LText tTags
  sqlType _                 = TText
  fromSql (SqlString x)     = TicketTags x
  fromSql v                 = error . toText $ "fromSql: text column with non-text value: " ++ show v
  defaultValue              = LCustom $ LText mempty


-- | Yes, not normalized, I know...
convertTextToTags :: Text -> TicketTags
convertTextToTags = TicketTags . split (==',')

convertTagsToText :: TicketTags -> [Text]
convertTagsToText (TicketTags tTags) = join ',' tTags

-- https://selda.link/
-- https://github.com/valderman/selda#generic-tables-and-queries
-- http://hackage.haskell.org/package/selda-0.2.0.0/docs/Database-Selda.html


withLocalSqlite :: forall m a. (MonadIO m, MonadMask m) => SeldaT m a -> m a
withLocalSqlite executable = withSQLite "log_classifier.sqlite" executable

ticketInfo :: GenTable TicketInfo
ticketInfo = genTable "ticket_info" [ticketId :- primaryGen]

{-
ticketsInfo :: Table (Int :*: Text :*: Text :*: Text)
ticketsInfo = table "TICKET_INFO"
      $ primary "ID"
    :*: required "URL"
    :*: required "TAGS"
    :*: required "STATUS"



getAllTickets :: SeldaM [Int :*: Text :*: Text :*: Text]
getAllTickets = query (select ticketsInfo)


getTicketByTicketId :: TicketId -> SeldaM [Int :*: Text :*: Text :*: Text]
getTicketByTicketId (TicketId ticketId) =
    query $ do
        ti@(tid :*: _ :*: _ :*: _) <- select ticketsInfo
        restrict (tid .== int ticketId)
        pure ti

getTicketInfo :: forall m. (MonadIO m) => TicketId -> m (Maybe TicketInfo)
getTicketInfo ticketId = liftIO $ withLocalSqlite $ do
    ticketInfoByTicketId <- getTicketByTicketId ticketId

    pure . safeHead . map convertToTicketsInfo $ ticketInfoByTicketId
  where
    convertToTicketsInfo :: (Int :*: Text :*: Text :*: Text) -> TicketInfo
    convertToTicketsInfo (id :*: url :*: tags :*: status) = TicketInfo
        { ticketId      = TicketId id
        , ticketUrl     = TicketURL url
        , ticketTags    = convertTextToTags tags
        , ticketStatus  = TicketStatus status
        }


-}

listTickets = error "!"

postTicketComment = error "!"

getAgentId = error "!"

getAttachment = error "!"

getTicketComments = error "!"

