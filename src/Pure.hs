{-# OPTIONS_GHC -Wmissing-methods #-}

module Pure where

import Universum

import Test.QuickCheck

data ZendeskAttachment = ZendeskAttachment [String]
    deriving (Eq, Show)

data ZendeskTicketContent
    = ZendeskTicketContent ZendeskAttachment
    deriving (Eq, Show)

newtype IssueName = IssueName String
    deriving (Eq, Show)

instance Arbitrary IssueName where
    arbitrary = do
        arbitraryIssueNumber <- show @String @Int <$> arbitrary `suchThat` (> 0)
        pure . IssueName $ "IOHKS-" <> arbitraryIssueNumber

newtype IssueRegex = IssueRegex String
    deriving (Eq, Show)

-- TODO(ks): For now let's say that we choose a very restricted subset for these...
instance Arbitrary IssueRegex where
    arbitrary = IssueRegex <$> (listOf1 $ choose ('a', 'z'))


data IOHKSIssue = IOHKSIssue IssueName IssueRegex
    deriving (Eq, Show)

instance Arbitrary IOHKSIssue where
    arbitrary = IOHKSIssue
        <$> arbitrary @IssueName
        <*> arbitrary @IssueRegex

data IOHKSIssues
    = IOHKSIssues [IOHKSIssue]
    deriving (Eq, Show)


instance Arbitrary IOHKSIssues where
    arbitrary = IOHKSIssues <$> (listOf1 $ arbitrary @IOHKSIssue)

data ZendeskComment
    = ZendeskComment [String]
    deriving (Eq, Show)

instance Arbitrary ZendeskComment where
    arbitrary = ZendeskComment <$> listOf1 arbitrary

-- TODO(ks): For now, type synonym
newtype TicketId = TicketId Int
    deriving (Eq, Show)

instance Arbitrary TicketId where
    arbitrary = TicketId <$> arbitrary `suchThat` (> 0)


type ValidTicketIds = [TicketId]

quickCheck_fetchZendeskIssue :: TicketId -> ValidTicketIds -> Property
quickCheck_fetchZendeskIssue ticketId validTicketIds =
    fetchZendeskIssue ticketId validTicketIds ===
        if ticketId `elem` validTicketIds
           then (Just . ZendeskTicketContent . ZendeskAttachment $ ["testing"])
           else Nothing


processTicket
    :: TicketId
    -> (TicketId -> Maybe ZendeskTicketContent)
    -> ZendeskComment
processTicket ticketId fetchZendeskIssueF =
    error "undefined..."

fetchZendeskIssue :: TicketId -> ValidTicketIds -> Maybe ZendeskTicketContent
fetchZendeskIssue ticketId validTicketIds
    | ticketId `elem` validTicketIds  = Just . ZendeskTicketContent . ZendeskAttachment $ ["testing"]
    | otherwise                       = Nothing


analyzeTicketAttachment :: ZendeskTicketContent -> IOHKSIssues -> ZendeskComment
analyzeTicketAttachment zendeskTicketContent iohksIssues =
    error "undefined..."


-- processTickets


