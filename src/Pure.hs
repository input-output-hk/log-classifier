{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Pure where

import Universum

import Test.QuickCheck

data ZendeskAttachment = ZendeskAttachment { _getZendeskAttachment :: [String] }
    deriving (Eq, Show)

data ZendeskTicketContent
    = ZendeskTicketContent { _getTicketContent :: ZendeskAttachment }
    deriving (Eq, Show)

newtype IssueName = IssueName { _getIssueName :: String }
    deriving (Eq, Show)

instance Arbitrary IssueName where
    arbitrary = do
        arbitraryIssueNumber <- show @String @Int <$> arbitrary `suchThat` (> 0)
        pure . IssueName $ "IOHKS-" <> arbitraryIssueNumber

newtype IssueRegex = IssueRegex { _getIssueRegex :: String }
    deriving (Eq, Show)

-- TODO(ks): For now let's say that we choose a very restricted subset for these...
instance Arbitrary IssueRegex where
    arbitrary = IssueRegex <$> (listOf1 $ choose ('a', 'z'))


data IOHKSIssue = IOHKSIssue
    { _getIOHKSIssueName    :: IssueName
    , _getIOHKSIssueRegex   :: IssueRegex
    }
    deriving (Eq, Show)

instance Arbitrary IOHKSIssue where
    arbitrary = IOHKSIssue
        <$> arbitrary @IssueName
        <*> arbitrary @IssueRegex

data IOHKSIssues
    = IOHKSIssues { _getIOHKSIssues :: [IOHKSIssue] }
    deriving (Eq, Show)

{-

"useless for the following reason","TimeSync","Daedalus Wallet Support Issue 1 Your PC's time is out of sync","To fix this issue, user needs to synchronize your computer's time","227"
"DBMalformed","DBError","Daedalus Wallet Support Issue 2 Local block data is corrupted","To fix this issue, please delete the content of the DB-1.0 folder","228"
"signalProcess: permission denied (Operation not permitted","StaleLockFile","Daedalus Wallet Support Issue 3 Launching node without admin rights","Please make sure you launch the application with admin rights","229"
"No such file or directory","FileNotFound","Daedalus Wallet Support Issue 4 File missing","to fix this issue, Please reinstall Daedalus and run the application","230"
"resource exhausted (No space left on device)","ShortStorage","Daedalus Wallet Support Issue 5 Not enough space on hard drive to store block data","To fix this, please create more disk space on your computer","231"
"returned empty list","NetworkError","Daedalus Wallet Support Issue 6 Firewall is blocking the connection","To fix this issue, please try other ISP provider","237"
"irrelevant to given wallet","BalanceError","Daedalus Wallet Support Issue 7 Daedalus shows wrong Ada amount","Unfortunately, there's no workaround for this solution. Please report support@iohk.io immediately","211"
"Network.Socket.recvBuf: resource vanished","ResourceVanished","Daedalus Wallet Support Issue 8 Network error","To fix this issue, please try other ISP provider","234"
"IO error: Failed to create dir","UserNameError","Daedalus Wallet Support Issue 9 User is using non-latin characters for username","To fix this issue, pleae change your PC's username using only latin-characters","235"
"open.lock: Locked by","StaleLockFile","Daedalus Wallet Support Issue 10 open.lock file is corrupted due to improper shutdown","The issue is that the application was improperly shutdown. As a workaround, you'll need to delete open.lock file.","236"
"Network.Socket.recvBuf: failed (No error)","ConnectionRefused","Daedalus Wallet Support Issue 11 Firewall is blocking the connection","To fix this issue, please try other ISP provider","234"

-}

instance Arbitrary IOHKSIssues where
    arbitrary = pure $ IOHKSIssues
        [ IOHKSIssue (IssueName "IOHKS-73") (IssueRegex "DBMalformed")
        , IOHKSIssue (IssueName "IOHKS-72") (IssueRegex "signalProcess: permission denied (Operation not permitted")
        ]

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

prop_analyzeTicketAttachment :: ZendeskTicketContent -> IOHKSIssues -> Bool
prop_analyzeTicketAttachment zendeskTicketContent iohksIssues =
    any (`elem` ticketContent) iohksRegexes -- a naive but initial version
  where
    iohksRegexes :: [String]
    iohksRegexes = map (_getIssueRegex . _getIOHKSIssueRegex) . _getIOHKSIssues $ iohksIssues

    ticketContent :: [String]
    ticketContent = _getZendeskAttachment . _getTicketContent $ zendeskTicketContent

-- processTickets


