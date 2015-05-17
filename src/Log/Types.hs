module Log.Types where

import           GHC.Generics
import           Prelude                    hiding (Sum)

import Data.List (intercalate)
import Control.Applicative ((<$>))
import           Control.Arrow              (returnA)
import qualified Crypto.Hash.SHA512         as SHA512
import           Data.Aeson                 (FromJSON (..), ToJSON (..))
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Base64     as B64
import           Data.ByteString.Internal   (c2w)
import           Data.Profunctor.Product    (p2, p3)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Text                  (Text)
import qualified Data.Text.Encoding         as TE
import           Data.Time.Clock
import           Opaleye
import           System.Random              (randomRIO)
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

data Log' a b c d e f g h i j k l m n o = Log { logId        :: a
                                              , logAccountId :: b
                                              , logType       :: c
                                              , logWhoOld      :: d
                                              , logWhoNew      :: e
                                              , logWhatOld  :: f
                                              , logWhatNew  :: g
                                              , logCategoryOld  :: h
                                              , logCategoryNew  :: i
                                              , logDateOld   :: j
                                              , logDateNew   :: k
                                              , logHowMuchOld :: l
                                              , logHowMuchNew :: m
                                              , logWhoPaysOld :: n
                                              , logWhoPaysNew :: o
                                              }

type Log = Log' Int
                Int
                Text
                (Maybe Int)
                (Maybe Int)
                (Maybe Text)
                (Maybe Text)
                (Maybe Text)
                (Maybe Text)
                (Maybe UTCTime)
                (Maybe UTCTime)
                (Maybe Double)
                (Maybe Double)
                (Maybe [Int])
                (Maybe [Int])

type NewLog = Log' ()
                   Int
                   Text
                   (Maybe Int)
                   (Maybe Int)
                   (Maybe Text)
                   (Maybe Text)
                   (Maybe Text)
                   (Maybe Text)
                   (Maybe UTCTime)
                   (Maybe UTCTime)
                   (Maybe Double)
                   (Maybe Double)
                   (Maybe [Int])
                   (Maybe [Int])

type LogColumn = Log' (Column PGInt4)
                      (Column PGInt4)
                      (Column PGText)
                      (Column (Nullable PGInt4))
                      (Column (Nullable PGInt4))
                      (Column (Nullable PGText))
                      (Column (Nullable PGText))
                      (Column (Nullable PGText))
                      (Column (Nullable PGText))
                      (Column (Nullable PGTimestamptz))
                      (Column (Nullable PGTimestamptz))
                      (Column (Nullable PGFloat8))
                      (Column (Nullable PGFloat8))
                      (Column (Nullable (PGArray PGInt4)))
                      (Column (Nullable (PGArray PGInt4)))

type NewLogColumn = Log' (Maybe (Column PGInt4))
                         (Column PGInt4)
                         (Column PGText)
                         (Column (Nullable PGInt4))
                         (Column (Nullable PGInt4))
                         (Column (Nullable PGText))
                         (Column (Nullable PGText))
                         (Column (Nullable PGText))
                         (Column (Nullable PGText))
                         (Column (Nullable PGTimestamptz))
                         (Column (Nullable PGTimestamptz))
                         (Column (Nullable PGFloat8))
                         (Column (Nullable PGFloat8))
                         (Column (Nullable (PGArray PGInt4)))
                         (Column (Nullable (PGArray PGInt4)))

deriving instance Generic (Log' a b c d e f g h i j k l m n o)
instance ToJSON Log
instance FromJSON Log
instance ToJSON NewLog
instance FromJSON NewLog

$(makeAdaptorAndInstance "pLog" ''Log')

-- NOTE(dbp 2015-05-02): This function is boilerplate, hopefully removable at some point.
conv :: NewLog -> IO NewLogColumn
conv (Log {..}) = do
  return $ Log { logId = Nothing
               , logAccountId = pgInt4 logAccountId
               , logType      = pgStrictText logType
               , logWhoOld       = maybeToNullable $ pgInt4 <$> logWhoOld
               , logWhoNew       = maybeToNullable $ pgInt4 <$> logWhoNew
               , logWhatOld      = maybeToNullable $ pgStrictText <$> logWhatOld
               , logWhatNew      = maybeToNullable $ pgStrictText <$> logWhatNew
               , logCategoryOld  = maybeToNullable $ pgStrictText <$> logCategoryOld
               , logCategoryNew  = maybeToNullable $ pgStrictText <$> logCategoryNew
               , logDateOld      = maybeToNullable $ pgUTCTime <$> logDateOld
               , logDateNew      = maybeToNullable $ pgUTCTime <$> logDateNew
               , logHowMuchOld   = maybeToNullable $ pgDouble <$> logHowMuchOld
               , logHowMuchNew   = maybeToNullable $ pgDouble <$> logHowMuchNew
               , logWhoPaysOld   = maybeToNullable $ pgInt4Array <$> logWhoPaysOld
               , logWhoPaysNew   = maybeToNullable $ pgInt4Array <$> logWhoPaysNew
               }

pgInt4Array :: [Int] -> Column (PGArray PGInt4)
pgInt4Array l = literalColumn . HPQ.OtherLit $ "{" ++ (intercalate "," (map show l)) ++ "}"

logTable :: Table NewLogColumn LogColumn
logTable = Table "log" (pLog Log { logId = optional "id"
                                 , logAccountId = required "account_id"
                                 , logType = required "type"
                                 , logWhoOld       = required "who_old"
                                 , logWhoNew       = required "who_new"
                                 , logWhatOld      = required "what_old"
                                 , logWhatNew      = required "what_new"
                                 , logCategoryOld  = required "category_old"
                                 , logCategoryNew  = required "category_new"
                                 , logDateOld      = required "date_old"
                                 , logDateNew      = required "date_new"
                                 , logHowMuchOld   = required "howmuch_old"
                                 , logHowMuchNew   = required "howmuch_new"
                                 , logWhoPaysOld   = required "whopays_old"
                                 , logWhoPaysNew   = required "whopays_new"
                                 })

logQuery :: Query LogColumn
logQuery = orderBy (desc logId) $ queryTable logTable

getAccountLogs :: Int -> Query LogColumn
getAccountLogs account_id = proc () ->
   do log <- logQuery -< ()
      restrict -< logAccountId log .== pgInt4 account_id
      returnA -< log
