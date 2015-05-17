module Log.API where

import Data.Time.Calendar
import Data.Time.Clock
import           Control.Monad                   (liftM)
import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Control.Monad.Trans.Either
import           Data.Aeson                      (ToJSON)
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString                 as B
import           Data.Profunctor.Product.Default (def)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as TE
import           GHC.Generics
import           GHC.Int                         (Int64)
import           Opaleye                         (pgStrictByteString,
                                                  pgStrictText,
                                                  runInsertReturning, runQuery)

import qualified Database.PostgreSQL.Simple      as PG
import qualified Database.Redis                  as R

import qualified Account.Session
import           Log.Types
import           Servant
import Servant.Docs


type Api = "logs" :> QueryParam "token" Text :> Get [Log]

instance ToSample [Log] where
  toSamples = [("Addition", [Log 1 1 "add" Nothing (Just 1) Nothing (Just "apples") Nothing (Just "groceries") Nothing (Just (UTCTime (fromGregorian 2015 5 1) 0)) Nothing (Just 20.5)]
               )
              ,("Edit", [Log 1 1 "edit" Nothing (Just 1) (Just "pears") (Just "apples") Nothing (Just "groceries") Nothing (Just (UTCTime (fromGregorian 2015 5 1) 0)) (Just 40.8) (Just 20.5)])
              ,("Delete", [Log 1 1 "delete" Nothing (Just 1) (Just "pears") Nothing (Just "groceries") Nothing (Just (UTCTime (fromGregorian 2015 5 1) 0)) Nothing (Just 40.8) Nothing])]

server :: PG.Connection -> R.Connection -> Server Api
server pg r = getLogs pg r
  where getLogs pg r (Just token) =
          do maid <- liftIO $ Account.Session.get r token
             case maid of
               Nothing -> return []
               Just account_id -> liftIO $ runQuery pg (getAccountLogs account_id)
