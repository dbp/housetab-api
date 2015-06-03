module Entry.API where

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
import           Entry.Types
import           Servant
import Servant.Docs


type Api = "entries" :> ReqBody '[JSON] NewEntry :> Post '[JSON] Entry
      :<|> "entries" :> QueryParam "token" Text :> Get '[JSON] [Entry]

instance ToSample [Entry] [Entry] where
  toSample _ = Just [Entry 1 1 1 "apples" "groceries" (UTCTime (fromGregorian 2015 5 1) 0) 4.5 [1,2]]

instance ToSample Entry Entry where
  toSample _ = Just (Entry 1 1 1 "apples" "groceries" (UTCTime (fromGregorian 2015 5 1) 0) 4.5 [1,2])

instance ToSample NewEntry NewEntry where
  toSample _ = Just (Entry () 1 1 "apples" "groceries" (UTCTime (fromGregorian 2015 5 1) 0) 4.5 [1,2])

server :: PG.Connection -> R.Connection -> Server Api
server pg r = postEntry pg r
         :<|> getEntries pg r
  where getEntries pg r (Just token) =
          do maid <- liftIO $ Account.Session.get r token
             case maid of
               Nothing -> return []
               Just account_id -> liftIO $ runQuery pg (getAccountEntries account_id)
        postEntry pg r entry =
          do entry' <- liftIO $ conv entry
             [entry''] <- liftIO $ runInsertReturning pg entryTable entry' id
             return entry''
