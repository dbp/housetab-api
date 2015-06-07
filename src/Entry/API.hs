{-# LANGUAGE ScopedTypeVariables #-}
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
                                                  pgStrictText, pgBool, pgInt4, (.==),
                                                  runInsert, runInsertReturning, runQuery, runUpdate)

import qualified Database.PostgreSQL.Simple      as PG
import qualified Database.Redis                  as R

import qualified Account.Session
import           Entry.Types
import           Servant
import Servant.Docs
import qualified Log.Types


type Api = "entries" :> QueryParam "token" Text :> ReqBody '[JSON] NewEntry :> Post '[JSON] Entry
      :<|> "entries" :> QueryParam "token" Text :> Capture "id" Int :>  ReqBody '[JSON] NewEntry :> Post '[JSON] Entry
      :<|> "entries" :> QueryParam "token" Text :> Get '[JSON] [Entry]

instance ToSample [Entry] [Entry] where
  toSample _ = Just [Entry 1 1 1 "apples" "groceries" (UTCTime (fromGregorian 2015 5 1) 0) 4.5 [1,2]]

instance ToSample Entry Entry where
  toSample _ = Just (Entry 1 1 1 "apples" "groceries" (UTCTime (fromGregorian 2015 5 1) 0) 4.5 [1,2])

instance ToSample NewEntry NewEntry where
  toSample _ = Just (Entry () 1 1 "apples" "groceries" (UTCTime (fromGregorian 2015 5 1) 0) 4.5 [1,2])

instance ToCapture (Capture "id" Int) where
  toCapture _ = DocCapture "id" "id of an entry"

server :: PG.Connection -> R.Connection -> Server Api
server pg r = postEntry pg r
         :<|> updateEntry pg r
         :<|> getEntries pg r
  where getEntries pg r (Just token) =
          do maid <- liftIO $ Account.Session.get r token
             case maid of
               Nothing -> return []
               Just account_id -> liftIO $ runQuery pg (getAccountEntries account_id)
        updateEntry pg r (Just token) i entry =
          do maid <- liftIO $ Account.Session.get r token
             case maid of
               Nothing -> left err404
               Just account_id ->
                 do [old :: Entry] <- liftIO $ runQuery pg (getEntry i)
                    liftIO $ runUpdate pg entryTable (const . conv $ entry)
                                                     ((.== pgInt4 i) . entryId)
                    liftIO $ runInsert
                               pg Log.Types.logTable
                               (Log.Types.conv (Log.Types.Log () account_id "edit"
                                               (Just (entryWho old))
                                               (mn (entryWho old) (entryWho entry))
                                               (Just (entryWhat old))
                                               (mn (entryWhat old) (entryWhat entry))
                                               (Just (entryCategory old))
                                               (mn (entryCategory old) (entryCategory entry))
                                               (Just (entryDate old))
                                               (mn (entryDate old) (entryDate entry))
                                               (Just (entryHowMuch old))
                                               (mn (entryHowMuch old) (entryHowMuch entry))
                                               (Just (entryWhoPays old))
                                               (mn (entryWhoPays old) (entryWhoPays entry))
                                               ))
                    return (entry { entryId = i })
                 where mn x y = if x == y
                                   then Nothing
                                   else Just y
        postEntry pg r (Just token) entry =
          do maid <- liftIO $ Account.Session.get r token
             case maid of
               Nothing -> left err401
               Just account_id ->
                 do [entry''] <- liftIO $ runInsertReturning pg entryTable (conv entry) id
                    liftIO $ runInsert
                               pg Log.Types.logTable
                               (Log.Types.conv (Log.Types.Log () account_id "add"
                                               Nothing
                                               (Just (entryWho entry))
                                               Nothing
                                               (Just (entryWhat entry))
                                               Nothing
                                               (Just (entryCategory entry))
                                               Nothing
                                               (Just (entryDate entry))
                                               Nothing
                                               (Just (entryHowMuch entry))
                                               Nothing
                                               (Just (entryWhoPays entry))
                                               ))
                    return entry''
