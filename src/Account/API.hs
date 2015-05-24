module Account.API where

import Control.Applicative
import           Control.Monad                   (liftM)
import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Control.Monad.Trans.Either
import           Data.Aeson                      (ToJSON)
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString                 as B
import           Data.Maybe                      (listToMaybe)
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
import           Account.Types
import           Servant
import Servant.Docs


newtype Success = Success Bool deriving Generic
instance ToJSON Success

type Api = "accounts" :> ReqBody '[JSON] NewAccount :> Post '[JSON] Account
      :<|> "accounts" :> "session" :> "new"
                      :> QueryParam "name" Text
                      :> QueryParam "password" Text
                      :> Get '[JSON] Account.Session.Authentication
      :<|> "accounts" :> "session" :> "check"
                      :> QueryParam "token" Text
                      :> Get '[JSON] Success
      :<|> "accounts" :> "session" :> "touch"
                      :> QueryParam "token" Text
                      :> Get '[JSON] Success
      :<|> "accounts" :> "session" :> "delete"
                      :> QueryParam "token" Text
                      :> Get '[JSON] Success


instance ToParam (QueryParam "name" Text) where
  toParam _ =
    DocQueryParam "name"
                  []
                  "The name of an account."
                  Normal

instance ToParam (QueryParam "password" Text) where
  toParam _ =
    DocQueryParam "password"
                  []
                  "The password of an account."
                  Normal

instance ToParam (QueryParam "token" Text) where
  toParam _ =
    DocQueryParam "token"
                  []
                  "A session token retrieved from the API, used for authenticating requests."
                  Normal

instance ToSample Success Success where
  toSamples _ = [("Operation succeeded.", Success True)
                ,("Operation failed.", Success False)]

instance ToSample Account Account where
  toSample _ = Just (Account 1 "ghouse" "emma@housetab.org" "hteo02h2th" "1111" False True)

instance ToSample NewAccount NewAccount where
  toSample _ = Just (Account () "ghouse" "emma@housetab.org" "password" () False True)

instance ToSample Account.Session.Authentication Account.Session.Authentication where
  toSamples _ = [("Successfully authenticated, contains the session token."
                 ,Account.Session.Authed "abcde")
                ,("Failed to authenticate.", Account.Session.NotAuthed)]

server :: PG.Connection -> R.Connection -> Server Api
server pg r = postAccount pg
             :<|> authenticate pg r
             :<|> checkSession r
             :<|> touchSession r
             :<|> deleteSession r
  where touchSession r (Just token) = liftIO (Success <$> Account.Session.touch r token)
        deleteSession r (Just token) = liftIO (Success <$> Account.Session.delete r token)
        checkSession r (Just token) = liftIO (Success <$> Account.Session.check r token)
        one :: PG.Connection -> Text -> IO (Maybe Account)
        one pg name = liftM listToMaybe $ runQuery pg (getAccountQuery name)
        postAccount pg account =
          do account' <- liftIO $ conv account
             [account''] <- liftIO $ runInsertReturning pg accountTable account' id
             return account''
        authenticate pg r (Just name) (Just password) =
          do maccount <- liftIO $ one pg name
             case maccount of
               Nothing -> return Account.Session.NotAuthed
               Just account -> do
                 let authenticated =
                       accountPassword account == hashPassword password (accountSalt account)
                 if authenticated
                    then do token <- liftIO (Account.Session.generate r (accountId account))
                            return $ Account.Session.Authed token
                    else return Account.Session.NotAuthed
