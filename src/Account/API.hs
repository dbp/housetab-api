{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Account.API where

import           Control.Monad                   (liftM)
import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Control.Monad.Trans.Either
import           Data.Aeson                      (ToJSON)
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString                 as B
import           Data.ByteString.Internal        (c2w)
import           Data.Profunctor.Product.Default (def)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as TE
import           GHC.Generics
import           GHC.Int                         (Int64)
import           Opaleye                         (pgStrictByteString,
                                                  pgStrictText,
                                                  runInsertReturning, runQuery)
import           System.Random                   (randomRIO)

import qualified Crypto.Hash.SHA512              as SHA512

import qualified Database.PostgreSQL.Simple      as PG
import qualified Database.Redis                  as R

import           Account.Types
import           Servant


data Authentication = Authed Text
                    | NotAuthed deriving Generic

instance ToJSON Authentication

type AccountApi = "accounts" :> ReqBody NewAccount :> Post Account
             :<|> "accounts" :> "session" :> "new"
                             :> Capture "name" Text
                             :> QueryParam "password" Text
                             :> Get Authentication
             :<|> "accounts" :> "session" :> "check"
                             :> QueryParam "token" Text
                             :> Get Bool
             :<|> "accounts" :> "session" :> "touch"
                             :> QueryParam "token" Text
                             :> Get Bool
             :<|> "accounts" :> Capture "name" Text :> Get Account
             :<|> "accounts" :> Get [Account]

sessionSeconds :: Integer
sessionSeconds = 60*60*24

genSalt :: IO ByteString
genSalt = do chars <- sequence (take 64 (repeat (randomRIO ('a','z'))))
             return $ B.pack (map c2w chars)
hashPassword :: Text -> ByteString -> ByteString
hashPassword cleartext salt =
  let a = B.append salt (TE.encodeUtf8 cleartext)
  in (iterate SHA512.hash a) !! 512
sessionKey :: Text -> ByteString
sessionKey token = B.append "housetab:session:" (TE.encodeUtf8 token)

generateSessionToken :: R.Connection -> Int -> IO Text
generateSessionToken r accountId =
  do token <- genSalt
     R.runRedis r (R.setex (sessionKey (TE.decodeUtf8 token)) sessionSeconds (TE.encodeUtf8 (T.pack (show accountId))))
     return (TE.decodeUtf8 token)

touchSession r (Just token) =
   do r <- liftIO $ R.runRedis r (R.expire (sessionKey token) sessionSeconds)
      case r of
        Left _ -> return False
        Right _ -> return True

checkSession r (Just token) =
  do ex <- liftIO $ R.runRedis r (R.exists (sessionKey token))
     case ex of
       Right True -> return True
       _ -> return False

conv :: NewAccount -> IO NewAccountColumn
conv (Account {..}) = do
  salt <- genSalt
  let hash = hashPassword accountPassword salt
  return $ Account { accountId = Nothing
                   , accountName = pgStrictText accountName
                   , accountEmail = pgStrictText accountEmail
                   , accountPassword = pgStrictByteString hash
                   , accountSalt = pgStrictByteString salt
                   }
one :: PG.Connection -> Text -> IO Account
one pg name = liftM head $ runQuery pg (getAccountQuery name)

postAccount pg account =
  do account' <- liftIO $ conv account
     [account''] <- liftIO $ runInsertReturning pg accountTable account' id
     return account''

authenticate pg r name (Just password) =
  do account <- liftIO $ one pg name
     let authenticated =
           accountPassword account == hashPassword password (accountSalt account)
     if authenticated
        then do token <- liftIO (generateSessionToken r (accountId account))
                return $ Authed token
        else return NotAuthed

getAccount pg name = liftIO $ one pg name

getAccounts pg = liftIO $ runQuery pg accountQuery

server :: PG.Connection -> R.Connection -> Server AccountApi
server pg r = postAccount pg
             :<|> authenticate pg r
             :<|> checkSession r
             :<|> touchSession r
             :<|> getAccount pg
             :<|> getAccounts pg
