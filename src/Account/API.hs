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


type AccountApi = "accounts" :> ReqBody NewAccount :> Post Account
             :<|> "accounts" :> "session" :> "new"
                             :> Capture "name" Text
                             :> QueryParam "password" Text
                             :> Get Account.Session.Authentication
             :<|> "accounts" :> "session" :> "check"
                             :> QueryParam "token" Text
                             :> Get Bool
             :<|> "accounts" :> "session" :> "touch"
                             :> QueryParam "token" Text
                             :> Get Bool
             :<|> "accounts" :> Capture "name" Text :> Get Account
             :<|> "accounts" :> Get [Account]


server :: PG.Connection -> R.Connection -> Server AccountApi
server pg r = postAccount pg
             :<|> authenticate pg r
             :<|> checkSession r
             :<|> touchSession r
             :<|> getAccount pg
             :<|> getAccounts pg
  where touchSession r (Just token) = liftIO (Account.Session.touch r token)
        checkSession r (Just token) = liftIO (Account.Session.check r token)
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
                then do token <- liftIO (Account.Session.generate r (accountId account))
                        return $ Account.Session.Authed token
                else return Account.Session.NotAuthed
        getAccount pg name = liftIO $ one pg name
        getAccounts pg = liftIO $ runQuery pg accountQuery
