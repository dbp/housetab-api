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


type Api = "accounts" :> ReqBody NewAccount :> Post Account
      :<|> "accounts" :> "session" :> "new"
                      :> QueryParam "name" Text
                      :> QueryParam "password" Text
                      :> Get Account.Session.Authentication
      :<|> "accounts" :> "session" :> "check"
                      :> QueryParam "token" Text
                      :> Get Bool
      :<|> "accounts" :> "session" :> "touch"
                      :> QueryParam "token" Text
                      :> Get Bool
      :<|> "accounts" :> "session" :> "delete"
                      :> QueryParam "token" Text
                      :> Get Bool


server :: PG.Connection -> R.Connection -> Server Api
server pg r = postAccount pg
             :<|> authenticate pg r
             :<|> checkSession r
             :<|> touchSession r
             :<|> deleteSession r
  where touchSession r (Just token) = liftIO (Account.Session.touch r token)
        deleteSession r (Just token) = liftIO (Account.Session.delete r token)
        checkSession r (Just token) = liftIO (Account.Session.check r token)
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
