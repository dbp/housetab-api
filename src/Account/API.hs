{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Account.API where

import Control.Monad (liftM)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B
import Control.Monad.IO.Class (liftIO)
import Opaleye (runQuery, runInsert, pgStrictText, pgStrictByteString)
import Data.Profunctor.Product.Default (def)
import GHC.Int (Int64)

import qualified Crypto.Hash.SHA512 as SHA512

import Database.PostgreSQL.Simple

import Servant
import Account.Types


type AccountApi = "accounts" :> ReqBody Account :> Post Account
             :<|> "accounts" :> "authenticate" :> Capture "name" Text :> QueryParam "password" Text :> Get Bool
             :<|> "accounts" :> Capture "name" Text :> Get Account
             :<|> "accounts" :> Get [Account]

server :: Connection -> Server AccountApi
server conn = postAccount
         :<|> authenticate
         :<|> getAccount
         :<|> getAccounts
  where conv :: Account -> NewAccountColumn
        conv (Account {..}) = Account { accountId = Nothing
                                      , accountName = pgStrictText accountName
                                      , accountEmail = pgStrictText accountEmail
                                      , accountPassword = pgStrictByteString accountPassword
                                      , accountSalt = pgStrictByteString accountSalt
                                      }
        insert :: Account -> IO Int64
        insert account = runInsert conn accountTable (conv account)
        all :: IO [Account]
        all = runQuery conn accountQuery
        one :: Text -> IO Account
        one name = liftM head $ runQuery conn (getAccountQuery name)
        postAccount account = liftIO $ insert account >> return account
        authenticate name (Just password) =
          do account <- liftIO $ one name
             let a = B.append (accountSalt account) (TE.encodeUtf8 password)
             -- liftIO $ B.putStrLn a
             return $ accountPassword account == (iterate SHA512.hash a) !! 512
        getAccount name     = liftIO $ one name
        getAccounts         = liftIO all
