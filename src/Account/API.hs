{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric #-}

module Account.API where

import Control.Monad (liftM)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B
import Control.Monad.IO.Class (liftIO)
import Opaleye (runQuery, runInsertReturning, pgStrictText, pgStrictByteString)
import Data.Profunctor.Product.Default (def)
import GHC.Int (Int64)
import GHC.Generics
import System.Random (randomRIO)
import Data.Aeson (ToJSON)

import qualified Crypto.Hash.SHA512 as SHA512

import Database.PostgreSQL.Simple

import Servant
import Account.Types


data Authentication = Authed Text
                    | NotAuthed deriving Generic

instance ToJSON Authentication

type AccountApi = "accounts" :> ReqBody NewAccount :> Post Account
             :<|> "accounts" :> "authenticate"
                             :> Capture "name" Text
                             :> QueryParam "password" Text
                             :> Get Authentication
             :<|> "accounts" :> Capture "name" Text :> Get Account
             :<|> "accounts" :> Get [Account]

server :: Connection -> Server AccountApi
server conn = postAccount
         :<|> authenticate
         :<|> getAccount
         :<|> getAccounts
  where genSalt :: IO B.ByteString
        genSalt = do chars <- sequence (take 64 (repeat (randomRIO (0,64))))
                     return $ B.pack chars
        hashPassword :: Text -> B.ByteString -> B.ByteString
        hashPassword cleartext salt =
          let a = B.append salt (TE.encodeUtf8 cleartext)
          in (iterate SHA512.hash a) !! 512
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
        one :: Text -> IO Account
        one name = liftM head $ runQuery conn (getAccountQuery name)
        postAccount account =
          do account' <- liftIO $ conv account
             [account''] <- liftIO $ runInsertReturning conn accountTable account' id
             return account''
        authenticate name (Just password) =
          do account <- liftIO $ one name
             let authenticated =
                   accountPassword account == hashPassword password (accountSalt account)
             return $ if authenticated
                         then Authed "111"
                         else NotAuthed
        getAccount name     = liftIO $ one name
        getAccounts         = liftIO $ runQuery conn accountQuery
