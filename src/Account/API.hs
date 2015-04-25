{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Account.API where

import Control.Monad.IO.Class (liftIO)
import Opaleye (runQuery, runInsert, pgStrictText)
import Data.Profunctor.Product.Default (def)
import GHC.Int (Int64)

import Database.PostgreSQL.Simple

import Servant
import Account.Types


type AccountApi = "accounts" :> ReqBody Account :> Post Account
             :<|> "accounts"  :> Get [Account]

server :: Connection -> Server AccountApi
server conn = postAccount
         :<|> getAccounts
  where conv :: Account -> NewAccountColumn
        conv (Account {..}) = Account { accountId = Nothing
                                      , accountName = pgStrictText accountName
                                      , accountEmail = pgStrictText accountEmail
                                      }
        insert :: Account -> IO Int64
        insert account = runInsert conn accountTable (conv account)
        all :: IO [Account]
        all = runQuery conn accountQuery
        postAccount account = liftIO $ insert account >> return account
        getAccounts         = liftIO all
