{-# LANGUAGE OverloadedStrings #-}

module Main where

import Account.Types
import Account.API

import Database.PostgreSQL.Simple (connectPostgreSQL)
import Servant (serve)
import Network.Wai.Handler.Warp (run)
import Data.Proxy (Proxy(..))

accountApi :: Proxy AccountApi
accountApi = Proxy

main :: IO ()
main = do
  conn <- connectPostgreSQL "host=localhost user=housetab_user dbname=housetab_devel password=111"
  run 8080 (serve accountApi $ server conn)
