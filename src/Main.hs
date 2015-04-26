{-# LANGUAGE OverloadedStrings #-}

module Main where

import Account.Types
import Account.API

import Database.PostgreSQL.Simple (connectPostgreSQL)
import Servant (serve)
import Network.Wai.Handler.Warp (run)
import Data.Proxy (Proxy(..))
import Database.Redis (connect, defaultConnectInfo)

accountApi :: Proxy AccountApi
accountApi = Proxy

main :: IO ()
main = do
  pg <- connectPostgreSQL "host=localhost \
                          \user=housetab_user \
                          \dbname=housetab_devel \
                          \password=111"
  redis <- connect defaultConnectInfo
  putStrLn "Listening on port 8000..."
  run 8000 (serve accountApi $ server pg redis)
