{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import qualified Account.API
import qualified Entry.API

import           Data.Proxy                 (Proxy (..))
import           Database.PostgreSQL.Simple (connectPostgreSQL)
import           Database.Redis             (connect, defaultConnectInfo)
import           Network.Wai.Handler.Warp   (run)
import           Servant
import           Servant.Utils.StaticFiles

type Api = "api" :> Account.API.Api
      :<|> "api" :> Entry.API.Api
      :<|> Raw

api :: Proxy Api
api = Proxy

main :: IO ()
main = do
  pg <- connectPostgreSQL "host=localhost \
                          \user=housetab_user \
                          \dbname=housetab_devel \
                          \password=111"
  redis <- connect defaultConnectInfo
  putStrLn "Listening on port 8000..."
  run 8000 (serve api $ (Account.API.server pg redis)
                   :<|> (Entry.API.server pg redis)
                   :<|> serveDirectory "static"
                   )
