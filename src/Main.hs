module Main where

import qualified Account.API
import qualified Entry.API
import qualified Person.API

import Data.Text (Text)
import qualified Data.Text as T
import           Data.Proxy                 (Proxy (..))
import           Database.PostgreSQL.Simple (connectPostgreSQL)
import           Database.Redis             (connect, defaultConnectInfo)
import           Network.Wai.Handler.Warp   (run)
import           Servant
import Servant.Docs
import           Servant.Utils.StaticFiles

type AppApi = "api" :> Account.API.Api
      :<|> "api" :> Entry.API.Api
      :<|> "api" :> Person.API.Api
      :<|> Raw

appApi :: Proxy AppApi
appApi = Proxy

type Api = "api" :> "docs" :> Get Text
      :<|> AppApi

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
  run 8000 (serve api $ (return $ T.pack $ markdown (docs appApi))
                   :<|> (Account.API.server pg redis)
                   :<|> (Entry.API.server pg redis)
                   :<|> (Person.API.server pg redis)
                   :<|> serveDirectory "static"
                   )
