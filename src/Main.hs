module Main where

import qualified Account.API
import qualified Entry.API
import qualified Person.API
import qualified Log.API
import qualified Lib
import qualified Entry.Types
import qualified Person.Types
import qualified Account.Session

import Data.Time.Clock
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import           Data.Proxy                 (Proxy (..))
import           Database.PostgreSQL.Simple (connectPostgreSQL)
import qualified Database.PostgreSQL.Simple as PG
import           Database.Redis             (connect, defaultConnectInfo)
import qualified Database.Redis as R
import           Network.Wai.Handler.Warp   (run)
import           Servant
import           Servant.Docs
import           Servant.Utils.StaticFiles
import Opaleye
import Data.Aeson

type Api = "api" :> Account.API.Api
      :<|> "api" :> Entry.API.Api
      :<|> "api" :> Person.API.Api
      :<|> "api" :> Log.API.Api
      :<|> "api" :> ResultApi
      :<|> "api" :> "docs" :> Get '[JSON] Text
      :<|> Raw

instance ToSample Text Text where
  toSample _ = Just "Text according to endpoint."

api :: Proxy Api
api = Proxy

type ResultApi = "result" :> QueryParam "token" Text :> Get '[JSON] Lib.Result
instance ToJSON Lib.Result
instance ToSample Lib.Result Lib.Result where
  toSample _ = Just (Lib.Result [(Person.Types.Person 1 1 "Jane" 50.0, 100.0, 20.0)
                                ,(Person.Types.Person 1 1 "John" 50.0, 140.0, -20.0)]
                                (2015, 5, 1))

result :: PG.Connection -> R.Connection -> Server ResultApi
result pg r (Just token) =
  do Just account_id <- liftIO $ Account.Session.get r token
     entries <- liftIO $ runQuery pg (Entry.Types.getAccountEntries account_id)
     now <- liftIO getCurrentTime
     persons <- liftIO $ runQuery pg (Person.Types.getAccountPersons now account_id)
     shares <- liftIO $ mapM (\p -> runQuery pg (Person.Types.getPersonShares
                                                        (Person.Types.personId p)))
                             persons
     return $ Lib.run (zip persons shares) entries

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
                   :<|> (Person.API.server pg redis)
                   :<|> (Log.API.server pg redis)
                   :<|> (result pg redis)
                   :<|> (return $ T.pack $ markdown (docs api))
                   :<|>  serveDirectory "static"
                   )
