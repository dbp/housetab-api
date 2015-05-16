module Person.API where

import Data.Time.Clock
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
import           Person.Types
import           Servant


type Api = "persons" :> QueryParam "token" Text :> Get [Person]

server :: PG.Connection -> R.Connection -> Server Api
server pg r = getPersons pg r
  where getPersons pg r (Just token) =
          do maid <- liftIO $ Account.Session.get r token
             case maid of
               Nothing -> return []
               Just account_id -> liftIO $ do now <- getCurrentTime
                                              runQuery pg (getAccountPersons now account_id)
