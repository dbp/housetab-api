module Account.Session where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (ToJSON)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import qualified Database.Redis         as R
import           GHC.Generics

import           Account.Types          (genSalt)

data Authentication = Authed Int Text
                    | NotAuthed deriving Generic

instance ToJSON Authentication

sessionSeconds :: Integer
sessionSeconds = 60*60*24

key :: Text -> ByteString
key token = B.append "housetab:session:" (TE.encodeUtf8 token)

generate :: R.Connection -> Int -> IO Text
generate r accountId =
  do token <- genSalt
     R.runRedis r (R.setex (key (TE.decodeUtf8 token))
                           sessionSeconds
                           (TE.encodeUtf8 (T.pack (show accountId))))
     return (TE.decodeUtf8 token)

touch :: R.Connection -> Text -> IO Bool
touch r token = do r <- liftIO $ R.runRedis r (R.expire (key token) sessionSeconds)
                   case r of
                     Left _ -> return False
                     Right _ -> return True

delete :: R.Connection -> Text -> IO Bool
delete r token = do r <- liftIO $ R.runRedis r (R.del [key token])
                    case r of
                      Left _ -> return False
                      Right _ -> return True

check :: R.Connection -> Text -> IO Bool
check r token = do ex <- liftIO $ R.runRedis r (R.exists (key token))
                   case ex of
                     Right True -> return True
                     _ -> return False

get :: R.Connection -> Text -> IO (Maybe Int)
get r token = do r <- liftIO $ R.runRedis r (R.get (key token))
                 case r of
                   Right (Just i) -> return (Just $ read . T.unpack . TE.decodeUtf8 $ i)
                   _ -> return Nothing
