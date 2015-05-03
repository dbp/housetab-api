module Account.Types where

import           GHC.Generics
import           Prelude                    hiding (Sum)

import           Control.Arrow              (returnA)
import qualified Crypto.Hash.SHA512         as SHA512
import           Data.Aeson                 (FromJSON (..), ToJSON (..))
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Base64     as B64
import           Data.ByteString.Internal   (c2w)
import           Data.Profunctor.Product    (p2, p3)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Text                  (Text)
import qualified Data.Text.Encoding         as TE
import           Opaleye
import           System.Random              (randomRIO)

data Account' a b c d e f g = Account { accountId             :: a
                                      , accountName           :: b
                                      , accountEmail          :: c
                                      , accountPassword       :: d
                                      , accountSalt           :: e
                                      , accountTutorialActive :: f
                                      , accountRecordHistory  :: g
                                      }
type Account = Account' Int Text Text ByteString ByteString Bool Bool
type NewAccount = Account' () Text Text Text () Bool Bool

type AccountColumn = Account' (Column PGInt4)
                              (Column PGText)
                              (Column PGText)
                              (Column PGBytea)
                              (Column PGBytea)
                              (Column PGBool)
                              (Column PGBool)

type NewAccountColumn = Account' (Maybe (Column PGInt4))
                                 (Column PGText)
                                 (Column PGText)
                                 (Column PGBytea)
                                 (Column PGBytea)
                                 (Column PGBool)
                                 (Column PGBool)

instance ToJSON ByteString where
    toJSON bs = toJSON (TE.decodeUtf8 $ B64.encode bs)

instance FromJSON ByteString where
    parseJSON o = parseJSON o >>= either fail return . B64.decode . TE.encodeUtf8


deriving instance Generic (Account' a b c d e f g)
instance ToJSON Account
instance FromJSON Account
instance ToJSON NewAccount
instance FromJSON NewAccount

$(makeAdaptorAndInstance "pAccount" ''Account')

genSalt :: IO ByteString
genSalt = do chars <- sequence (take 64 (repeat (randomRIO ('a','z'))))
             return $ B.pack (map c2w chars)

hashPassword :: Text -> ByteString -> ByteString
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
                   , accountTutorialActive = pgBool accountTutorialActive
                   , accountRecordHistory = pgBool accountRecordHistory
                   }

accountTable :: Table NewAccountColumn AccountColumn
accountTable =
  Table "accounts" (pAccount Account { accountId = optional "id"
                                     , accountName = required "name"
                                     , accountEmail = required "email"
                                     , accountPassword = required "password"
                                     , accountSalt = required "salt"
                                     , accountTutorialActive = required "tutorial_active"
                                     , accountRecordHistory = required "record_history"
                                     })

accountQuery :: Query AccountColumn
accountQuery = queryTable accountTable

getAccountQuery :: Text -> Query AccountColumn
getAccountQuery name = proc () ->
   do account <- accountQuery -< ()
      restrict -< accountName account .== pgStrictText name
      returnA -< account
