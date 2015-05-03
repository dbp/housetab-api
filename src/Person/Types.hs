module Person.Types where

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
import           Data.Time.Clock
import           Opaleye
import           System.Random              (randomRIO)

data Person' a b c = Person { personId        :: a
                            , personAccountId :: b
                            , personName      :: c
                            }

type Person = Person' Int Int Text
type NewPerson = Person' () Int Text

type PersonColumn = Person' (Column PGInt4)
                            (Column PGInt4)
                            (Column PGText)

type NewPersonColumn = Person' (Maybe (Column PGInt4))
                               (Column PGInt4)
                               (Column PGText)

instance ToJSON ByteString where
    toJSON bs = toJSON (TE.decodeUtf8 $ B64.encode bs)

instance FromJSON ByteString where
    parseJSON o = parseJSON o >>= either fail return . B64.decode . TE.encodeUtf8


deriving instance Generic (Person' a b c)
instance ToJSON Person
instance FromJSON Person
instance ToJSON NewPerson
instance FromJSON NewPerson

$(makeAdaptorAndInstance "pPerson" ''Person')

-- NOTE(dbp 2015-05-02): This function is boilerplate, hopefully removable at some point.
conv :: NewPerson -> IO NewPersonColumn
conv (Person {..}) = do
  return $ Person { personId = Nothing
                   , personAccountId = pgInt4 personAccountId
                   , personName      = pgStrictText personName
                 }

personTable :: Table NewPersonColumn PersonColumn
personTable = Table "persons" (pPerson Person { personId        = optional "id"
                                              , personAccountId = required "account_id"
                                              , personName      = required "name"
                                              })

personQuery :: Query PersonColumn
personQuery = orderBy (desc personName) $ queryTable personTable

getAccountPersons :: Int -> Query PersonColumn
getAccountPersons account_id = proc () ->
   do person <- personQuery -< ()
      restrict -< personAccountId person .== pgInt4 account_id
      returnA -< person
