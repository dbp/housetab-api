module Person.Types where

import           GHC.Generics
import           Prelude                    hiding (Sum, max)

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

data Person' a b c d = Person { personId        :: a
                              , personAccountId :: b
                              , personName      :: c
                              , personCurrentShare :: d
                              }

type Person = Person' Int Int Text Double
type NewPerson = Person' () Int Text ()

type PersonColumn = Person' (Column PGInt4)
                            (Column PGInt4)
                            (Column PGText)
                            (Column PGFloat8)

type PersonColumn' = (Column PGInt4, Column PGInt4, Column PGText)

type NewPersonColumn = (Maybe (Column PGInt4), Column PGInt4, Column PGText)

instance ToJSON ByteString where
    toJSON bs = toJSON (TE.decodeUtf8 $ B64.encode bs)

instance FromJSON ByteString where
    parseJSON o = parseJSON o >>= either fail return . B64.decode . TE.encodeUtf8


deriving instance Generic (Person' a b c d)
instance ToJSON Person
instance FromJSON Person
instance ToJSON NewPerson
instance FromJSON NewPerson

$(makeAdaptorAndInstance "pPerson" ''Person')

data Share' a b c d = Share { shareId :: a
                            , sharePersonId :: b
                            , shareStart :: c
                            , shareValue :: d
                            }

type Share = Share' Int Int UTCTime Double
type ShareColumn = Share' (Column PGInt4)
                          (Column PGInt4)
                          (Column PGTimestamptz)
                          (Column PGFloat8)

deriving instance Generic (Share' a b c d)
instance ToJSON Share
instance FromJSON Share

$(makeAdaptorAndInstance "pShare" ''Share')

-- NOTE(dbp 2015-05-02): This function is boilerplate, hopefully removable at some point.
conv :: NewPerson -> IO NewPersonColumn
conv (Person {..}) = do
  return $ (Nothing, pgInt4 personAccountId, pgStrictText personName)

personTable :: Table NewPersonColumn PersonColumn'
personTable = Table "persons" $ p3 (optional "id", required "account_id",required "name")

shareTable :: Table ShareColumn ShareColumn
shareTable = Table "shares" (pShare $ Share (required "id")
                                            (required "person_id")
                                            (required "start")
                                            (required "value"))


currentSharesQuery :: UTCTime -> Query (Column PGInt4, Column PGFloat8, Column PGTimestamptz)
currentSharesQuery now = aggregate (p3 (groupBy, groupBy, max)) $
                         orderBy (desc (\(_,_,a) -> a)) $ proc () ->
  do share <- queryTable shareTable -< ()
     restrict -< shareStart share .< pgUTCTime now
     returnA -< (sharePersonId share, shareValue share, shareStart share)

personQuery :: UTCTime -> Query PersonColumn
personQuery now = orderBy (desc personName) $ proc () ->
   do (id, account, name) <- queryTable personTable -< ()
      (sperson, svalue, _) <- currentSharesQuery now -< ()
      restrict -< sperson .== id
      returnA -< Person id account name svalue


getAccountPersons :: UTCTime -> Int -> Query PersonColumn
getAccountPersons now account_id = proc () ->
   do person <- personQuery now -< ()
      restrict -< personAccountId person .== pgInt4 account_id
      returnA -< person
