module Entry.Types where

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

data Entry' a b c d e f g = Entry { entryId        :: a
                                  , entryAccountId :: b
                                  , entryWho       :: c
                                  , entryWhat      :: d
                                  , entryCategory  :: e
                                  , entryDate      :: f
                                  , entryHowMuch   :: g
                                  }

type Entry = Entry' Int Int Int Text Text UTCTime Double
type NewEntry = Entry' () Int Int Text Text UTCTime Double

type EntryColumn = Entry' (Column PGInt4)
                          (Column PGInt4)
                          (Column PGInt4)
                          (Column PGText)
                          (Column PGText)
                          (Column PGTimestamptz)
                          (Column PGFloat8)

type NewEntryColumn = Entry' (Maybe (Column PGInt4))
                             (Column PGInt4)
                             (Column PGInt4)
                             (Column PGText)
                             (Column PGText)
                             (Column PGTimestamptz)
                             (Column PGFloat8)

instance ToJSON ByteString where
    toJSON bs = toJSON (TE.decodeUtf8 $ B64.encode bs)

instance FromJSON ByteString where
    parseJSON o = parseJSON o >>= either fail return . B64.decode . TE.encodeUtf8


deriving instance Generic (Entry' a b c d e f g)
instance ToJSON Entry
instance FromJSON Entry
instance ToJSON NewEntry
instance FromJSON NewEntry

$(makeAdaptorAndInstance "pEntry" ''Entry')

-- NOTE(dbp 2015-05-02): This function is boilerplate, hopefully removable at some point.
conv :: NewEntry -> IO NewEntryColumn
conv (Entry {..}) = do
  return $ Entry { entryId = Nothing
                 , entryAccountId = pgInt4 entryAccountId
                 , entryWho       = pgInt4 entryWho
                 , entryWhat      = pgStrictText entryWhat
                 , entryCategory  = pgStrictText entryCategory
                 , entryDate      = pgUTCTime entryDate
                 , entryHowMuch   = pgDouble entryHowMuch
                 }

entryTable :: Table NewEntryColumn EntryColumn
entryTable = Table "entries" (pEntry Entry { entryId = optional "id"
                                           , entryAccountId = required "account_id"
                                           , entryWho       = required "who"
                                           , entryWhat      = required "what"
                                           , entryCategory  = required "category"
                                           , entryDate      = required "date"
                                           , entryHowMuch   = required "howmuch"
                                           })

entryQuery :: Query EntryColumn
entryQuery = orderBy (desc entryDate) $ queryTable entryTable

getAccountEntries :: Int -> Query EntryColumn
getAccountEntries account_id = proc () ->
   do entry <- entryQuery -< ()
      restrict -< entryAccountId entry .== pgInt4 account_id
      returnA -< entry
