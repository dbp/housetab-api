{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}

module Account.Types where

import Prelude hiding (Sum)
import GHC.Generics

import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.ByteString (ByteString)
import Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.ByteString.Base64 as B64

import Opaleye
import           Data.Profunctor.Product (p2, p3)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Control.Arrow (returnA)

data Account' a b c d e = Account { accountId :: a
                                  , accountName :: b
                                  , accountEmail :: c
                                  , accountPassword :: d
                                  , accountSalt :: e
                                  }
type Account = Account' Int Text Text ByteString ByteString
type NewAccountColumn = Account' (Maybe (Column PGInt4)) (Column PGText) (Column PGText) (Column PGBytea) (Column PGBytea)
type AccountColumn = Account' (Column PGInt4) (Column PGText) (Column PGText) (Column PGBytea) (Column PGBytea)

instance ToJSON ByteString where
    toJSON bs = toJSON (TE.decodeUtf8 $ B64.encode bs)

instance FromJSON ByteString where
    parseJSON o = parseJSON o >>= either fail return . B64.decode . TE.encodeUtf8


deriving instance Generic (Account' a b c d e)
instance ToJSON Account
instance FromJSON Account

$(makeAdaptorAndInstance "pAccount" ''Account')

accountTable :: Table NewAccountColumn AccountColumn
accountTable = Table "accounts" (pAccount Account { accountId = optional "id"
                                                  , accountName = required "name"
                                                  , accountEmail = required "email"
                                                  , accountPassword = required "password"
                                                  , accountSalt = required "salt"
                                                  })

accountQuery :: Query AccountColumn
accountQuery = queryTable accountTable

getAccountQuery :: Text -> Query AccountColumn
getAccountQuery name = proc () ->
   do account <- accountQuery -< ()
      restrict -< accountName account .== pgStrictText name
      returnA -< account
