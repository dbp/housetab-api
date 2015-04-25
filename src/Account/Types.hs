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
import Data.Aeson (FromJSON(..), ToJSON(..))

import Opaleye
import           Data.Profunctor.Product (p2, p3)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)


data Account' a b c = Account { accountId :: a, accountName :: b, accountEmail :: c }
type Account = Account' Int Text Text
type NewAccountColumn = Account' (Maybe (Column PGInt4)) (Column PGText) (Column PGText)
type AccountColumn = Account' (Column PGInt4) (Column PGText) (Column PGText)

deriving instance Generic (Account' a b c)
instance ToJSON Account
instance FromJSON Account

$(makeAdaptorAndInstance "pAccount" ''Account')

accountTable :: Table NewAccountColumn AccountColumn
accountTable = Table "accounts" (pAccount Account { accountId = optional "id"
                                                  , accountName = required "name"
                                                  , accountEmail = required "email" })

accountQuery :: Query AccountColumn
accountQuery = queryTable accountTable
