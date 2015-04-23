{-# LANGUAGE OverloadedStrings #-}
module M20150422231121_base_schema where

import           Control.Monad
import           Rivet.Migration.V0

migrate = do
  createTable "accounts"
    [ColumnSpec "id" "serial" Nothing (Just "PRIMARY KEY")
    ,ColumnSpec "name" "text" Nothing (Just "NOT NULL")
    ,ColumnSpec "email" "text" Nothing (Just "NOT NULL UNIQUE")
    ]
  createTable "persons"
    [ColumnSpec "id" "serial" Nothing (Just "PRIMARY KEY")
    ,ColumnSpec "account_id" "integer" Nothing (Just "NOT NULL REFERENCES accounts(id)")
    ,ColumnSpec "name" "text" Nothing (Just "NOT NULL")
    ]
  createTable "entries"
    [ColumnSpec "id" "serial" Nothing (Just "PRIMARY KEY")
    ,ColumnSpec "account_id" "integer" Nothing (Just "NOT NULL REFERENCES accounts(id)")
    ,ColumnSpec "who" "integer" Nothing (Just "NOT NULL REFERENCES persons(id)")
    ,ColumnSpec "what" "text" Nothing (Just "NOT NULL")
    ,ColumnSpec "category" "text" Nothing (Just "NOT NULL")
    ,ColumnSpec "date" "timestamptz" Nothing (Just "NOT NULL")
    ,ColumnSpec "howmuch" "decimal(12,2)" Nothing (Just "NOT NULL")
    ]
  createTable "entries_whopays"
    [ColumnSpec "id" "serial" Nothing (Just "PRIMARY KEY")
    ,ColumnSpec "entry_id" "integer" Nothing (Just "NOT NULL REFERENCES entries(id)")
    ,ColumnSpec "person_id" "integer" Nothing (Just "NOT NULL REFERENCES persons(id)")
    ]
  createTable "shares"
    [ColumnSpec "id" "serial" Nothing (Just "PRIMARY KEY")
    ,ColumnSpec "person_id" "integer" Nothing (Just "NOT NULL REFERENCES persons(id)")
    ,ColumnSpec "start" "timestamptz" Nothing (Just "NOT NULL")
    ,ColumnSpec "value" "integer" Nothing (Just "NOT NULL")
    ]
