{-# LANGUAGE OverloadedStrings #-}
module M20150422231121_base_schema where

import           Control.Monad
import           Rivet.Migration.V0

migrate = do
  createTable "accounts"
    [ColumnSpec "id" "serial" Nothing (Just "PRIMARY KEY")
    ,ColumnSpec "name" "text" Nothing (Just "NOT NULL")
    ,ColumnSpec "email" "text" Nothing (Just "NOT NULL UNIQUE")
    ,ColumnSpec "password" "bytea" Nothing (Just "NOT NULL")
    ,ColumnSpec "salt" "bytea" Nothing (Just "NOT NULL")
    ,ColumnSpec "tutorial_active" "boolean" Nothing (Just "NOT NULL")
    ,ColumnSpec "record_history" "boolean" Nothing (Just "NOT NULL")
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
    ,ColumnSpec "howmuch" "float8" Nothing (Just "NOT NULL")
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
    ,ColumnSpec "value" "float8" Nothing (Just "NOT NULL")
    ]
  createTable "log"
    [ColumnSpec "id" "serial" Nothing (Just "PRIMARY KEY")
    ,ColumnSpec "account_id" "integer" Nothing (Just "NOT NULL REFERENCES accounts(id)")
    ,ColumnSpec "type" "text" Nothing (Just "NOT NULL")
    ,ColumnSpec "who_old" "integer" Nothing (Just "REFERENCES persons(id)")
    ,ColumnSpec "who_new" "integer" Nothing (Just "REFERENCES persons(id)")
    ,ColumnSpec "what_old" "text" Nothing Nothing
    ,ColumnSpec "what_new" "text" Nothing Nothing
    ,ColumnSpec "category_old" "text" Nothing Nothing
    ,ColumnSpec "category_new" "text" Nothing Nothing
    ,ColumnSpec "date_old" "timestamptz" Nothing Nothing
    ,ColumnSpec "date_new" "timestamptz" Nothing Nothing
    ,ColumnSpec "howmuch_old" "float8" Nothing Nothing
    ,ColumnSpec "howmuch_new" "float8" Nothing Nothing
    ]
  createTable "log_whopays_old"
    [ColumnSpec "id" "serial" Nothing (Just "PRIMARY KEY")
    ,ColumnSpec "log_id" "integer" Nothing (Just "NOT NULL REFERENCES log(id)")
    ,ColumnSpec "person_id" "integer" Nothing (Just "NOT NULL REFERENCES persons(id)")
    ]
  createTable "log_whopays_new"
    [ColumnSpec "id" "serial" Nothing (Just "PRIMARY KEY")
    ,ColumnSpec "log_id" "integer" Nothing (Just "NOT NULL REFERENCES log(id)")
    ,ColumnSpec "person_id" "integer" Nothing (Just "NOT NULL REFERENCES persons(id)")
    ]
