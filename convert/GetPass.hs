{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Database.MongoDB as DB
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import Data.Bson
import System.Environment (getArgs)

main :: IO ()
main =
  do [account] <- getArgs
     pipe <- DB.connect (DB.host "127.0.0.1")
     Just e <- DB.access pipe DB.master "housetab"
                  (DB.findOne (DB.select ["accountName" DB.=: account] "users"))
     let Just (String pass) :: Maybe Value = DB.lookup "password" e
     print $ B.unpack (B8.pack (T.unpack pass))
