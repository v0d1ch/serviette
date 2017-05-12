{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module Api.Sql where

import           Control.Monad.Except
import           Control.Monad.Reader        (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Data.Int                    (Int64)
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert,
                                              selectFirst, selectList, (==.))
import           Network.Wai                 (Application)
import           Servant

import           Config                      (App (..), Config (..))
import           Models

data Sql = Sql

type SqlAPI = "sql" :> Get '[JSON] [Sql]

-- | The server that runs the SqlAPI
sqlServer :: ServerT SqlAPI App
sqlServer = allJson

-- | Returns all users in the database.
allJson :: App [Sql]
allJson = undefined

