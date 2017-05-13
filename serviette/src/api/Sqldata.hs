{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module Api.Sqldata where

import           Control.Monad.Except
import           Control.Monad.Reader        (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Data.Int                    (Int64)
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert,
                                              selectFirst, selectList, (==.))
import           Network.Wai                 (Application)
import           Servant

import           Config                      (App (..), Config (..))
import           Data.Aeson
import           GHC.Generics
import           Models


data SelectTable = SelectTable String deriving (Show, Generic)
data Fields = Fields [String]  deriving (Show, Generic)
data JoinTable = JoinTable
  { tablename :: String
  } deriving (Show, Generic)


data SqlData = SqlData{
      selectTable :: SelectTable
    , joinTable   :: [JoinTable]
  } deriving (Show, Generic)

instance FromJSON SelectTable
instance ToJSON SelectTable

instance FromJSON Fields
instance ToJSON Fields

instance FromJSON JoinTable
instance ToJSON JoinTable

instance FromJSON SqlData
instance ToJSON SqlData

type SqlAPI = "sql" :> Get '[JSON] [SqlData]

-- | The server that runs the SqlAPI
sqlServer :: ServerT SqlAPI App
sqlServer = allJson

allJson :: App [SqlData]
allJson = undefined
