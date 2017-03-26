{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib
    ( startApp
    , app
    ) where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import GHC.Generics
import Servant
import Servant.JS
import System.FilePath
import DbConnect

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API = "users" :> Get '[JSON] [User]

-- * A simple Counter data type
newtype Counter = Counter { value :: Int }
  deriving (Generic, Show, Num)

instance ToJSON Counter
type TestApi = "counter" :> Post '[JSON] Counter -- endpoint for increasing the counter
          :<|> "counter" :> Get  '[JSON] Counter -- endpoint to get the current value

type TestApi' = TestApi -- The API we want a JS handler for
           :<|> Raw     -- used for serving static files

-- this proxy only targets the proper endpoints of our API,
-- not the static file serving bit
testApi :: Proxy TestApi
testApi = Proxy

-- where our static files reside
www :: FilePath
www = "/Users/v0d1ch/code/serviette/"

startApp :: IO ()
startApp = do
  -- writeJSForAPI testApi jquery (www </> "api.js")
  executeMigrations
  getAllPersons
  run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return users

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]

