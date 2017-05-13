{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module Api.User where

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

type UserAPI =
         "users" :> Get '[JSON] [Entity User]
    :<|> "users" :> Capture "name" String :> Get '[JSON] (Entity User)

-- | The server that runs the UserAPI
userServer :: ServerT UserAPI App
userServer = allUsers :<|> singleUser

-- | Returns all users in the database.
allUsers :: App [Entity User]
allUsers =
    runDb (selectList [] [])

-- | Returns a user by name or throws a 404 error.
singleUser :: [Char] -> App (Entity User)
singleUser str = do
    maybeUser <- runDb (selectFirst [UserName ==. str] [])
    case maybeUser of
         Nothing ->
            throwError err404
         Just person ->
            return person

