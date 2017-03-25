{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module DbConnect (mysqlConnect) where 

import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Logger    (runStderrLoggingT)
import           Database.Persist
import           Database.Persist.MySQL
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    age Int Maybe
    address Int
    deriving Show
BlogPost
    title String
    authorId PersonId
    deriving Show
|]

connectionInfo = defaultConnectInfo { connectPort = 3306,
                                      connectPassword = "secret",
                                      connectUser = "homestead",
                                      connectDatabase = "bsccrm"}

mysqlConnect :: IO ()
mysqlConnect = runStderrLoggingT $ withMySQLPool connectionInfo 10 $ \pool -> liftIO $ do
         flip runSqlPersistMPool pool $ do
           printMigration migrateAll
           -- runMigration migrateAll
