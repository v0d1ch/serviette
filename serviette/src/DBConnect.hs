{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module DbConnect (dbConnect) where

import Control.Monad.IO.Class  (liftIO)
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Control.Monad.Logger


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
        Person
            name String
            age Int Maybe
            deriving Show
        BlogPost
            title String
            authorId PersonId
            deriving Show
        |]


connStr = "host=localhost dbname=v0d1ch user=postgres password=root port=5432" 

dbConnect :: IO ()
dbConnect = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> do
       liftIO $ flip runSqlPersistMPool pool $ do
         printMigration migrateAll
         res  :: [Entity Person] <- selectList [] [LimitTo 1] 
         liftIO $ print res
            -- runMigration migrateAll

            -- johnId <- insert $ Person "John Doe" $ Just 35
            -- janeId <- insert $ Person "Jane Doe" Nothing

            -- insert $ BlogPost "My fr1st p0st" johnId
            -- insert $ BlogPost "One more for good measure" johnId

            -- oneJohnPost <- selectList [BlogPostAuthorId ==. johnId] [LimitTo 1]
            -- liftIO $ print (oneJohnPost :: [Entity BlogPost])

            -- john <- get johnId
            -- liftIO $ print (john :: Maybe Person)

            -- delete janeId
            -- deleteWhere [BlogPostAuthorId ==. johnId]
