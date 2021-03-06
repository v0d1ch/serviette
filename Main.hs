module Main where

import Data.ApiDataTypes
import Data.Serviette
import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B

-- | THIS FILE IS HERE JUST FOR TESTING PURPOSES !!!

jsonFile :: FilePath
jsonFile = "test/select_data.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

main :: IO ()
main = do
 d <- (eitherDecode <$> getJSON) :: IO (Either String SqlQuery)
 case d of
  Left err -> putStrLn err
  Right ps -> print $ rawSqlStr ps
