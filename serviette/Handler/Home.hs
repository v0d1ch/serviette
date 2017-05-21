module Handler.Home where

import qualified Data.Aeson as J
import           Import

getHomeR :: Handler Value
getHomeR = do
  return $ J.String "serviette"


