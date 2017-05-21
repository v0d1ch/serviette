module Handler.Home where
import           Data.Aeson      (object, (.=))
import qualified Data.Aeson      as J
import           Data.Text       (pack)
import           Import
import           Yesod.Core.Json (returnJson)

getHomeR :: Handler Value
getHomeR = do
  return $ String "serviette"


