{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module App where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Proxy
import Data.Text
import GHC.Generics
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.API()
import Servant.Client
import System.IO

-- * api

type ItemApi =
  "item" :> Get '[JSON] [Item]
    :<|> "item" :> Capture "itemId" Integer :> Get '[JSON] Item

itemApi :: Proxy ItemApi
itemApi = Proxy

-- * app

run :: IO ()
run = do
  let port = 3000
      settings =
        setPort port
          $ setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port))
          $ defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ serve itemApi server

server :: Server ItemApi
server =
  getItems
    :<|> getItemById

getItems :: Handler [Item]
getItems = liftIO $ executeTestAPI

getItemById :: Integer -> Handler Item
getItemById = \case
  0 -> return exampleItem
  _ -> throwError err404

exampleItem :: Item
exampleItem = Item 0 "example item"

-- * item

data Item = Item
  { itemId :: Integer,
    itemText :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Item

instance FromJSON Item

data a + b = Foo a b

type X = Int + Bool

-- * call api

type TestResponse = Text

data TestApiResponse = TestApiResponse
  { hello :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON TestApiResponse

instance ToJSON TestApiResponse

type TestCallAPI =
  "/" :> Post '[JSON] TestApiResponse

testCallAPI :: Proxy TestCallAPI
testCallAPI = Proxy

getRes :: ClientM TestApiResponse
getRes = client testCallAPI

executeTestAPI :: IO [Item]
executeTestAPI = do
  manager' <- newManager defaultManagerSettings
  res <- runClientM getRes (mkClientEnv manager' (BaseUrl Http "127.0.0.1" 8000 ""))
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right _ -> return ()
  return [exampleItem]
