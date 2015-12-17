{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Aeson
import Data.Time.Calendar
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Control.Monad.Trans.Either
import Servant
import Servant.Elm

import qualified Data.Text    as T
import qualified Data.Text.IO as T

type UserAPI = "users" :> Get '[JSON] [User]
          :<|> "discovery" :> Capture "id" Int :> Get '[JSON] Discovery

data User = User
  { name :: String
  , age :: Int
  } deriving (Eq, Show, Generic)

data Discovery = Discovery
  { details :: String
  , year :: Int
  } deriving (Eq, Show, Generic)

instance ToJSON User
instance ToJSON Discovery

users :: EitherT ServantErr IO [User]
users = return [ User "Isaac Newton"    372
  , User "Albert Einstein" 136
  ]

discovery :: Int -> EitherT ServantErr IO Discovery
discovery date = return (Discovery "The thing" date)

server :: Server UserAPI
server = users :<|> discovery

userAPI :: Proxy UserAPI
userAPI = Proxy

app :: Application
app = serve userAPI server

main :: IO ()
main = do
  putStrLn $ elmForAPI userAPI
  run 8081 app
