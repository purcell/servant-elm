{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import           Control.Monad.Trans.Either
import           Data.Aeson
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Servant
import           Servant.Elm

type UserAPI = "users" :> Get '[JSON] [User]
          :<|> "discovery" :> Capture "id" Int :> Get '[JSON] Discovery

data User = User
  { name :: String
  , age  :: Int
  } deriving (Eq, Show, Generic)

data Discovery = Discovery
  { details :: String
  , year    :: Int
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
app = simpleCors $ serve userAPI server

main :: IO ()
main = do
  putStrLn $ elmForAPI userAPI
  run 8081 app
