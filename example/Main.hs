{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import           Data.Aeson
import           Data.Time.Calendar
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Servant
import           Servant.Elm

import qualified Data.Text                   as T
import qualified Data.Text.IO                as T

type UserAPI = "users" :> Get '[JSON] [User]

data User = User
  { name :: String
  , age  :: Int
  } deriving (Eq, Show, Generic)

instance ToJSON User

users :: [User]
users =
  [ User "Isaac Newton"    372
  , User "Albert Einstein" 136
  ]

server :: Server UserAPI
server = return users

userAPI :: Proxy UserAPI
userAPI = Proxy

app :: Application
app = simpleCors $ serve userAPI server

main :: IO ()
main = do
  putStrLn "Some string"
  putStrLn $ elmForAPI userAPI
  run 8081 app
