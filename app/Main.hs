{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Network.Wai.Middleware.Cors
import Web.Scotty

import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)

data Member = Member
  { id :: Int
  , name :: String
  , email :: String
  } deriving (Show, Generic)

instance ToJSON Member

instance FromJSON Member

main :: IO ()
main = do
  scotty 9000 $ do
    middleware simpleCors
    get "/hello/:name" $ do
      name <- param "name"
      html $ mconcat [ "<h1>Hello ", name, " from Scotty!</h1><hr/>"]
