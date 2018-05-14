{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Network.Wai.Middleware.Cors
import Web.Scotty
import Prelude hiding (id)

import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)

import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap

import Control.Concurrent (newMVar, readMVar, putMVar, takeMVar)
import Control.Monad.Trans.Class (lift)

data Member = Member
  { id :: Int
  , name :: String
  , email :: String
  } deriving (Show, Generic)

instance ToJSON Member

instance FromJSON Member

main :: IO ()
main = do
  membersRef <- newMVar $ IntMap.fromList [ (1, Member 1 "Kurt" "kurt@email.com")
                                          , (2, Member 2 "Sonja" "sonja@post.dk")
                                          ]
  scotty 9000 $ do
    middleware simpleCors
    get "/hello/:name" $ do
      name <- param "name"
      html $ mconcat [ "<h1>Hello ", name, " from Scotty!</h1><hr/>"]
    get "/member/count" $  do
      members <- lift $ readMVar membersRef
      json (IntMap.size members)

insertMember :: Member -> IntMap Member -> (Member, IntMap Member)
insertMember member intMap =
  if IntMap.member (id member) intMap then
    (member, IntMap.insert (id member) member intMap)
  else
    let
      m = Member ((IntMap.size intMap) + 1) (name member) (email member)
    in
      (m, IntMap.insert (id m) m intMap)
