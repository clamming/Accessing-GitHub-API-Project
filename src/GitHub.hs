{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DuplicateRecordFields     #-}

module GitHub where 

import           Control.Monad       (mzero)
import           Data.Aeson
import           Data.Proxy
import           Data.Text
import           GHC.Generics
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import           Servant.API
import           Servant.Client

type Username = Text
type UserAgent = Text
type Reponame = Text

data GitHubUser =
  GitHubUser { login :: Text
              , followers :: Integer
              , following :: Integer
             } deriving (Generic, FromJSON, Show)

data GitHubRepo =
   GitHubRepo { name :: Text
              , id :: Maybe Integer
              , language :: Maybe Text
              } deriving (Generic, FromJSON, Show)

data RepoLanguages =
   RepoLanguages   { java :: Maybe Integer
                   , python :: Maybe Integer
                   , haskell :: Maybe Integer
                   } deriving (Generic, FromJSON, Show)

type GitHubAPI = "users" :> Header "user-agent" UserAgent 
                         :> Capture "username" Username  :> Get '[JSON] GitHubUser
            :<|> "users" :> Header "user-agent" UserAgent 
                          :> Capture "username" Username  :> "repos" :>  Get '[JSON] [GitHubRepo]
            :<|> "repos" :> Header  "user-agent" UserAgent 
                          :> Capture "username" Username  
                          :> Capture "repo"     Reponame  :> "Languages" :>  Get '[JSON] [RepoLanguages]

gitHubAPI :: Proxy GitHubAPI
gitHubAPI = Proxy

getUser :: Maybe UserAgent -> Username -> ClientM GitHubUser
getUserRepos :: Maybe UserAgent -> Username -> ClientM [GitHubRepo]
getRepoLanguages :: Maybe UserAgent -> Username -> Reponame -> ClientM [RepoLanguages]

getUser :<|> getUserRepos :<|> getRepoLanguages = client gitHubAPI