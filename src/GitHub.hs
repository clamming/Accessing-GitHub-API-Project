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

-- user data types sourced from GitHub user documentation at https://docs.github.com/en/free-pro-team@latest/rest/reference/users
data GitHubUser =
  GitHubUser { login :: Text
              , followers :: Integer
              , following :: Integer
             } deriving (Generic, FromJSON, Show)

-- data types for the repo are sourced from GitHub documentation at https://docs.github.com/en/free-pro-team@latest/rest/reference/repos#list-public-repositories 
data GitHubRepo =
   GitHubRepo { name :: Text
              , visibility :: Maybe Text
              , language :: Maybe Text
              } deriving (Generic, FromJSON, Show)

-- data types for the repo languages are sourced from https://docs.github.com/en/free-pro-team@latest/rest/reference/repos#list-repository-languages
-- data RepoLanguages =
 --  RepoLanguages   { java :: Maybe Integer
   --                , python :: Maybe Integer
     --              , haskell :: Maybe Integer
       --            } deriving (Generic, FromJSON, Show)

data RepoContributor =
  RepoContributor { login :: Text
                  , contributions :: Integer
                  } deriving (Generic, FromJSON, Show)

type GitHubAPI = "users" :> Header "user-agent" UserAgent
                         :> BasicAuth "github" Int
                         :> Capture "username" Username  :> Get '[JSON] GitHubUser
            :<|> "users" :> Header "user-agent" UserAgent
                         :> BasicAuth "github" Int
                          :> Capture "username" Username  :> "repos" :>  Get '[JSON] [GitHubRepo]
       
            :<|> "repos" :> Header  "user-agent" UserAgent
                         :> BasicAuth "github" Int
                         :> Capture "username" Username  
                         :> Capture "repo"     Reponame  :> "contributors" :>  Get '[JSON] [RepoContributor]

gitHubAPI :: Proxy GitHubAPI
gitHubAPI = Proxy

getUser ::          Maybe UserAgent -> BasicAuthData -> Username            -> ClientM GitHubUser
getUserRepos ::     Maybe UserAgent -> BasicAuthData -> Username            -> ClientM [GitHubRepo]
getRepoContribs ::  Maybe UserAgent -> BasicAuthData -> Username -> Reponame -> ClientM [RepoContributor]

getUser :<|> getUserRepos :<|> getRepoContribs = client gitHubAPI