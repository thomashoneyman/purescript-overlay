module Bin.Env where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String as String
import Dotenv as Dotenv
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Lib.Foreign.Octokit (GitHubToken(..))
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS.Aff
import Node.Path (FilePath)
import Node.Process as Process

-- | Loads the environment from a .env file, if one exists.
loadEnvFile :: FilePath -> Aff Unit
loadEnvFile dotenv = do
  contents <- FS.Aff.readTextFile UTF8 dotenv
  Dotenv.loadContents (String.trim contents)

-- | An environment key
newtype EnvKey a = EnvKey { key :: String, decode :: String -> Either String a }

printEnvKey :: forall a. EnvKey a -> String
printEnvKey (EnvKey { key }) = key

-- | Look up an optional environment variable, throwing an exception if it is
-- | present but cannot be decoded. Empty strings are considered missing values.
lookupOptional :: forall m a. MonadEffect m => EnvKey a -> m (Maybe a)
lookupOptional (EnvKey { key, decode }) = liftEffect $ Process.lookupEnv key >>= case _ of
  Nothing -> pure Nothing
  Just "" -> pure Nothing
  Just value -> case decode value of
    Left error -> do
      Console.log $ "Found " <> key <> " in the environment with value " <> value <> ", but it could not be decoded: " <> error
      liftEffect (Process.exit 1)
    Right decoded -> pure $ Just decoded

-- | Look up a required environment variable, throwing an exception if it is
-- | missing, an empty string, or present but cannot be decoded.
lookupRequired :: forall m a. MonadEffect m => EnvKey a -> m a
lookupRequired (EnvKey { key, decode }) = liftEffect $ Process.lookupEnv key >>= case _ of
  Nothing -> do
    Console.log $ key <> " is not present in the environment."
    liftEffect (Process.exit 1)
  Just "" -> do
    Console.log $ "Found " <> key <> " in the environment, but its value was an empty string."
    liftEffect (Process.exit 1)
  Just value -> case decode value of
    Left error -> do
      Console.log $ "Found " <> key <> " in the environment with value " <> value <> ", but it could not be decoded: " <> error
      liftEffect (Process.exit 1)
    Right decoded -> pure decoded

-- | A user GitHub token at the REPO_TOKEN key.
githubToken :: EnvKey GitHubToken
githubToken = EnvKey
  { key: "REPO_TOKEN"
  , decode: \str -> do
      if String.null str then
        Left "Token string is empty."
      else case String.stripPrefix (String.Pattern "ghp_") str of
        Nothing -> Left "Expected prefix ghp_ on token, but it wasn't found."
        Just _ -> Right (GitHubToken str)
  }
