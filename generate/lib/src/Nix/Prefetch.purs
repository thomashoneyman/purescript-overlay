module Lib.Nix.Prefetch
  ( nixPrefetchUrl
  , prefetchNpmDeps
  ) where

import Prelude

import Control.Monad.Except (ExceptT(..))
import Control.Monad.Except as Except
import Data.Either (Either(..))
import Data.String as String
import Effect.Aff.Class (class MonadAff, liftAff)
import Node.Library.Execa as Execa
import Node.Path (FilePath)
import Registry.Sha256 (Sha256)
import Registry.Sha256 as Sha256

prefetchNpmDeps :: forall m. MonadAff m => FilePath -> m (Either String Sha256)
prefetchNpmDeps file = liftAff do
  spawned <- Execa.execa "prefetch-npm-deps" [ file ] identity
  result <- spawned.result
  pure $ case result of
    Left error -> Left error.message
    Right { stdout } -> Sha256.parse (String.trim stdout)

nixPrefetchUrl :: forall m. MonadAff m => String -> m (Either String Sha256)
nixPrefetchUrl url = Except.runExceptT do
  nixHash <- ExceptT (nixPrefetchUrlRaw url)
  sha256 <- ExceptT (nixHashToSha256 nixHash)
  pure sha256

nixPrefetchUrlRaw :: forall m. MonadAff m => String -> m (Either String String)
nixPrefetchUrlRaw url = liftAff do
  spawned <- Execa.execa "nix-prefetch-url" [ url ] identity
  result <- spawned.result
  pure $ case result of
    Left error -> Left error.message
    Right { stdout } -> Right (String.trim stdout)

nixHashToSha256 :: forall m. MonadAff m => String -> m (Either String Sha256)
nixHashToSha256 hash = liftAff do
  spawned <- Execa.execa "nix" [ "hash", "to-sri", "--type", "sha256", hash ] identity
  result <- spawned.result
  pure $ case result of
    Left error -> Left error.message
    Right { stdout } -> Sha256.parse (String.trim stdout)
