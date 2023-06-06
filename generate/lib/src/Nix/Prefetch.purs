module Lib.Nix.Prefetch
  ( nixPrefetchTarball
  ) where

import Prelude

import Control.Monad.Except (ExceptT(..))
import Control.Monad.Except as Except
import Data.Either (Either(..))
import Data.String as String
import Effect.Aff.Class (class MonadAff, liftAff)
import Node.Library.Execa as Execa
import Registry.Sha256 (Sha256)
import Registry.Sha256 as Sha256

nixPrefetchTarball :: forall m. MonadAff m => String -> m (Either String Sha256)
nixPrefetchTarball url = Except.runExceptT do
  nixHash <- ExceptT (nixPrefetchUrl url)
  sha256 <- ExceptT (nixHashToSha256 nixHash)
  pure sha256

nixPrefetchUrl :: forall m. MonadAff m => String -> m (Either String String)
nixPrefetchUrl url = liftAff do
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
