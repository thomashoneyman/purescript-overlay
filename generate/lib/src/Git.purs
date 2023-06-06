module Lib.Git where

import Prelude

import Data.Newtype (class Newtype)

-- | A Git tag
newtype Tag = Tag String

derive instance Newtype Tag _
derive newtype instance Eq Tag
derive newtype instance Ord Tag

-- | A Git commit hash
newtype CommitSha = CommitSha String

derive instance Newtype CommitSha _
derive newtype instance Eq CommitSha
derive newtype instance Ord CommitSha
