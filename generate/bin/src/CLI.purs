module Bin.CLI where

import Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as Arg
import Data.Array as Array
import Data.Either (Either(..))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Node.Path (FilePath)
import Node.Process as Process

data Commit = DoCommit | NoCommit

derive instance Eq Commit

data Verbosity = Quiet | Normal | Verbose

derive instance Eq Verbosity
derive instance Ord Verbosity

-- TODO: Allow specifying one or more specific tools to include (default to all).
-- Make the manifest dir optional (default: use git rev-parse to go to the root,
-- look for a "manifests" directory containing "purs.json" and "spago.json").
--
-- Then, commands mean "verify <tool> using <dir>" or "update <tool>"
data Command
  = Verify { dir :: FilePath, verbosity :: Verbosity }
  | Prefetch { dir :: FilePath, verbosity :: Verbosity }
  | Update { dir :: FilePath, verbosity :: Verbosity, commit :: Commit }

derive instance Eq Command

parser :: ArgParser Command
parser = ado
  verbosity <- verbosityFlag
  command <- Arg.choose "command"
    [ Arg.command [ "verify" ]
        "Verify that the generation script can read and write the manifests."
        ( ado
            dir <- manifestDir
            Arg.flagHelp
            in \v -> Verify { dir, verbosity: v }
        )
    , Arg.command [ "prefetch" ]
        "Run the generation script without modifying files (print output)."
        ( ado
            dir <- manifestDir
            Arg.flagHelp
            in \v -> Prefetch { dir, verbosity: v }
        )
    , Arg.command [ "update" ]
        "Run the generation script and write files."
        ( ado
            dir <- manifestDir
            commit <- commitFlag
            Arg.flagHelp
            in \v -> Update { dir, verbosity: v, commit }
        )
    ]
  Arg.flagHelp
  in command verbosity
  where
  manifestDir =
    Arg.anyNotFlag "MANIFEST_DIR" "Location of the tooling manifests"

  commitFlag =
    Arg.flag [ "--commit" ]
      "Whether to commit results and open a pull request. Default: false"
      # Arg.boolean
      # Arg.default false
      # map (if _ then DoCommit else NoCommit)

  verbosityFlag =
    Arg.flag [ "-v", "--verbose" ]
      "Enable verbose logging output"
      # Arg.boolean
      # Arg.default false
      # map (if _ then Verbose else Normal)

-- | Execute the CLI parser and return the command to run.
run :: forall m. MonadEffect m => m Command
run = do
  args <- Array.drop 2 <$> liftEffect Process.argv

  let description = "A generation script for updating PureScript tooling versions."
  case Arg.parseArgs "generate" description parser args of
    Left error -> do
      Console.log (Arg.printArgError error)
      case error of
        Arg.ArgError _ Arg.ShowHelp -> do
          liftEffect (Process.exit' 0)
        _ ->
          liftEffect (Process.exit' 1)
    Right command ->
      pure command
