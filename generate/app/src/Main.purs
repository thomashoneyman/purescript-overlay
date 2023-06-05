module App.Main where

import Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as Arg
import Data.Array as Array
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Node.Process as Process

data GenerateMode = Verify | DryRun | Local

derive instance Eq GenerateMode

parser :: ArgParser GenerateMode
parser =
  Arg.choose "mode"
    [ Arg.flag [ "--verify" ]
        "Verify that the generation script can read and write the manifests."
        $> Verify
    , Arg.flag [ "--dry-run" ]
        "Run the generation script without modifying files (print output)."
        $> DryRun
    , Arg.flag [ "--local" ]
        "Run the generation script and write files locally."
        $> Local
    ] <* Arg.flagHelp

main :: Effect Unit
main = Aff.launchAff_ do
  args <- Array.drop 2 <$> liftEffect Process.argv

  let description = "A generation script for updating PureScript tooling versions."
  mode <- case Arg.parseArgs "generate" description parser args of
    Left error -> do
      Console.log (Arg.printArgError error)
      case error of
        Arg.ArgError _ Arg.ShowHelp -> do
          liftEffect (Process.exit 0)
        _ ->
          liftEffect (Process.exit 1)
    Right command ->
      pure command

  case mode of
    Verify -> do
      Console.log "Verifying..."
    DryRun ->
      Console.log "Running in dry-run mode..."
    Local ->
      Console.log "Running in local mode..."
