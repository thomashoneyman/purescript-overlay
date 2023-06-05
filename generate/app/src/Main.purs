module App.Main where

import Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as Arg
import Data.Argonaut.Parser as Argonaut.Parser
import Data.Array as Array
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Data.String as String
import Data.Traversable (for, for_)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Lib.NixManifest (NixManifest(..))
import Lib.NixManifest as NixManifest
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS.Aff
import Node.Path (FilePath)
import Node.Path as Path
import Node.Process as Process

data Commit = DoCommit | NoCommit

derive instance Eq Commit

data Command
  = Verify FilePath
  | Prefetch FilePath
  | Update FilePath Commit

derive instance Eq Command

parser :: ArgParser Command
parser =
  Arg.choose "command"
    [ Arg.command [ "verify" ]
        "Verify that the generation script can read and write the manifests."
        do
          Verify <$> manifestDir
            <* Arg.flagHelp
    , Arg.command [ "prefetech" ]
        "Run the generation script without modifying files (print output)."
        do
          Prefetch <$> manifestDir
            <* Arg.flagHelp
    , Arg.command [ "update" ]
        "Run the generation script and write files."
        do
          Update
            <$> manifestDir
            <*> updateOptions
            <* Arg.flagHelp
    ] <* Arg.flagHelp
  where
  manifestDir =
    Arg.anyNotFlag "MANIFEST_DIR" "Location of the tooling manifests"

  updateOptions =
    Arg.flag [ "--commit" ]
      "Whether to commit results and open a pull request. Default: false"
      # Arg.boolean
      # Arg.default false
      # map (if _ then DoCommit else NoCommit)

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
    Verify dir -> do
      Console.log "Verifying that manifests can be read and written..."
      allPaths <- FS.Aff.readdir dir
      let toolPaths = Array.filter (not <<< String.contains (String.Pattern "named.json")) allPaths
      parsed <- for toolPaths \path -> do
        rawManifest <- FS.Aff.readTextFile UTF8 $ Path.concat [ dir, path ]
        jsonManifest <- case Argonaut.Parser.jsonParser rawManifest of
          Left error -> Aff.throwError $ Aff.error $ "JSON error decoding manifest " <> path <> ": " <> error
          Right json -> pure json
        case path of
          "purs.json" -> case CA.decode NixManifest.pursManifestCodec jsonManifest of
            Left error -> Aff.throwError $ Aff.error $ "JSON error decoding purs.json: " <> CA.printJsonDecodeError error
            Right manifest -> pure $ PursManifest manifest
          "spago.json" -> case CA.decode NixManifest.spagoManifestCodec jsonManifest of
            Left error -> Aff.throwError $ Aff.error $ "JSON error decoding purs.json: " <> CA.printJsonDecodeError error
            Right manifest -> pure $ SpagoManifest manifest
          otherPath ->
            Aff.throwError $ Aff.error $ "Unexpected manifest path: " <> otherPath
      for_ parsed case _ of
        PursManifest _manifest ->
          Console.log "Successfully parsed purs.json"
        SpagoManifest _manifest ->
          Console.log "Successfully parsed spago.json"

    Prefetch _ ->
      Console.log "Prefetching..."

    Update _ _ ->
      Console.log "Updating..."
