{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Outpaths
  ( currentOutpathSet,
    currentOutpathSetUncached,
    Outpath (..),
    ResultLine (..),
    dummyOutpathSetBefore,
    dummyOutpathSetAfter,
    parseEvalResults,
    packageRebuilds,
    numPackageRebuilds,
    outpathReport,
  )
where

import Control.Exception (onException)
import Data.Aeson (FromJSON (parseJSON), Value, eitherDecodeStrict', encode, object, withObject, (.:), (.:?), (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import qualified Git
import Language.Haskell.TH.Env (envQ)
import OurPrelude
import qualified System.Directory
import qualified System.FilePath as FP
import qualified System.IO as IO
import qualified System.IO.Error as IOE
import qualified System.Posix.Files as F
import qualified Utils

nixEvalJobsBin :: String
nixEvalJobsBin = fromJust ($$(envQ "NIXEVALJOBS") :: Maybe String) <> "/bin/nix-eval-jobs"

outPathsExpr :: Text
outPathsExpr =
  [interpolate|
{ checkMeta
, path ? ./.
}:
let
  lib = import (path + "/lib");
  hydraJobs = import (path + "/pkgs/top-level/release.nix")
    # Compromise: accuracy vs. resources needed for evaluation.
    # we only evaluate one architecture per OS as we most likely catch all
    # mass-rebuilds this way.
    {
      supportedSystems = [
        "x86_64-linux"
     ];

      nixpkgsArgs = {
        config = {
          allowUnfree = true;
          allowInsecurePredicate = x: true;
          checkMeta = checkMeta;

          handleEvalIssue = reason: errormsg:
            let
              fatalErrors = [
                "unknown-meta" "broken-outputs"
              ];
            in if builtins.elem reason fatalErrors
              then abort errormsg
              else true;

          inHydra = true;
        };
      };
    };
  nixosJobs = import (path + "/nixos/release.nix") {
    supportedSystems = [ "x86_64-linux" ];
  };
  recurseIntoAttrs = attrs: attrs // { recurseForDerivations = true; };

  # hydraJobs leaves recurseForDerivations as empty attrmaps;
  # that would break nix-env and we also need to recurse everywhere.
  tweak = lib.mapAttrs
    (name: val:
      if name == "recurseForDerivations" then true
      else if lib.isAttrs val && val.type or null != "derivation"
              then recurseIntoAttrs (tweak val)
      else val
    );

  # Some of these contain explicit references to platform(s) we want to avoid;
  # some even (transitively) depend on ~/.nixpkgs/config.nix (!)
  blacklist = [
    "tarball" "metrics" "manual"
    "darwin-tested" "unstable" "stdenvBootstrapTools"
    "moduleSystem" "lib-tests" # these just confuse the output
  ];

in
  tweak (
    (builtins.removeAttrs hydraJobs blacklist)
    // {
      nixosTests.simple-container = nixosJobs.tests.simple-container;
      nixosTests.simple-vm = nixosJobs.tests.simple-vm;
    }
  )
|]

evaluateOutpaths :: (MonadIO m) => Maybe FilePath -> ExceptT Text m (Set ResultLine)
evaluateOutpaths cacheFile = do
  cacheDir <- liftIO $ Utils.outpathCacheDir
  let outpathFile = (cacheDir </> "outpaths.nix")
  liftIO $ T.writeFile outpathFile outPathsExpr
  liftIO $ putStrLn "[outpaths] eval start"
  currentDir <- liftIO $ System.Directory.getCurrentDirectory
  result <-
    tryIOTextET $
      runEvaluator
        [ "--workers",
          "1",
          "--max-memory-size",
          "4096",
          "--arg",
          "path",
          currentDir,
          "--arg",
          "checkMeta",
          "true",
          "--show-trace",
          outpathFile
        ]
        cacheFile
  liftIO $ putStrLn "[outpaths] eval end"
  result & hoistEither

data Outpath = Outpath
  { mayName :: Maybe Text,
    storePath :: Text
  }
  deriving (Eq, Ord, Show)

data ResultLine = ResultLine
  { package :: Text,
    architecture :: Text,
    outpaths :: Vector Outpath
  }
  deriving (Eq, Ord, Show)

data EvalResult = EvalResult
  { evalAttr :: Text,
    evalDrvPath :: Maybe Text,
    evalOutputs :: Maybe (Map Text (Maybe Text)),
    evalSystem :: Maybe Text,
    evalError :: Maybe Text
  }
  deriving (Eq, Show)

instance FromJSON EvalResult where
  parseJSON = withObject "nix-eval-jobs result" $ \result ->
    EvalResult
      <$> result .: "attr"
      <*> result .:? "drvPath"
      <*> result .:? "outputs"
      <*> result .:? "system"
      <*> result .:? "error"

currentOutpathSet :: (MonadIO m) => ExceptT Text m (Set ResultLine)
currentOutpathSet = do
  rev <- Git.headRev
  file <- lift $ outpathCacheFile rev
  fileExists <- liftIO $ F.fileExist file
  if fileExists
    then do
      result <- tryIOTextET $ IO.withBinaryFile file IO.ReadMode $ \handle -> consumeEvalResults handle Nothing
      result & hoistEither
    else evaluateOutpaths (Just file)

currentOutpathSetUncached :: (MonadIO m) => ExceptT Text m (Set ResultLine)
currentOutpathSetUncached = evaluateOutpaths Nothing

outpathCacheFile :: (MonadIO m) => Text -> m FilePath
outpathCacheFile rev = do
  dir <- Utils.outpathCacheDir
  pure $ dir <> "/" <> T.unpack rev <> ".jsonl"

runEvaluator :: [String] -> Maybe FilePath -> IO (Either Text (Set ResultLine))
runEvaluator arguments Nothing =
  withProcessWait config $ \process -> do
    result <- consumeEvalResults (getStdout process) Nothing
    checkExitCode process
    pure result
  where
    config = setStdout createPipe $ setStderr inherit $ proc nixEvalJobsBin arguments
runEvaluator arguments (Just file) = do
  (temporary, handle) <- IO.openBinaryTempFile (FP.takeDirectory file) (FP.takeFileName file <> ".tmp")
  let cleanup = do
        IOE.catchIOError (IO.hClose handle) (const $ pure ())
        IOE.catchIOError (System.Directory.removeFile temporary) (const $ pure ())
  ( do
      result <- withProcessWait config $ \process -> do
        parsed <- consumeEvalResults (getStdout process) (Just handle)
        checkExitCode process
        pure parsed
      case result of
        Left _ -> cleanup >> pure result
        Right _ -> do
          IO.hFlush handle
          IO.hClose handle
          System.Directory.renameFile temporary file
          pure result
    )
    `onException` cleanup
  where
    config = setStdout createPipe $ setStderr inherit $ proc nixEvalJobsBin arguments

consumeEvalResults :: IO.Handle -> Maybe IO.Handle -> IO (Either Text (Set ResultLine))
consumeEvalResults source cacheHandle = go 1 S.empty Nothing
  where
    go lineNumber results firstFailure = do
      atEnd <- IO.hIsEOF source
      if atEnd
        then pure $
          case firstFailure of
            Just failure -> Left failure
            Nothing
              | S.null results -> Left "nix-eval-jobs returned no successful results"
              | otherwise -> Right results
        else do
          line <- BS.hGetLine source
          if BS8.all (`elem` [' ', '\t', '\r', '\n']) line
            then go lineNumber results firstFailure
            else case firstFailure of
              Just _ -> go (lineNumber + 1) results firstFailure
              Nothing ->
                case decodeByteStringLine lineNumber line of
                  Left failure -> go (lineNumber + 1) results (Just failure)
                  Right evalResult -> do
                    writeProjectedResult cacheHandle evalResult
                    case insertEvalResult results evalResult of
                      Left failure -> go (lineNumber + 1) results (Just failure)
                      Right updated -> go (lineNumber + 1) updated Nothing

writeProjectedResult :: Maybe IO.Handle -> EvalResult -> IO ()
writeProjectedResult Nothing _ = pure ()
writeProjectedResult (Just handle) result = do
  BSL.hPut handle (encode $ projectEvalResult result)
  BS.hPut handle "\n"

projectEvalResult :: EvalResult -> Value
projectEvalResult result =
  object $
    ["attr" .= evalAttr result]
      <> maybe [] (\value -> ["drvPath" .= value]) (evalDrvPath result)
      <> maybe [] (\value -> ["outputs" .= value]) (evalOutputs result)
      <> maybe [] (\value -> ["system" .= value]) (evalSystem result)
      <> maybe [] (\value -> ["error" .= value]) (evalError result)

insertEvalResult :: Set ResultLine -> EvalResult -> Either Text (Set ResultLine)
insertEvalResult results result
  | isJust (evalError result) = Right results
  | otherwise = do
      mayResult <- toResultLine result
      pure $ maybe results (`S.insert` results) mayResult

dummyOutpathSetBefore :: Text -> Set ResultLine
dummyOutpathSetBefore attrPath = S.singleton (ResultLine attrPath "x86-64" (V.singleton (Outpath (Just "attrPath") "fakepath")))

dummyOutpathSetAfter :: Text -> Set ResultLine
dummyOutpathSetAfter attrPath = S.singleton (ResultLine attrPath "x86-64" (V.singleton (Outpath (Just "attrPath") "fakepath-edited")))

parseEvalResults :: Text -> Either Text (Set ResultLine)
parseEvalResults input = do
  results <- traverse decodeLine numberedLines
  case filter (isNothing . evalError) results of
    [] -> Left "nix-eval-jobs returned no successful results"
    successfulResults -> do
      canonicalResults <- catMaybes <$> traverse toResultLine successfulResults
      case canonicalResults of
        [] -> Left "nix-eval-jobs returned no successful results"
        _ -> Right $ S.fromList canonicalResults
  where
    numberedLines = zip [(1 :: Int) ..] $ filter (not . T.null) $ T.strip <$> T.lines input

decodeLine :: (Int, Text) -> Either Text EvalResult
decodeLine (lineNumber, line) = decodeByteStringLine lineNumber (TE.encodeUtf8 line)

decodeByteStringLine :: Int -> BS.ByteString -> Either Text EvalResult
decodeByteStringLine lineNumber line =
  either
    (\failure -> Left $ "Invalid nix-eval-jobs JSON on line " <> tshow lineNumber <> ": " <> T.pack failure)
    Right
    (eitherDecodeStrict' line)

toResultLine :: EvalResult -> Either Text (Maybe ResultLine)
toResultLine result =
  case (evalDrvPath result, evalOutputs result, evalSystem result) of
    (Nothing, _, _) -> missingField "drvPath"
    (_, Nothing, _) -> missingField "outputs"
    (_, _, Nothing) -> missingField "system"
    (Just _, Just outputs, Just system)
      | M.null outputs -> Left $ "nix-eval-jobs returned no outputs for " <> evalAttr result
      | M.null knownOutputs -> Right Nothing
      | otherwise ->
          Right . Just $
            ResultLine
              (fromMaybe (evalAttr result) $ T.stripSuffix ("." <> system) $ evalAttr result)
              system
              (V.fromList $ toOutpath <$> M.toAscList knownOutputs)
      where
        knownOutputs = M.mapMaybe id outputs
  where
    missingField field = Left $ "nix-eval-jobs result for " <> evalAttr result <> " is missing " <> field

toOutpath :: (Text, Text) -> Outpath
toOutpath (name, path) = Outpath (if name == "out" then Nothing else Just name) path

packageRebuilds :: Set ResultLine -> Vector Text
packageRebuilds = S.toList >>> fmap package >>> sort >>> V.fromList >>> V.uniq

numPackageRebuilds :: Set ResultLine -> Int
numPackageRebuilds diff = V.length $ packageRebuilds diff

outpathReport :: Set ResultLine -> Text
outpathReport diff =
  let pkg = tshow $ V.length $ packageRebuilds diff
      firstFifty = T.unlines $ V.toList $ V.take 50 $ packageRebuilds diff
      numPaths = tshow $ S.size diff
   in [interpolate|
        $numPaths total rebuild path(s)

        $pkg package rebuild(s)

        First fifty rebuilds by attrpath
        $firstFifty
      |]
