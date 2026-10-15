{-# LANGUAGE OverloadedStrings #-}

module OutpathsSpec (spec) where

import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Outpaths
import Test.Hspec

spec :: Spec
spec = do
  describe "parseEvalResults" do
    it "parses a default output and removes the system suffix from the package" do
      parseEvalResults singleOutput
        `shouldBe` Right expectedSingleOutput

    it "removes only the final system suffix from nested and quoted attributes" do
      parseEvalResults nestedOutput
        `shouldBe` Right
          ( S.singleton
              ( ResultLine
                  "haskellPackages.\"foo.bar\""
                  "x86_64-linux"
                  (V.singleton $ Outpath Nothing "/nix/store/foo-bar")
              )
          )

    it "preserves and sorts named outputs while leaving the default output unnamed" do
      parseEvalResults multipleOutputs
        `shouldBe` Right
          ( S.singleton
              ( ResultLine
                  "example"
                  "x86_64-linux"
                  ( V.fromList
                      [ Outpath (Just "dev") "/nix/store/example-dev",
                        Outpath (Just "doc") "/nix/store/example-doc",
                        Outpath Nothing "/nix/store/example"
                      ]
                  )
              )
          )

    it "skips per-attribute evaluation errors while preserving successes" do
      parseEvalResults (singleOutput <> "\n" <> evaluationError)
        `shouldBe` Right expectedSingleOutput

    it "ignores a very large inputDrvs object" do
      parseEvalResults largeInputDrvsOutput
        `shouldBe` Right expectedSingleOutput

    it "drops unknown content-addressed outputs while preserving known outputs" do
      parseEvalResults mixedNullableOutputs
        `shouldBe` Right expectedSingleOutput

    it "skips records whose declared outputs are all unknown" do
      parseEvalResults (singleOutput <> "\n" <> allNullOutputs)
        `shouldBe` Right expectedSingleOutput

      parseEvalResults allNullOutputs
        `shouldBe` Left "nix-eval-jobs returned no successful results"

    it "rejects output with no successful results" do
      parseEvalResults evaluationError
        `shouldBe` Left "nix-eval-jobs returned no successful results"

      parseEvalResults ""
        `shouldBe` Left "nix-eval-jobs returned no successful results"

    it "reports malformed JSON with its line number" do
      case parseEvalResults (singleOutput <> "\nnot-json") of
        Left failure -> failure `shouldSatisfy` T.isInfixOf "line 2"
        Right result -> expectationFailure $ "expected failure, got " <> show result

    it "rejects incomplete successful records" do
      case parseEvalResults missingOutputs of
        Left failure -> failure `shouldSatisfy` T.isInfixOf "missing outputs"
        Right result -> expectationFailure $ "expected failure, got " <> show result

      case parseEvalResults emptyOutputs of
        Left failure -> failure `shouldSatisfy` T.isInfixOf "returned no outputs"
        Right result -> expectationFailure $ "expected failure, got " <> show result

singleOutput :: Text
singleOutput =
  "{\"attr\":\"hello.x86_64-linux\",\"attrPath\":[\"hello\",\"x86_64-linux\"],\"drvPath\":\"/nix/store/hello.drv\",\"outputs\":{\"out\":\"/nix/store/hello\"},\"system\":\"x86_64-linux\"}"

largeInputDrvsOutput :: Text
largeInputDrvsOutput =
  "{\"attr\":\"hello.x86_64-linux\",\"attrPath\":[\"hello\",\"x86_64-linux\"],\"inputDrvs\":{\"/nix/store/dependency.drv\":["
    <> T.intercalate "," (replicate 100000 "\"out\"")
    <> "]},\"drvPath\":\"/nix/store/hello.drv\",\"outputs\":{\"out\":\"/nix/store/hello\"},\"system\":\"x86_64-linux\"}"

mixedNullableOutputs :: Text
mixedNullableOutputs =
  "{\"attr\":\"hello.x86_64-linux\",\"attrPath\":[\"hello\",\"x86_64-linux\"],\"drvPath\":\"/nix/store/hello.drv\",\"outputs\":{\"dev\":null,\"out\":\"/nix/store/hello\"},\"system\":\"x86_64-linux\"}"

allNullOutputs :: Text
allNullOutputs =
  "{\"attr\":\"contentAddressed.x86_64-linux\",\"attrPath\":[\"contentAddressed\",\"x86_64-linux\"],\"drvPath\":\"/nix/store/content-addressed.drv\",\"outputs\":{\"out\":null},\"system\":\"x86_64-linux\"}"

expectedSingleOutput :: S.Set ResultLine
expectedSingleOutput =
  S.singleton
    ( ResultLine
        "hello"
        "x86_64-linux"
        (V.singleton $ Outpath Nothing "/nix/store/hello")
    )

nestedOutput :: Text
nestedOutput =
  "{\"attr\":\"haskellPackages.\\\"foo.bar\\\".x86_64-linux\",\"attrPath\":[\"haskellPackages\",\"foo.bar\",\"x86_64-linux\"],\"drvPath\":\"/nix/store/foo-bar.drv\",\"outputs\":{\"out\":\"/nix/store/foo-bar\"},\"system\":\"x86_64-linux\"}"

multipleOutputs :: Text
multipleOutputs =
  "{\"attr\":\"example.x86_64-linux\",\"attrPath\":[\"example\",\"x86_64-linux\"],\"drvPath\":\"/nix/store/example.drv\",\"outputs\":{\"out\":\"/nix/store/example\",\"doc\":\"/nix/store/example-doc\",\"dev\":\"/nix/store/example-dev\"},\"system\":\"x86_64-linux\"}"

evaluationError :: Text
evaluationError =
  "{\"attr\":\"broken.x86_64-linux\",\"attrPath\":[\"broken\",\"x86_64-linux\"],\"error\":\"evaluation failed\"}"

missingOutputs :: Text
missingOutputs =
  "{\"attr\":\"incomplete.x86_64-linux\",\"attrPath\":[\"incomplete\",\"x86_64-linux\"],\"drvPath\":\"/nix/store/incomplete.drv\",\"system\":\"x86_64-linux\"}"

emptyOutputs :: Text
emptyOutputs =
  "{\"attr\":\"empty.x86_64-linux\",\"attrPath\":[\"empty\",\"x86_64-linux\"],\"drvPath\":\"/nix/store/empty.drv\",\"outputs\":{},\"system\":\"x86_64-linux\"}"
