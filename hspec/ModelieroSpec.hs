-- |
-- End-to-end tests.
module ModelieroSpec where

import Cases qualified
import Coalmine.EvenSimplerPaths qualified as Path
import Modeliero.Codegens.Haskell qualified as HaskellCodegen
import Modeliero.Codegens.Haskell.Params qualified as HaskellCodegen
import Modeliero.Preludes.Spec
import Modeliero.Sources.AsyncApi qualified as AsyncApiSource
import Turtle qualified

spec :: Spec
spec = do
  describe "Async API" do
    let dirPath = fixturesPath <> "async-api/"
    fixturePaths <-
      Path.listDirectory dirPath
        & runIO
    forM_ fixturePaths \fixturePath -> do
      describe (generalize fixturePath) do
        it "Compiles to Haskell" do
          AsyncApiSource.load
            AsyncApiSource.defaultConfig
            (generalize (dirPath <> fixturePath))
            >>= \case
              Left err ->
                err
                  & renderAsYamlText
                  & toList
                  & expectationFailure
              Right source -> do
                let tempDirPath = tempDirRootPath <> "async-api" <> "haskell" <> fixturePath
                Path.removeForcibly tempDirPath
                Path.createDirs tempDirPath
                HaskellCodegen.write
                  tempDirPath
                  HaskellCodegen.Model
                    { name =
                        fixturePath
                          & Path.name
                          & Cases.spinalize
                          & to
                          & fromString,
                      version = source.version,
                      types = source.types,
                      instances =
                        HaskellCodegen.Instances
                          { aeson =
                              Just
                                HaskellCodegen.Aeson
                                  { casing = HaskellCodegen.KebabCasing
                                  },
                            arbitrary = True,
                            anonymizable = True
                          }
                    }
                runShellCmd [i|cd ${tempDirPath} && cabal build --project-dir=. -j --ghc-options="-j"|]
                pure ()

fixturesPath :: Path
fixturesPath =
  "hspec/ModelieroSpec/fixtures/"

tempDirRootPath :: Path
tempDirRootPath =
  "dist/hspec-artifacts/ModelieroSpec/"

runShellCmd :: Text -> IO ()
runShellCmd cmd = do
  putStrLn [i|> ${cmd}|]
  Turtle.inshellWithErr cmd mempty
    & fmap (either (prefixLine "E > ") (prefixLine "O > "))
    & Turtle.stdout
  where
    prefixLine prefix =
      Turtle.unsafeTextToLine . mappend prefix . Turtle.lineToText
