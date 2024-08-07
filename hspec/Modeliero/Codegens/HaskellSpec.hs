module Modeliero.Codegens.HaskellSpec where

import Coalmine.Prelude
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Modeliero.Codegens.Haskell qualified as Subject
import Modeliero.Codegens.Haskell.Params qualified as Subject.Params
import Test.Hspec
import Test.Hspec.Expectations.Contrib

spec :: Spec
spec = do
  describe "Cases" do
    describe "ISO-8601" do
      let params =
            Subject.Params.Model
              { name = "iso8601",
                version = pure 0 <> pure 1 <> pure 0,
                types =
                  [ Subject.Params.TypeDeclaration
                      { name = "ymd",
                        docs = "ISO-8601 Year Month Day for representing a date.",
                        definition =
                          Subject.Params.ProductTypeDefinition
                            [ Subject.Params.Field
                                { name = "separated",
                                  docs = "Whether the format contains dashes.",
                                  type_ =
                                    Subject.Params.PlainFieldType
                                      ( Subject.Params.StandardPlainType
                                          Subject.Params.BoolStandardType
                                      )
                                },
                              Subject.Params.Field
                                { name = "year",
                                  docs = "Year.",
                                  type_ =
                                    Subject.Params.PlainFieldType
                                      (Subject.Params.LocalPlainType "year")
                                },
                              Subject.Params.Field
                                { name = "month",
                                  docs = "Month.",
                                  type_ =
                                    Subject.Params.PlainFieldType
                                      (Subject.Params.LocalPlainType "month")
                                },
                              Subject.Params.Field
                                { name = "day",
                                  docs = "Day.",
                                  type_ =
                                    Subject.Params.PlainFieldType
                                      (Subject.Params.LocalPlainType "day")
                                }
                            ]
                      }
                  ],
                instances =
                  Subject.Params.Instances
                    { show = True,
                      eq = True,
                      ord = True,
                      generic = True,
                      aeson =
                        Just
                          Subject.Params.Aeson
                            { casing = Subject.Params.KebabCasing
                            },
                      arbitrary = True
                    }
              }
          result = Subject.compile params
          fileNames = fmap fst result
          fileMap = Map.fromList result

      describe "Result" do
        describe "Length" do
          let resultLength = length result

          it "Equals the length of files grouped by path" do
            shouldBe resultLength (Map.size fileMap)

          it "Is as expected" . annotate (show fileNames) $ do
            shouldBe resultLength 3

      describe "Cabal file" do
        let lookupResult = Map.lookup "modeliero-artifacts-iso8601.cabal" fileMap

        it "Exists" do
          shouldNotBe lookupResult Nothing

        forM_ lookupResult \(Text.strip -> content) ->
          describe "Content" do
            it "Is as expected" do
              shouldBe content
                $ [i|
                    cabal-version: 3.0
                    
                    name: modeliero-artifacts-iso8601
                    synopsis: Generated model package
                    version: 0.1.0
                    
                    library
                      hs-source-dirs: library
                      default-extensions: ApplicativeDo, BangPatterns, BinaryLiterals, BlockArguments, ConstraintKinds, DataKinds, DefaultSignatures, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable, DerivingVia, DuplicateRecordFields, EmptyDataDecls, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, HexFloatLiterals, LambdaCase, LiberalTypeSynonyms, MultiParamTypeClasses, MultiWayIf, NoFieldSelectors, NoImplicitPrelude, NoMonomorphismRestriction, NumericUnderscores, OverloadedRecordDot, OverloadedStrings, PatternGuards, PatternSynonyms, ParallelListComp, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeApplications, TypeFamilies, TypeOperators, ViewPatterns
                      default-language: Haskell2010
                      exposed-modules:
                        ModelieroArtifacts.Iso8601
                      other-modules:
                        ModelieroArtifacts.Iso8601.Types.Ymd
                      build-depends:
                        QuickCheck >=2.15 && <3,
                        aeson >=2 && <3,
                        base >=4.14 && <5
                  |]

      describe "Types" do
        describe "Ymd" do
          let lookupResult = Map.lookup "library/ModelieroArtifacts/Iso8601/Types/Ymd.hs" fileMap

          it "Exists" do
            shouldNotBe lookupResult Nothing

          forM_ lookupResult \content ->
            describe "Content" do
              it "Is as expected" do
                shouldBe content
                  $ [i|
                      module ModelieroArtifacts.Iso8601.Types.Ymd
                        ( Ymd(..),
                        )
                      where
                      
                      import Prelude
                      import Data.Aeson qualified as Aeson
                      import Data.Aeson.KeyMap qualified as Aeson.KeyMap
                      import Data.Aeson.Types qualified as Aeson.Types
                      import GHC.Generics qualified
                      import ModelieroArtifacts.Iso8601.Types.Day qualified as Local
                      import ModelieroArtifacts.Iso8601.Types.Month qualified as Local
                      import ModelieroArtifacts.Iso8601.Types.Year qualified as Local
                      import Test.QuickCheck.Arbitrary qualified as Qc.Arbitrary

                      -- | ISO-8601 Year Month Day for representing a date.
                      data Ymd = Ymd
                        { -- | Whether the format contains dashes.
                          separated :: Bool,
                          -- | Year.
                          year :: Local.Year,
                          -- | Month.
                          month :: Local.Month,
                          -- | Day.
                          day :: Local.Day
                        }
                        deriving (Show, Eq, Ord, GHC.Generics.Generic)

                      instance Aeson.ToJSON Ymd where
                        toJSON value =
                          (Aeson.Object . Aeson.KeyMap.fromList)
                            [ ("separated", Aeson.toJSON value.separated),
                              ("year", Aeson.toJSON value.year),
                              ("month", Aeson.toJSON value.month),
                              ("day", Aeson.toJSON value.day)
                            ]

                      instance Aeson.FromJSON Ymd where
                        parseJSON = \case
                          Aeson.Object object -> do
                            separated <- Aeson.Types.parseField object "separated"
                            year <- Aeson.Types.parseField object "year"
                            month <- Aeson.Types.parseField object "month"
                            day <- Aeson.Types.parseField object "day"
                            pure Ymd{..}
                          json -> Aeson.Types.typeMismatch "Object" json

                      instance Qc.Arbitrary.Arbitrary Ymd where
                        arbitrary Ymd{..} = do
                          separated <- Qc.Arbitrary.arbitrary
                          year <- Qc.Arbitrary.arbitrary
                          month <- Qc.Arbitrary.arbitrary
                          day <- Qc.Arbitrary.arbitrary
                          pure Ymd{..}
                        shrink value = do
                          separated <- Qc.Arbitrary.shrink value.separated
                          year <- Qc.Arbitrary.shrink value.year
                          month <- Qc.Arbitrary.shrink value.month
                          day <- Qc.Arbitrary.shrink value.day
                          pure Ymd{..}
                    |]

      describe "Reexports" do
        let lookupResult = Map.lookup "library/ModelieroArtifacts/Iso8601.hs" fileMap

        it "Exists" do
          shouldNotBe lookupResult Nothing

        forM_ lookupResult \content ->
          describe "Content" do
            it "Is as expected" do
              shouldBe content
                $ [i|
                    module ModelieroArtifacts.Iso8601
                      ( module ModelieroArtifacts.Iso8601.Types.Ymd,
                      )
                    where
                    
                    import ModelieroArtifacts.Iso8601.Types.Ymd
                  |]
