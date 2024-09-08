module Modeliero.Codegens.HaskellSpec where

import Coalmine.Prelude
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Modeliero.Codegens.Haskell qualified as Subject
import Modeliero.Codegens.Haskell.Params qualified as Subject
import Test.Hspec
import Test.Hspec.Expectations.Contrib

spec :: Spec
spec = do
  describe "Cases" do
    describe "ISO-8601" do
      let params =
            Subject.Model
              { name = "iso8601",
                version = pure 0 <> pure 1 <> pure 0,
                types =
                  [ Subject.TypeDeclaration
                      { name = "ymd",
                        docs = "ISO-8601 Year Month Day for representing a date.",
                        definition =
                          Subject.ProductTypeDefinition
                            [ Subject.Field
                                { name = "separated",
                                  jsonName = "contains_dash",
                                  docs = "Whether the format contains dashes.",
                                  type_ =
                                    Subject.PlainValueType
                                      ( Subject.StandardPlainType
                                          Subject.BoolStandardType
                                      )
                                },
                              Subject.Field
                                { name = "year",
                                  jsonName = "year",
                                  docs = "Year.",
                                  type_ =
                                    Subject.PlainValueType
                                      (Subject.LocalPlainType "year")
                                },
                              Subject.Field
                                { name = "month",
                                  jsonName = "month",
                                  docs = "Month.",
                                  type_ =
                                    Subject.PlainValueType
                                      (Subject.LocalPlainType "month")
                                },
                              Subject.Field
                                { name = "day",
                                  jsonName = "day",
                                  docs = "Day.",
                                  type_ =
                                    Subject.PlainValueType
                                      (Subject.LocalPlainType "day")
                                }
                            ]
                      },
                    Subject.TypeDeclaration
                      { name = "ym",
                        docs =
                          [i|
                            Year, month.
              
                            @YYYY-MM@
                          |],
                        definition =
                          Subject.ProductTypeDefinition
                            [ Subject.Field
                                { name = "year",
                                  jsonName = "year",
                                  docs = "Year.",
                                  type_ =
                                    Subject.PlainValueType
                                      (Subject.LocalPlainType "year")
                                },
                              Subject.Field
                                { name = "month",
                                  jsonName = "month",
                                  docs = "Month.",
                                  type_ =
                                    Subject.PlainValueType
                                      (Subject.LocalPlainType "month")
                                }
                            ]
                      },
                    Subject.TypeDeclaration
                      { name = "md",
                        docs =
                          [i|
                            ISO-8601 Month Day for representing a date.

                            - @--MM-DD@
                            - @--MMDD@
                          |],
                        definition =
                          Subject.ProductTypeDefinition
                            [ Subject.Field
                                { name = "month",
                                  jsonName = "month",
                                  docs = "Month.",
                                  type_ =
                                    Subject.PlainValueType
                                      (Subject.LocalPlainType "month")
                                },
                              Subject.Field
                                { name = "day",
                                  jsonName = "day",
                                  docs = "Day.",
                                  type_ =
                                    Subject.PlainValueType
                                      (Subject.LocalPlainType "day")
                                }
                            ]
                      },
                    Subject.TypeDeclaration
                      { name = "year",
                        docs = "",
                        definition =
                          Subject.NewtypeTypeDefinition
                            Subject.NewtypeDefinition
                              { anonymizable = False,
                                wrappedType =
                                  Left
                                    $ Subject.IntegerRefinement
                                      Subject.IntegerRestrictions
                                        { min = (-9999),
                                          max = 9999
                                        }
                              }
                      },
                    Subject.TypeDeclaration
                      { name = "month",
                        docs = "",
                        definition =
                          Subject.NewtypeTypeDefinition
                            Subject.NewtypeDefinition
                              { anonymizable = False,
                                wrappedType =
                                  Left
                                    $ Subject.IntegerRefinement
                                      Subject.IntegerRestrictions
                                        { min = 1,
                                          max = 12
                                        }
                              }
                      },
                    Subject.TypeDeclaration
                      { name = "day",
                        docs = "",
                        definition =
                          Subject.NewtypeTypeDefinition
                            Subject.NewtypeDefinition
                              { anonymizable = False,
                                wrappedType =
                                  Left
                                    $ Subject.IntegerRefinement
                                      Subject.IntegerRestrictions
                                        { min = 1,
                                          max = 31
                                        }
                              }
                      },
                    Subject.TypeDeclaration
                      { name = "calendar-date",
                        docs =
                          [i|
                            - @YYYY-MM-DD@ or @YYYYMMDD@
                            - @YYYY-MM@ (but not @YYYYMM@)
                            - @--MM-DD@ or @--MMDD@
                          |],
                        definition =
                          Subject.SumTypeDefinition
                            [ Subject.Variant
                                { name = "ymd",
                                  jsonName = "yearMonthDay",
                                  docs = "",
                                  type_ = Subject.PlainValueType (Subject.LocalPlainType "ymd"),
                                  anonymizable = True
                                },
                              Subject.Variant
                                { name = "ym",
                                  jsonName = "ym",
                                  docs = "",
                                  type_ = Subject.PlainValueType (Subject.LocalPlainType "ym"),
                                  anonymizable = False
                                },
                              Subject.Variant
                                { name = "md",
                                  jsonName = "md",
                                  docs = "",
                                  type_ = Subject.PlainValueType (Subject.LocalPlainType "md"),
                                  anonymizable = True
                                }
                            ]
                      }
                  ],
                instances =
                  Subject.Instances
                    { show = True,
                      eq = True,
                      ord = True,
                      hashable = True,
                      aeson =
                        Just
                          Subject.Aeson
                            { casing = Subject.KebabCasing
                            },
                      arbitrary = True,
                      anonymizable = True
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
            shouldBe resultLength 10

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
                    
                    common base
                      default-language: Haskell2010
                      default-extensions:
                        ApplicativeDo
                        BangPatterns
                        BinaryLiterals
                        BlockArguments
                        ConstraintKinds
                        DataKinds
                        DefaultSignatures
                        DeriveAnyClass
                        DeriveDataTypeable
                        DeriveFoldable
                        DeriveFunctor
                        DeriveGeneric
                        DeriveTraversable
                        DerivingVia
                        DuplicateRecordFields
                        EmptyCase
                        EmptyDataDecls
                        FlexibleContexts
                        FlexibleInstances
                        FunctionalDependencies
                        GADTs
                        GeneralizedNewtypeDeriving
                        HexFloatLiterals
                        ImportQualifiedPost
                        LambdaCase
                        LiberalTypeSynonyms
                        MultiParamTypeClasses
                        MultiWayIf
                        NamedFieldPuns
                        NoFieldSelectors
                        NoImplicitPrelude
                        NoMonomorphismRestriction
                        NumericUnderscores
                        OverloadedLabels
                        OverloadedRecordDot
                        OverloadedStrings
                        ParallelListComp
                        PatternGuards
                        PatternSynonyms
                        QuasiQuotes
                        RankNTypes
                        RecordWildCards
                        ScopedTypeVariables
                        StandaloneDeriving
                        StrictData
                        TemplateHaskell
                        TupleSections
                        TypeApplications
                        TypeFamilies
                        TypeOperators
                        UndecidableInstances
                        ViewPatterns
                    
                    library
                      import: base
                      hs-source-dirs: library
                      exposed-modules:
                        ModelieroArtifacts.Iso8601
                      other-modules:
                        ModelieroArtifacts.Iso8601.Types.CalendarDate
                        ModelieroArtifacts.Iso8601.Types.Day
                        ModelieroArtifacts.Iso8601.Types.Md
                        ModelieroArtifacts.Iso8601.Types.Month
                        ModelieroArtifacts.Iso8601.Types.Year
                        ModelieroArtifacts.Iso8601.Types.Ym
                        ModelieroArtifacts.Iso8601.Types.Ymd
                      build-depends:
                        QuickCheck >=2.15 && <3,
                        aeson >=2 && <3,
                        base-prelude >=1.6.1.1 && <1.7,
                        hashable >=1.4 && <2,
                        modeliero-base >=0 && <0.1,
                        scientific >=0.3 && <0.4,
                        text >=2 && <3
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
                      
                      import BasePrelude
                      import Data.Aeson qualified as Aeson
                      import Data.Aeson.KeyMap qualified as Aeson.KeyMap
                      import Data.Aeson.Types qualified as Aeson.Types
                      import ModelieroArtifacts.Iso8601.Types.Day qualified as Local
                      import ModelieroArtifacts.Iso8601.Types.Month qualified as Local
                      import ModelieroArtifacts.Iso8601.Types.Year qualified as Local
                      import Test.QuickCheck.Arbitrary qualified as QuickCheck.Arbitrary

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
                        deriving (Show, Eq, Ord)

                      instance Aeson.ToJSON Ymd where
                        toJSON value =
                          (Aeson.Object . Aeson.KeyMap.fromList)
                            [ ("contains_dash", Aeson.toJSON value.separated),
                              ("year", Aeson.toJSON value.year),
                              ("month", Aeson.toJSON value.month),
                              ("day", Aeson.toJSON value.day)
                            ]

                      instance Aeson.FromJSON Ymd where
                        parseJSON = \case
                          Aeson.Object object -> do
                            separated <- Aeson.Types.parseField object "contains_dash"
                            year <- Aeson.Types.parseField object "year"
                            month <- Aeson.Types.parseField object "month"
                            day <- Aeson.Types.parseField object "day"
                            pure Ymd{..}
                          json -> Aeson.Types.typeMismatch "Object" json

                      instance QuickCheck.Arbitrary.Arbitrary Ymd where
                        arbitrary = do
                          separated <- QuickCheck.Arbitrary.arbitrary
                          year <- QuickCheck.Arbitrary.arbitrary
                          month <- QuickCheck.Arbitrary.arbitrary
                          day <- QuickCheck.Arbitrary.arbitrary
                          pure Ymd{..}
                        shrink value = do
                          separated <- QuickCheck.Arbitrary.shrink value.separated
                          year <- QuickCheck.Arbitrary.shrink value.year
                          month <- QuickCheck.Arbitrary.shrink value.month
                          day <- QuickCheck.Arbitrary.shrink value.day
                          pure Ymd{..}
                    |]

        describe "Year" do
          let lookupResult = Map.lookup "library/ModelieroArtifacts/Iso8601/Types/Year.hs" fileMap

          it "Exists" do
            shouldNotBe lookupResult Nothing

          forM_ lookupResult \content ->
            describe "Content" do
              it "Is as expected" do
                shouldBe
                  content
                  [i|
                    module ModelieroArtifacts.Iso8601.Types.Year
                      ( Year,
                      )
                    where
                    
                    import BasePrelude
                    import Data.Aeson qualified as Aeson
                    import Data.Aeson.Types qualified as Aeson.Types
                    import Data.Hashable qualified as Hashable
                    import Data.Scientific qualified as Scientific
                    import Data.Text qualified as Text
                    import ModelieroBase qualified
                    import Test.QuickCheck.Arbitrary qualified as QuickCheck.Arbitrary
                    import Test.QuickCheck.Gen qualified as QuickCheck.Gen
                    
                    newtype Year = Year
                      { base :: Int
                      }
                      deriving (Show, Eq, Ord, Hashable.Hashable)

                    instance ModelieroBase.Special Year where
                      type GeneralizationOf Year = Int
                      type SpecializationError Year = Text.Text
                      specialize value = do
                        when (value < -9999) $
                          Left ("Value is smaller than -9999: " <> fromString (show value))
                        when (value > 9999) $
                          Left ("Value is larger than 9999: " <> fromString (show value))
                        pure (Year value)
                      generalize (Year base) = base
                    
                    instance QuickCheck.Arbitrary.Arbitrary Year where
                      arbitrary = Year <$> QuickCheck.Gen.chooseInt (-9999, 9999)
                      shrink = const []
                    
                    instance Aeson.ToJSON Year where
                      toJSON (Year base) = Aeson.toJSON base
                    
                    instance Aeson.FromJSON Year where
                      parseJSON json = do
                        scientific <-
                          case json of
                            Aeson.Number scientific -> pure scientific
                            _ -> Aeson.Types.typeMismatch "Number" json
                        int <-
                          if Scientific.isInteger scientific
                            then case Scientific.toBoundedInteger scientific of
                              Just int -> pure int
                              Nothing -> fail ("Number " <> show scientific <> " is out of Int bounds")
                            else fail ("Number " <> show scientific <> " is not an integer")
                        case ModelieroBase.specialize int of
                          Right result -> pure result
                          Left text -> fail (Text.unpack text)
                  |]

        describe "CalendarDate" do
          let lookupResult = Map.lookup "library/ModelieroArtifacts/Iso8601/Types/CalendarDate.hs" fileMap

          it "Exists" do
            shouldNotBe lookupResult Nothing

          forM_ lookupResult \content ->
            describe "Content" do
              it "Is as expected" do
                shouldBe
                  content
                  [i|
                    module ModelieroArtifacts.Iso8601.Types.CalendarDate
                      ( CalendarDate (..),
                      )
                    where
                 
                    import BasePrelude
                    import Data.Aeson qualified as Aeson
                    import Data.Aeson.KeyMap qualified as Aeson.KeyMap
                    import Data.Aeson.Types qualified as Aeson.Types
                    import Data.Hashable qualified as Hashable
                    import ModelieroArtifacts.Iso8601.Types.Md qualified as Local
                    import ModelieroArtifacts.Iso8601.Types.Ym qualified as Local
                    import ModelieroArtifacts.Iso8601.Types.Ymd qualified as Local
                    import ModelieroBase qualified
                    import Test.QuickCheck.Arbitrary qualified as QuickCheck.Arbitrary
                    import Test.QuickCheck.Gen qualified as QuickCheck.Gen

                    -- |
                    -- - @YYYY-MM-DD@ or @YYYYMMDD@
                    -- - @YYYY-MM@ (but not @YYYYMM@)
                    -- - @--MM-DD@ or @--MMDD@
                    data CalendarDate
                      = YmdCalendarDate Local.Ymd
                      | YmCalendarDate Local.Ym
                      | MdCalendarDate Local.Md
                      deriving (Show, Eq, Ord)
                    
                    instance Hashable.Hashable CalendarDate where
                      hashWithSalt salt = \case
                        YmdCalendarDate ymd ->
                          salt
                            & flip Hashable.hashWithSalt (0 :: Int)
                            & flip Hashable.hashWithSalt ymd
                        YmCalendarDate ym ->
                          salt
                            & flip Hashable.hashWithSalt (1 :: Int)
                            & flip Hashable.hashWithSalt ym
                        MdCalendarDate md ->
                          salt
                            & flip Hashable.hashWithSalt (2 :: Int)
                            & flip Hashable.hashWithSalt md

                    instance Aeson.ToJSON CalendarDate where
                      toJSON = \case
                        YmdCalendarDate ymd ->
                          ymd
                            & Aeson.toJSON
                            & Aeson.KeyMap.singleton "yearMonthDay"
                            & Aeson.toJSON
                        YmCalendarDate ym ->
                          ym
                            & Aeson.toJSON
                            & Aeson.KeyMap.singleton "ym"
                            & Aeson.toJSON
                        MdCalendarDate md ->
                          md
                            & Aeson.toJSON
                            & Aeson.KeyMap.singleton "md"
                            & Aeson.toJSON
                    
                    instance Aeson.FromJSON CalendarDate where
                      parseJSON = \case
                        Aeson.Object object ->
                          [ object
                              & Aeson.KeyMap.lookup "yearMonthDay"
                              & fmap 
                                ( \json -> 
                                  YmdCalendarDate
                                    <$> Aeson.parseJSON json
                                    Aeson.Types.<?> Aeson.Types.Key "yearMonthDay"
                                ),
                            object
                              & Aeson.KeyMap.lookup "ym"
                              & fmap 
                                ( \json -> 
                                  YmCalendarDate
                                    <$> Aeson.parseJSON json
                                    Aeson.Types.<?> Aeson.Types.Key "ym"
                                ),
                            object
                              & Aeson.KeyMap.lookup "md"
                              & fmap 
                                ( \json -> 
                                  MdCalendarDate
                                    <$> Aeson.parseJSON json
                                    Aeson.Types.<?> Aeson.Types.Key "md"
                                )
                          ]
                            & asum
                            & fromMaybe (Aeson.Types.parseFail noTagFoundMessage)
                          where
                            noTagFoundMessage =
                              "No expected key found. \
                              \It should be one of the following: \
                              \yearMonthDay, ym, md"
                        json -> Aeson.Types.typeMismatch "Object" json

                    instance QuickCheck.Arbitrary.Arbitrary CalendarDate where
                      arbitrary =
                        QuickCheck.Gen.oneof
                          [ YmdCalendarDate
                              <$> QuickCheck.Arbitrary.arbitrary,
                            YmCalendarDate
                              <$> QuickCheck.Arbitrary.arbitrary,
                            MdCalendarDate
                              <$> QuickCheck.Arbitrary.arbitrary
                          ]
                      shrink = \case
                        YmdCalendarDate ymd ->
                          YmdCalendarDate
                            <$> QuickCheck.Arbitrary.shrink ymd
                        YmCalendarDate ym ->
                          YmCalendarDate
                            <$> QuickCheck.Arbitrary.shrink ym
                        MdCalendarDate md ->
                          MdCalendarDate
                            <$> QuickCheck.Arbitrary.shrink md
                    
                    instance ModelieroBase.Anonymizable CalendarDate where
                      anonymize forced = \case
                        YmdCalendarDate ymd ->
                          YmdCalendarDate
                            (ModelieroBase.anonymize True ymd)
                        YmCalendarDate ym ->
                          YmCalendarDate
                            (ModelieroBase.anonymize forced ym)
                        MdCalendarDate md ->
                          MdCalendarDate
                            (ModelieroBase.anonymize True md)
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
                      ( module ModelieroArtifacts.Iso8601.Types.CalendarDate,
                        module ModelieroArtifacts.Iso8601.Types.Day,
                        module ModelieroArtifacts.Iso8601.Types.Md,
                        module ModelieroArtifacts.Iso8601.Types.Month,
                        module ModelieroArtifacts.Iso8601.Types.Year,
                        module ModelieroArtifacts.Iso8601.Types.Ym,
                        module ModelieroArtifacts.Iso8601.Types.Ymd,
                      )
                    where
                    
                    import ModelieroArtifacts.Iso8601.Types.CalendarDate
                    import ModelieroArtifacts.Iso8601.Types.Day
                    import ModelieroArtifacts.Iso8601.Types.Md
                    import ModelieroArtifacts.Iso8601.Types.Month
                    import ModelieroArtifacts.Iso8601.Types.Year
                    import ModelieroArtifacts.Iso8601.Types.Ym
                    import ModelieroArtifacts.Iso8601.Types.Ymd
                  |]
