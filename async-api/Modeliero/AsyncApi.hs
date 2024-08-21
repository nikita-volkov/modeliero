{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns #-}

-- | Partial implementation of Async API determined by the requirements of Modeliero.
module Modeliero.AsyncApi where

import AesonValueParser qualified as Avp
import Coalmine.Prelude
import Data.Aeson qualified as Aeson
import Data.OpenApi qualified as OpenApi
import Data.Yaml qualified as Yaml

-- * Model

data AsyncApi = AsyncApi
  { info :: Info,
    components :: ExtendedComponents
  }
  deriving (Show, Eq)

data Info = Info
  { title :: Text,
    version :: NumericVersion,
    description :: Text
  }
  deriving (Show, Eq)

data ExtendedComponents = ExtendedComponents
  { schemas :: HashMap Text ExtendedSchema
  }
  deriving (Show, Eq)

data ExtendedSchema = ExtendedSchema
  { base :: OpenApi.Schema,
    anonymizable :: Bool
  }
  deriving (Show, Eq)

-- * Instances

instance Aeson.FromJSON AsyncApi where
  parseJSON = Avp.runAsValueParser do
    Avp.object do
      info <- Avp.field "info" Avp.fromJSON
      components <- Avp.field "components" Avp.fromJSON
      pure AsyncApi {..}

instance Aeson.FromJSON Info where
  parseJSON = Avp.runAsValueParser do
    Avp.object do
      title <- Avp.field "title" Avp.fromJSON
      version <- Avp.field "version" (Avp.string (Avp.attoparsedText literalParser))
      description <- Avp.field "description" Avp.fromJSON
      pure Info {..}

instance Aeson.FromJSON ExtendedComponents where
  parseJSON = Avp.runAsValueParser do
    Avp.object do
      schemas <- Avp.field "schemas" Avp.fromJSON
      pure ExtendedComponents {..}

instance Aeson.FromJSON ExtendedSchema where
  parseJSON json = do
    base <- parseJSON json
    flip Avp.runAsValueParser json do
      Avp.object do
        anonymizable <- Avp.field "x-anonymizable" Avp.fromJSON <|> pure False
        pure ExtendedSchema {..}

load :: FilePath -> IO AsyncApi
load =
  Yaml.decodeFileThrow
