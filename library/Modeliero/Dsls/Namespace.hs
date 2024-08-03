{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing #-}

module Modeliero.Dsls.Namespace
  ( Namespace,
    fromSlugList,
    fromModuleName,
    toSlugList,
    toModuleName,
    toFilePath,
  )
where

import Coalmine.Prelude
import Data.Text qualified as Text
import Modeliero.Dsls.Package

newtype Namespace = Namespace {slugs :: NonEmpty Slug}

instance Semigroup Namespace where
  left <> right =
    Namespace (left.slugs <> right.slugs)

fromSlugList :: [Slug] -> Maybe Namespace
fromSlugList =
  error "TODO"

fromModuleName :: Text -> Maybe Namespace
fromModuleName =
  error "TODO"

toSlugList :: Namespace -> [Slug]
toSlugList =
  error "TODO"

toModuleName :: Namespace -> Text
toModuleName =
  error "TODO"

toFilePath :: Namespace -> FilePath
toFilePath =
  error "TODO"
