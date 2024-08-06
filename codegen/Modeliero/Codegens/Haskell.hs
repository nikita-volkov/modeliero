{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing #-}

module Modeliero.Codegens.Haskell where

import Coalmine.Prelude
import Modeliero.Codegens.Haskell.Dsls.Code qualified as Code
import Modeliero.Codegens.Haskell.Dsls.Namespace qualified as Namespace
import Modeliero.Codegens.Haskell.Dsls.Package qualified as Package
import Modeliero.Codegens.Haskell.Params qualified as Params

type Params = Params.Model

-- | Compiled files of the package at their relative paths.
type Result = [(FilePath, Text)]

compile :: Params -> Result
compile =
  error "TODO"
