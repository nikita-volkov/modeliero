{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing #-}

module Modeliero.Templates.RefinedArbitraryInstance where

import Coalmine.Prelude
import Modeliero.Dsls.Code qualified as Code
import Modeliero.Dsls.Namespace qualified as Namespace
import Modeliero.Dsls.Package qualified as Package

type Params = Type

type Result = Code.Code

data Type
  = IntType
      (Maybe Int)
      (Maybe Int)

compile :: Params -> Result
compile params =
  error "TODO"
