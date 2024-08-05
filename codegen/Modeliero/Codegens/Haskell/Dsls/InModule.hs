-- | A simpler API over Code.
module Modeliero.Codegens.Haskell.Dsls.InModule
  ( compileModule,
    compileDependencies,
    compileContent,
    compileCode,
    InModule,
    import_,
    export,
    reexport,
    code,
    groupedLegacyExp,
    ungroupedLegacyExp,
    Import (..),
    Dependency (..),
  )
where

import Coalmine.Prelude
import CodegenKit.HaskellPackage.Contexts.Exp qualified as LegacyExp
import Modeliero.Codegens.Haskell.Dsls.Code (Import (..))
import Modeliero.Codegens.Haskell.Dsls.Code qualified as Code
import Modeliero.Codegens.Haskell.Dsls.Package (Dependency (..))
import Modeliero.Codegens.Haskell.Dsls.Package qualified as Package

compileModule ::
  [Text] ->
  [(Text, Text)] ->
  InModule TextBlock ->
  Package.Module
compileModule name importAliases =
  Code.compileCodeModule name importAliases . compileCode

compileDependencies :: InModule TextBlock -> [Package.Dependency]
compileDependencies =
  Code.compileCodeDependencies . compileCode

compileContent :: [Text] -> [(Text, Text)] -> InModule TextBlock -> Text
compileContent namespace importAliases =
  Code.compileCodeContent namespace importAliases . compileCode

compileCode :: InModule TextBlock -> Code.Code
compileCode (InModule run) =
  run Code.textBlock

newtype InModule a
  = InModule ((a -> Code.Code) -> Code.Code)
  deriving
    (Functor, Applicative, Monad)
    via (Cont Code.Code)

import_ :: Code.Import -> InModule Text
import_ import_ =
  InModule (Code.importing import_)

export :: Text -> InModule ()
export export =
  InModule (\cont -> cont () <> Code.export export)

reexport :: Code.Import -> InModule ()
reexport =
  void . code . Code.reexport

code :: Code.Code -> InModule TextBlock
code code =
  InModule (Code.splicing code)

groupedLegacyExp :: LegacyExp.Exp -> InModule TextBlock
groupedLegacyExp =
  code . Code.groupedLegacyExp

ungroupedLegacyExp :: LegacyExp.Exp -> InModule TextBlock
ungroupedLegacyExp =
  code . Code.ungroupedLegacyExp
