-- | A simpler API over Code.
module Modeliero.Codegens.Haskell.Dsls.InModule
  ( compileToModule,
    compileToDependencies,
    compileToContent,
    compileToCode,
    InModule,
    requestImport,
    registerExport,
    registerReexport,
    liftCode,
    liftGroupedLegacyExp,
    liftUngroupedLegacyExp,
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

compileToModule ::
  [Text] ->
  [(Text, Text)] ->
  InModule TextBlock ->
  Package.Module
compileToModule name importAliases =
  Code.compileCodeModule name importAliases . compileToCode

compileToDependencies :: InModule TextBlock -> [Package.Dependency]
compileToDependencies =
  Code.compileCodeDependencies . compileToCode

compileToContent :: [Text] -> [(Text, Text)] -> InModule TextBlock -> Text
compileToContent namespace importAliases =
  Code.compileCodeContent namespace importAliases . compileToCode

compileToCode :: InModule TextBlock -> Code.Code
compileToCode (InModule run) =
  run Code.textBlock

newtype InModule a
  = InModule ((a -> Code.Code) -> Code.Code)
  deriving
    (Functor, Applicative, Monad)
    via (Cont Code.Code)

requestImport :: Code.Import -> InModule Text
requestImport import_ =
  InModule (Code.importing import_)

registerExport :: Text -> InModule ()
registerExport export =
  InModule (\cont -> cont () <> Code.export export)

registerReexport :: Code.Import -> InModule ()
registerReexport =
  void . liftCode . Code.reexport

liftCode :: Code.Code -> InModule TextBlock
liftCode code =
  InModule (Code.splicing code)

liftGroupedLegacyExp :: LegacyExp.Exp -> InModule TextBlock
liftGroupedLegacyExp =
  liftCode . Code.groupedLegacyExp

liftUngroupedLegacyExp :: LegacyExp.Exp -> InModule TextBlock
liftUngroupedLegacyExp =
  liftCode . Code.ungroupedLegacyExp
