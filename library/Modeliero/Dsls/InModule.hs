-- | A simpler API over Code.
module Modeliero.Dsls.InModule
  ( compileModule,
    compileDependencies,
    compileContent,
    compileCode,
    InModule,
    import_,
    export,
  )
where

import Coalmine.Prelude
import Modeliero.Dsls.Code qualified as Code
import Modeliero.Dsls.Package qualified as Package

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
compileCode inModule =
  inModule.run Code.textBlock

newtype InModule a
  = InModule
  { run :: (a -> Code.Code) -> Code.Code
  }
  deriving
    (Functor, Applicative, Monad)
    via (Cont Code.Code)

import_ :: Code.Import -> InModule Text
import_ import_ =
  InModule (Code.importing import_)

export :: Text -> InModule ()
export export =
  InModule (\cont -> cont () <> Code.export export)
