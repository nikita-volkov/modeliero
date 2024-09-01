module Modeliero.Sources.AsyncApi.TestModel.Preludes.Spec
  ( module Modeliero.Sources.AsyncApi.TestModel.Preludes.Spec,
    module Modeliero.Codegens.Haskell.Params,
    module Modeliero.Preludes.Spec,
  )
where

import Modeliero.Codegens.Haskell.Params
import Modeliero.Preludes.Spec
import Modeliero.Sources.AsyncApi qualified as Source

parsingSchema ::
  (HasCallStack) =>
  Text ->
  Text ->
  ([TypeDeclaration] -> Spec) ->
  Spec
parsingSchema name content schemaSpec =
  describe (toList name) do
    case Source.parse Source.defaultConfig content of
      Left err ->
        err
          & renderAsYamlText
          & toList
          & expectationFailure
          & it "Parses fine"
      Right model ->
        schemaSpec model.types
