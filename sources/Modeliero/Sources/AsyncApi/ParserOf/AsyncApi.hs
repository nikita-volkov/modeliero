module Modeliero.Sources.AsyncApi.ParserOf.AsyncApi where

import Modeliero.AesonUtil.Values qualified as Json
import Modeliero.AsyncApi qualified as Input
import Modeliero.Sources.AsyncApi.ParserOf.ExtendedComponents qualified as ParserOfExtendedComponents
import Modeliero.Sources.AsyncApi.Preludes.Parser

type Input = Input.AsyncApi

type Output = [TypeDeclaration]

data Error
  = ComponentsError ParserOfExtendedComponents.Error
  deriving (Eq, Show, Generic)

instance ToJSON Error where
  toJSON = \case
    ComponentsError componentsError ->
      componentsError
        & toJSON
        & Json.tagged "components"

parse :: Config -> Input -> Either Error Output
parse config input = do
  componentsOutput <-
    ParserOfExtendedComponents.parse config input.components
      & first ComponentsError
  pure componentsOutput
