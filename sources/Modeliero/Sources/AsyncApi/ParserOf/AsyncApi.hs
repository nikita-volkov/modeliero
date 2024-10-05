module Modeliero.Sources.AsyncApi.ParserOf.AsyncApi where

import Cases qualified
import Modeliero.AesonUtil.Values qualified as Json
import Modeliero.AsyncApi qualified as Input
import Modeliero.Sources.AsyncApi.ParserOf.ExtendedComponents qualified as ParserOfExtendedComponents
import Modeliero.Sources.AsyncApi.Preludes.Parser

type Input = Input.AsyncApi

data Output = Output
  { name :: Slug,
    version :: NonEmpty Word,
    types :: [TypeDeclaration]
  }

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
  types <-
    ParserOfExtendedComponents.parse config input.components
      & first ComponentsError
  name <-
    input.info.title
      & Cases.spinalize
      & to
      & fromString
      & pure
  pure
    Output
      { version = input.info.version & to,
        name,
        types
      }
