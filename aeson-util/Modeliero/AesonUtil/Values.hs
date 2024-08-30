module Modeliero.AesonUtil.Values where

import Coalmine.Prelude
import Data.Aeson.Key qualified as Aeson.Key
import Data.Aeson.KeyMap qualified as Aeson.KeyMap

tagged :: Text -> Json -> Json
tagged tag value =
  Aeson.KeyMap.singleton (Aeson.Key.fromText tag) value
    & ObjectJson

assocList :: [(Text, Json)] -> Json
assocList =
  ObjectJson . fromList . (fmap . first) Aeson.Key.fromText
