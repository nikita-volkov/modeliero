module Modeliero.Codegens.Haskell.CompilersOf.IsLiteral where

import Coalmine.Prelude
import Modeliero.Codegens.Haskell.Params

valueType :: ValueType -> Bool
valueType = \case
  PlainValueType a -> plainType a
  _ -> False

plainType :: PlainType -> Bool
plainType = \case
  StandardPlainType a -> standardType a
  _ -> False

standardType :: StandardType -> Bool
standardType =
  [ TextStandardType,
    UuidStandardType,
    DayStandardType,
    UtcTimeStandardType,
    LocalTimeStandardType,
    ZonedTimeStandardType,
    TimeZoneStandardType,
    EmailStandardType,
    UriStandardType,
    IriStandardType,
    HostnameStandardType,
    IpStandardType,
    IpV4StandardType,
    IpV6StandardType
  ]
    & flip elem
