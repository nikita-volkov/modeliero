-- | Model of the parameters.
module Modeliero.Codegens.Haskell.Params where

import Coalmine.Prelude

data Model = Model
  { name :: Slug,
    version :: NonEmpty Word,
    types :: [TypeDeclaration],
    instances :: Instances
  }

-- ** TypeDeclaration

data TypeDeclaration = TypeDeclaration
  { name :: Slug,
    docs :: Text,
    definition :: TypeDefinition
  }

data TypeDefinition
  = ProductTypeDefinition [Field]
  | RefinedTypeDefinition Refinement

-- *** Product

data Field = Field
  { name :: Slug,
    docs :: Text,
    type_ :: FieldType
  }

data FieldType
  = PlainFieldType PlainType
  | MaybeFieldType PlainType
  | VectorFieldType PlainType
  | HashMapFieldType PlainType PlainType

data PlainType
  = -- | Reference to a type defined in this model.
    LocalPlainType Slug
  | -- | One of the finite set of standard types with extra support.
    StandardPlainType StandardType
  | -- | Any type from any package with a limited support.
    CustomPlainType CustomType

data StandardType
  = BoolStandardType
  | IntStandardType
  | DoubleStandardType
  | ScientificStandardType
  | TextStandardType
  | UuidStandardType
  | DayStandardType
  | UtcTimeStandardType
  | LocalTimeStandardType
  | ZonedTimeStandardType
  | TimeZoneStandardType
  | EmailStandardType
  | UrlStandardType

data CustomType = CustomType
  { packageName :: Text,
    minVersion :: NonEmpty Word,
    maxVersion :: NonEmpty Word,
    module_ :: Text,
    name :: Text
  }

-- *** Refinement

data Refinement
  = TextRefinement TextRestrictions
  | IntegerRefinement IntegerRestrictions
  | DoubleRefinement DoubleRestrictions

data TextRestrictions = TextRestrictions
  { minLength :: Maybe Int,
    maxLength :: Maybe Int,
    charsetRangeList :: Maybe [(Char, Char)],
    regexp :: Maybe Text
  }

data IntegerRestrictions = IntegerRestrictions
  { min :: Maybe Integer,
    max :: Maybe Integer
  }

data DoubleRestrictions = DoubleRestrictions
  { min :: Maybe Double,
    max :: Maybe Double
  }

-- ** Instances

data Instances = Instances
  { show :: Bool,
    eq :: Bool,
    ord :: Bool,
    generic :: Bool,
    aeson :: Maybe Aeson,
    arbitrary :: Bool
  }

data Aeson = Aeson
  { casing :: Casing
  }

data Casing
  = CamelCasing
  | SnakeCasing
  | KebabCasing
