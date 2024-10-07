# Swiss army knife for model code-generation (in beta)

Universal data-model code-generation CLI application. It reads specs from various formats and generates code for various languages.

# Design

This project is designed to be scalable in two directions:

1. Supported input formats, e.g.:
    - JSON Schema
    - OpenAPI
    - AsyncAPI
    - TypeSpec
2. Supported generation targets:
    - Code for various programming languages
    - Other specs (for conversion purposes)

# Status

The project is in a working state: it can read a schema and generate code. It is well covered with tests, however it is not yet battle-tested in any large production project and it is limited in features.

## Supported spec sources

- ### AsyncAPI

## Supported generation targets

- ### Haskell library
    
    Provides data types and instances exposed as one module with no naming conflicts.
    
    The library is designed to compile fast, which it achieves by completely avoiding Generic and Template Haskell and by optimizing its structure for parallel compilation. It defines every type with its instances in a separate module and then reexports them all from a dome module which is fast to compile.

# Instructions

## Installing the CLI application

### From source

1. Install the Cabal build tool by following [the official instructions](https://www.haskell.org/cabal/)

2. Clone the application source:

    ```bash
    git clone https://github.com/nikita-volkov/modeliero.git
    ```

3. Install the application:

    ```bash
    cd modeliero
    cabal install
    ```

## Using

### Generating Haskell Code

From a directory containing a file named `asyncapi.yaml` execute:

```bash
modeliero
```

The `modeliero-artifacts` directory will get generated. You'll find the generated code within.

The AsyncAPI standard is supported in a limited set of combinations. See the [Designing AsyncAPI](#designing-asyncapi) section.

### Designing compatible AsyncAPI

The AsyncAPI standard is supported in a limited set of combinations. It lets you design complete models, which includes products, sums, vectors, maybes and maps. It may not support some valid AsyncAPI schemas yet.

To ensure compatibility with this codegen you should design new models by following the patterns that are described further.

#### Product Types

```yaml
components:
  schemas:
    two-properties-with-first-required:
      type: object
      required: [my-property1]
      properties:
        my-property1:
          type: string
        my-property2:
          type: string
```

maps to Haskell as:

```haskell
data TwoPropertiesWithFirstRequired = TwoPropertiesWithFirstRequired
  { myProperty1 :: Text.Text,
    myProperty2 :: Maybe Text.Text
  }
```

Do notice how the `required` section is used to encode nullability.

#### Sum Types

We interpret a pattern where tagging objects are defined as options in `oneOf`. The example explains it best:

```yaml
components:
  schemas:
    one-of-two:
      oneOf:
        - type: object
          required: [one] -- The tagging property must be required
          properties: -- Strictly single property
            one: -- Tag name
              type: string
        - type: object
          required: [two]
          properties:
            two:
              type: boolean
```

maps to Haskell as:

```haskell
data OneOfTwo
  = OneOneOfTwo Text
  | TwoOneOfTwo Bool
```

In the generated code the constructors are disambiguated using the following pattern: the name of the tag is suffixed by the type name.

#### Enums

```yaml
components:
  schemas:
    my-enum:
      enum:
        - option-one
        - option-two
```

maps to Haskell as:

```haskell
data MyEnum
  = OptionOneMyEnum
  | OptionTwoMyEnum
```

In the generated code the constructors are disambiguated using the same pattern as with sums: the name of the tag is suffixed by the type name.

#### Arrays

```yaml
components:
  schemas:
    in-outline-position:
      type: array
      items:
        type: string

    in-inline-position:
      type: object
      required: [my-field2]
      properties:
        my-field1:
          type: array
          items:
            type: string
        my-field2:
          type: array
          items:
            type: array
            items:
              type: string
```

```haskell
newtype InOutlinePosition = InOutlinePosition
  { base :: Vector Text
  }

data InInlinePosition = InInlinePosition
  { myField1 :: Maybe (Vector Text),
    myField2 :: Vector (Vector Text)
  }
```

#### Dictionaries

Not supported yet.

#### Primitives

All primitives are supported.
