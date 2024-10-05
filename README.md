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

The project is in a working state: it can read a schema and generate code. It is well covered with tests, however it is not yet battle-tested in any large production project and it is limited in features

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

In CLI or shell execute:

```bash
modeliero PATH_TO_ASYNC_API_SPEC
```

The `modeliero-artifacts` directory will get generated. You'll find the generated code within.
