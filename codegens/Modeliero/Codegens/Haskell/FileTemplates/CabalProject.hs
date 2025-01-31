module Modeliero.Codegens.Haskell.FileTemplates.CabalProject
  ( filePath,
    content,
  )
where

import Coalmine.Prelude

filePath :: FilePath
filePath =
  "cabal.project"

content :: Text
content =
  [i|
    packages: .
    
    allow-newer:
      , *:base
      , *:template-haskell
    
    source-repository-package
      type: git
      location: https://github.com/nikita-volkov/modeliero-base
      tag: 88599ec4524a856435b8b89b34deb5387ee8ff5c
  |]
