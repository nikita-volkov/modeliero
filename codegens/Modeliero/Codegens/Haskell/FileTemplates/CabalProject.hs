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
      tag: f20a39e640db2d3cd7a501eaab8a8c3b5c4b535e
  |]
