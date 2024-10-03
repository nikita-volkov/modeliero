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
      tag: 6a758107eb363409ae15cc76437eff8516e4c4aa
  |]
