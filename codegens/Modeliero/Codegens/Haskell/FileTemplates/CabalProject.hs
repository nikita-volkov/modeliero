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
      tag: bb60d2ec7de0d8966210e9857e554aeb237e6cc2
  |]
