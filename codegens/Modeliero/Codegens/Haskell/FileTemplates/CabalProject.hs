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
      tag: 370393cea81635f0594780566ee565cd6db53a36
  |]
