module Modeliero.Codegens.Haskell.Dsls.Package where

import Coalmine.NumericVersion qualified as NumericVersion
import Coalmine.Prelude hiding (writeFile)
import CodegenKit.Legacy.ByLanguage.Haskell.Composers.Cabal qualified as Cabal
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO

data File = File
  { path :: FilePath,
    content :: Text
  }

data Package = Package
  { name :: Text,
    synopsis :: Text,
    version :: NumericVersion,
    modules :: Modules
  }

data Modules = Modules
  { public :: [Module],
    private :: [Module]
  }

data Module = Module
  { name :: [Text],
    dependencies :: [Dependency],
    content :: Text
  }

data Dependency = Dependency
  { name :: Text,
    minVersion :: NumericVersion,
    maxVersion :: NumericVersion
  }
  deriving (Eq, Ord, Show, Generic, Hashable)

writePackage :: FilePath -> Package -> IO ()
writePackage rootPath package =
  forM_ (compilePackageFiles package) \file ->
    Text.IO.writeFile
      (mconcat [rootPath, "/", file.path])
      file.content

-- | Compile package to files.
compilePackageFiles :: Package -> [File]
compilePackageFiles package =
  compilePackageCabalFile package : compilePackageModules package

compilePackageCabalFile :: Package -> File
compilePackageCabalFile package =
  File
    { path = (package.name <> ".cabal") & toList,
      content =
        Cabal.contents
          (Cabal.plainPackageName package.name)
          package.synopsis
          (Cabal.listVersion package.version.head package.version.tail)
          ( package.modules.public
              & fmap
                ( \module_ ->
                    module_.name
                      & Text.intercalate "."
                      & Cabal.plainModuleRef
                )
          )
          ( package.modules.private
              & fmap
                ( \module_ ->
                    module_.name
                      & Text.intercalate "."
                      & Cabal.plainModuleRef
                )
          )
          (compileCabalDependencies package)
    }

compileDependencyMap :: Package -> Map Text (NumericVersion, NumericVersion)
compileDependencyMap package =
  (package.modules.public <> package.modules.private)
    & concatMap (.dependencies)
    & fmap
      ( \dependency ->
          (dependency.name, (dependency.minVersion, dependency.maxVersion))
      )
    & Map.fromListWith
      ( \(minVersionL, maxVersionL) (minVersionR, maxVersionR) ->
          ( max minVersionL minVersionR,
            min maxVersionL maxVersionR
          )
      )

compileCabalDependencies :: Package -> [Cabal.Dependency]
compileCabalDependencies =
  fmap
    ( \(packageName, (minVersion, maxVersion)) ->
        Cabal.rangeDependency
          (Cabal.plainPackageName packageName)
          (Cabal.listVersion minVersion.head minVersion.tail)
          (Cabal.listVersion maxVersion.head maxVersion.tail)
    )
    . Map.toList
    . compileDependencyMap

compilePackageModules :: Package -> [File]
compilePackageModules package =
  (package.modules.public <> package.modules.private)
    & fmap
      ( \module_ ->
          File
            { path =
                module_.name
                  & Text.intercalate "/"
                  & mappend "library/"
                  & flip mappend ".hs"
                  & toList,
              content = module_.content
            }
      )
