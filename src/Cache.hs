{-# LANGUAGE OverloadedStrings #-}

-- |

module Cache where

import Prelude hiding (FilePath)
import           Data.Attoparsec.Text

-- from Cabal
import qualified Distribution.PackageDescription.Parse as C
import qualified Distribution.Verbosity as C
import qualified Distribution.PackageDescription as C
import qualified Distribution.Package as C

import qualified Filesystem as FP
import qualified Filesystem.Path.CurrentOS as FP
import           System.Environment
import           System.Process

import           Control.Applicative
import           Data.Char (isSpace)
import           Data.Monoid
import qualified Data.Text as T
import           Data.Text (Text)

-- | A haskell package and a version string.  We don't interpret the
-- version string, so we don't bother parsing its parts.
data PkgVer = PkgVer {
    _pkgName :: Text,
    _pkgVer :: Text
    } deriving Show

pkgFromText :: Alternative m => Text -> m PkgVer
pkgFromText t = case T.split (=='-') . T.takeWhile (not . isSpace) $ t of
    [] -> empty
    [_singleton] -> empty
    parts -> pure $ PkgVer (T.intercalate "-" (init parts)) (last parts)

textFromPkg :: PkgVer -> Text
textFromPkg (PkgVer name ver) = mconcat [name, "-", ver]

excludePkg :: Text -> [PkgVer] -> [PkgVer]
excludePkg name = filter (\p -> _pkgName p /= name)

-- | A Nix let binding
data NixDef = NixDef {
    _attrName :: Text,
    _attrValue :: Text
    }

textFromNix :: NixDef -> Text
textFromNix n = mconcat [_attrName n, " = ", _attrValue n, ";\n"]
(</>) :: Text -> Text -> Text
"" </> b = b
a </> "" = a
a </> b = case (T.last a, T.head b) of -- order of cases matters
    ('/', '/') -> T.init a <> T.tail b
    ('/', _) -> a <> b
    (_, '/') -> a <> b
    _ -> mconcat [a, "/", b]

-- | directory where nix derivations are kept
hackageNixPath :: Text
hackageNixPath = ".hscache"

makeHscacheDir :: IO ()
makeHscacheDir = do
    home <- FP.getHomeDirectory
    FP.createDirectory True $ FP.append home $ FP.fromText hackageNixPath

-- | the path to a particular derivation
derivationPath :: FP.FilePath -> PkgVer -> Text
derivationPath home (PkgVer pkg ver) = home' </> hackageNixPath </> pkg </> filename where
  filename = pkg <> "-" <> ver <> ".nix"
  home' = case FP.toText home of
      Left t -> t
      Right t -> t

derivationPath' :: FP.FilePath -> PkgVer -> FP.FilePath
derivationPath' home (PkgVer pkg ver) =
    FP.append home . FP.fromText $ hackageNixPath </> pkg </> filename where
      filename = pkg <> "-" <> ver <> ".nix"

-- | the nix expression to load a particular derivation from disk
versionedDerivation :: FP.FilePath -> PkgVer -> NixDef
versionedDerivation home pkg = NixDef (_pkgName pkg) def where
  def = mconcat ["self.callPackage \"", derivationPath home pkg, "\" {}"]

-- | check whether a given derivation is already on disk
derivationExists :: FP.FilePath -> PkgVer -> IO Bool
derivationExists home = FP.isFile . FP.fromText . derivationPath home

-- | save a derivation (with cabal2nix) if it's not already on disk
createDerivation :: PkgVer -> IO ()
createDerivation pkg = do
    home <- FP.getHomeDirectory
    exists <- derivationExists home pkg
    if exists then return () else do
        nix <- readProcess "cabal2nix" [T.unpack $ "cabal://" <> textFromPkg pkg] ""
        let path = derivationPath' home pkg
        FP.createDirectory True (FP.directory path)
        writeFile (FP.encodeString $ derivationPath' home pkg) nix

dryrunVersions :: Parser [PkgVer]
dryrunVersions = dryrunHeader *> many versionLine

dryrunHeader :: Parser ()
dryrunHeader = manyTill anyLine depsIntro *> pure ()

anyLine :: Parser Text
anyLine = takeTill isEndOfLine <* endOfLine

depsIntro :: Parser Text
depsIntro = string "In order, the following would be installed (use -v for more details):" <* endOfLine

versionLine :: Parser PkgVer
versionLine = pkgFromText =<< anyLine

dryrun :: [String] -> IO Text
dryrun args = T.pack <$> readProcess "cabal" (defArgs ++ args) "" where
  defArgs = ["install", "--dry-run"
            , "--package-db=clear", "--package-db=global"
            , "--constraint=transformers installed"]

cabalFile :: IO FP.FilePath
cabalFile = do
    files <- FP.listDirectory =<< FP.getWorkingDirectory
    return . head $ filter (flip FP.hasExtension "cabal") files

thisPackageName :: IO Text
thisPackageName = do
    description <- C.readPackageDescription C.normal . FP.encodeString =<< cabalFile
    let (C.PackageName name) = C.packageName description
    return $ T.pack name

nixText :: FP.FilePath -> [PkgVer] -> Text
nixText home pkgs = mconcat [header, pinnedDeps, footer] where
  pinnedDeps = mconcat . fmap (textFromNix . versionedDerivation home) $ pkgs
  header = "{ pkgs ? import <nixpkgs> {}, haskellPackages ? pkgs.haskellngPackages }:\n\n\
\let\n\
\  hs = haskellPackages.override {\n\
\        overrides = self: super: rec {\n"
  footer = "          thisPackage = self.callPackage ./. {};\n\
\      };\n\
\    };\n\
\  in (hs.thisPackage.override (args: args // {\n\
\    mkDerivation = expr: args.mkDerivation (expr // {\n\
\      buildTools = [  hs.cabal-install ];\n\
\    });\n\
\  })).env\n\
\"
