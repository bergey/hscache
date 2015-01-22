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
pkgFromText t = case filter (/= "") $ T.split isSpace t of
    (t':_) ->  case T.split (=='-') t of
        [] -> empty
        [_singleton] -> empty
        parts -> pure $ PkgVer (T.intercalate "-" (init parts)) (last parts)
    _ -> empty

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
hackageNixPath = "/home/bergey/code/nixHaskellVersioned/"

-- | the path to a particular derivation
derivationPath :: PkgVer -> Text
derivationPath (PkgVer pkg ver)= hackageNixPath </> pkg </> ver <> ".nix"

-- | the nix expression to load a particular derivation from disk
versionedDerivation :: PkgVer -> NixDef
versionedDerivation pkg = NixDef (_pkgName pkg) def where
  def = mconcat ["self.callPackage ", derivationPath pkg, "{}"]

-- | check whether a given derivation is already on disk
derivationExists :: PkgVer -> IO Bool
derivationExists = FP.isFile . FP.fromText . derivationPath

-- | save a derivation (with cabal2nix) if it's not already on disk
createDerivation :: PkgVer -> IO ()
createDerivation pkg = do
    exists <- derivationExists pkg
    if exists then return () else do
        nix <- readProcess "cabal2nix" [T.unpack $ "cabal://" <> textFromPkg pkg] ""
        let path = FP.fromText $ derivationPath pkg
        FP.createDirectory True (FP.directory path)
        writeFile (T.unpack $ derivationPath pkg) nix

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

dryrun :: [String] -> IO String
dryrun args = readProcess "cabal"
              ("install" : "--dry-run" : "--package-db=clear" : "--package-db=global" : args) ""

cabalFile :: IO FP.FilePath
cabalFile = do
    files <- FP.listDirectory =<< FP.getWorkingDirectory
    return . head $ filter (flip FP.hasExtension "cabal") files

thisPackageName :: IO Text
thisPackageName = do
    description <- C.readPackageDescription C.normal . FP.encodeString =<< cabalFile
    let (C.PackageName name) = C.packageName description
    return $ T.pack name

nixText :: [PkgVer] -> Text
nixText pkgs = mconcat [header, pinnedDeps, footer] where
  pinnedDeps = mconcat . fmap (textFromNix . versionedDerivation) $ pkgs
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
