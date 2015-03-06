{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |

module Cache where

import           Data.Attoparsec.Text

-- from Cabal
import qualified Distribution.PackageDescription.Parse as C
import qualified Distribution.Verbosity as C
import qualified Distribution.Package as C
import Data.List.Split (splitWhen)

import Imports
import qualified Filesystem                as FP
import qualified Filesystem.Path.CurrentOS as FP
import qualified Data.Text                 as T
import qualified Data.Text.IO              as T
import Prelude (init, last)

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

--( <//>) :: Text -> Text -> Text
-- "" <//> b = b
-- a <//> "" = a
-- a <//> b = case (T.last a, T.head b) of -- order of cases matters
--     ('/', '/') -> T.init a <> T.tail b
--     ('/', _) -> a <> b
--     (_, '/') -> a <> b
--     _ -> mconcat [a, "/", b]

-- | directory where nix derivations are kept
hackageNixPath :: Text
hackageNixPath = ".hscache"

makeHscacheDir :: Sh ()
makeHscacheDir = do
    home <- liftIO $ FP.getHomeDirectory
    mkdir_p $ home </> hackageNixPath

-- derivationPath :: FP.FilePath -> PkgVer -> Text
-- derivationPath home (PkgVer pkg ver) = home' <//> hackageNixPath <//> pkg <//> filename where
--   filename = pkg <> "-" <> ver <> ".nix"
--   home' = case FP.toText home of
--       Left t -> t
--       Right t -> t

-- | the path to a particular derivation
derivationPath :: FP.FilePath -> PkgVer -> FP.FilePath
derivationPath home (PkgVer pkg ver) =
    home </> hackageNixPath </> pkg </> filename where
      filename = pkg <> "-" <> ver <> ".nix"

-- | the nix expression to load a particular derivation from disk
versionedDerivation :: FP.FilePath -> PkgVer -> NixDef
versionedDerivation home pkg = NixDef (_pkgName pkg) def where
  def = mconcat ["self.callPackage \"", toTextIgnore $ derivationPath home pkg, "\" {}"]

-- | check whether a given derivation is already on disk
derivationExists :: FP.FilePath -> PkgVer -> Sh Bool
derivationExists home = test_f . derivationPath home

cabal2nix :: [Text] -> Sh Text
cabal2nix = command "cabal2nix" []

-- | Create @default.nix@ in the specified directory, by calling
-- @cabal2nix@.  Overwrites @default.nix@ if one exists.
defaultNix :: FilePath -> Sh ()
defaultNix dir = chdir dir $ do
    nix <- cabal2nix ["."]
    writefile "default.nix" nix

-- | save a derivation (with cabal2nix) if it's not already on disk
createDerivation :: PkgVer -> Sh ()
createDerivation pkg = do
    home <- liftIO $ FP.getHomeDirectory
    exists <- derivationExists home pkg
    if exists then return () else do
        nix <- cabal2nix ["cabal://" <> textFromPkg pkg]
        let path = derivationPath home pkg
        mkdir_p $ FP.directory path
        writefile (derivationPath home pkg) nix

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

-- | call @cabal install --dry-run@ to get a list of dependencies with
-- appropriate versions.  This runs in a cabal sandbox to handle
-- add-source (local) dependencies, and in a nix-shell to pick the
-- same versions of GHC & Cabal used for other commands.  Both are
-- necessary to pick the correct versions.
dryrun :: [Text] -> Sh Text
dryrun args = do
    cleanSandbox
    -- create dryrun.nix / tempfile
    errExit False $ do
        let cabalCmd = "cabal install --dry-run " <> T.intercalate " " args
        frozen <- run "nix-shell" ["dryrun.nix", "--command", cabalCmd]
        ex <- lastExitCode
        case ex of
         0 -> return frozen
         2 -> do
              echo_err "nix-shell failed with Exit Code 2; What does it mean?"
              return frozen
         seriousErr -> do
             echo_err $ "nix-shell failed with Exit Code " <> T.pack (show seriousErr)
             exit 1

cabalFile :: Sh FilePath
cabalFile = do
    files <- ls =<< liftIO FP.getWorkingDirectory
    case filter (hasExt "cabal") files of
     (h:_) -> return h
     _ -> error "Could not find .cabal file."

thisPackageName :: Sh Text
thisPackageName = do
    fn <- cabalFile
    description <- liftIO $ C.readPackageDescription C.normal . FP.encodeString $ fn
    let (C.PackageName name) = C.packageName description
    return $ T.pack name

freeze :: [Text] -> Sh ()
freeze args = do
    cout <- dryrun args
    name <- thisPackageName
    case excludePkg name <$> parseOnly dryrunVersions cout of
     Left er -> echo_err $ T.pack er
     Right versions -> do
         makeHscacheDir
         home <- liftIO $ FP.getHomeDirectory
         writefile "hscache.nix" $ nixText home versions
         traverse_ createDerivation versions

shell :: Text -> Sh ()
shell t = case T.words t of
    (prog:args) -> run_ (fromText prog) args
    _ -> error "shell recieved empty list (no program to run)"

cabal_ :: Text -> [Text] -> Sh ()
cabal_ = command1_ "cabal" []

cabal :: Text -> [Text] -> Sh Text
cabal = command1 "cabal" []

createSandbox  :: Sh ()
createSandbox = cabal_ "sandbox" ["init"]

addSource :: [Text] -> Sh ()
addSource [] = return ()
addSource dirs = command_ "cabal" ["sandbox", "add-source"] dirs

getAddSource :: Sh [Text]
getAddSource = do
    inSandbox <- test_f "cabal.sandbox.config"
    if inSandbox then do
        cout <- cabal "sandbox" ["list-sources"]
        return $ case splitWhen T.null (T.lines cout) of
            (_:dirs: _) -> dirs -- middle of 3
            _ -> []
        else return []

cleanSandbox :: Sh ()
cleanSandbox = do
    dirs <- getAddSource
    cabal_ "sandbox" ["delete"]
    createSandbox
    addSource dirs

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
\  }))\n\
\"
