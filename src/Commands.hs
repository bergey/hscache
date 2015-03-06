{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |

module Commands where

import           Cache

import qualified Data.Text as T
import           Imports

build :: Sh ()
build = do
    run_  "nix-shell" ["hscache.nix", "-K", a"-A", "env", "--command", "cabal build" ]

install :: Sh ()
install = shell "nix-env -f hscache.nix -i"

nix_shell :: Sh ()
nix_shell = shell "nix-shell hscache.nix -A env"

exec :: [Text] -> Sh ()
exec cmd = shell $ "nix-shell hscache.nix -A env --command '" <> T.intercalate " " cmd <> "''"

repl :: Sh ()
repl = shell "nix-shell hscache.nix -A env --command ghci"

-- | Call configure if the arguments require it, or if there is no hscache.nix.
-- Return True if configure was needed, otherwise False
configureIfNeeded :: [Text] -> Sh Bool
configureIfNeeded args = case args of
    [] -> do
        exists <- test_f "hscache.nix"
        if exists then return False else do
            echo "hscache.nix does not exist; creating with default options"
            freeze args >> return True
    _ -> do
        echo "reconfiguring with arguments given on command-line"
        freeze args
        return True

-- | Register the specified directories in the local sandbox, and
-- recreate default.nix in each directory.  This function assumes
-- there is already a sandbox in the current working directory.
addSourceCmd :: [FilePath] -> Sh ()
addSourceCmd dirs = do
    dirs' <- traverse toTextWarn dirs
    addSource dirs'
    mapM_ defaultNix dirs
