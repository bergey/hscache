{-# LANGUAGE OverloadedStrings #-}

-- |

module Main where

import           Cache
import Commands

import qualified System.Process as P
import Options.Applicative

import qualified Data.Text                 as T
import qualified Data.Text.IO              as T

import Shelly (shelly, Sh, fromText, FilePath)
import Data.Monoid (mconcat)
import Prelude hiding (FilePath)

main :: IO ()
main = execParser hscacheInfo >>= shelly . hscache

hscache :: Command -> Sh ()
hscache (Freeze args) = freeze args
hscache (AddSource dirs) = addSourceCmd dirs
hscache Build = build
hscache Install = install
hscache (Exec args) = exec args
hscache Repl = repl
hscache Shell = nix_shell

hscacheInfo :: ParserInfo Command
hscacheInfo = info (helper <*> commandParser)
              (fullDesc
              <> progDesc "Fast sandboxed Haskell builds.  Sandboxed builds of Haskell packages, using Cabal's dependency solver.  Cache built packages indexed by all their transitive dependencies."
              <> header "hscache - fast sandboxed Haskell builds")

data Command
    = Freeze [T.Text]
    | AddSource [FilePath]
    | Build
    | Install
    | Exec [T.Text]
    | Repl
    | Shell

commandParser :: Parser Command
commandParser =
    subparser $ mconcat
    [ command "freeze" $
      info (Freeze  <$> many (argument text (metavar "CONSTRAINTS..."))) $
      progDesc "Pick versions for all dependencies."

    , command "add-source" $
      info (AddSource <$> some (argument filepath (metavar "DIRS..."))) $
      progDesc "Make one or more local package available."

    , command "build" $
      info (pure Build) $
      progDesc "compile all configured components"

    , command "install" $
      info (pure Install) $
      progDesc "install executables on user path"

    , command "exec" $
      info (Exec <$> some (argument text (metavar "COMMANDS..."))) $
      progDesc "Give a command access to the sandbox package repository."

    , command "repl" $
      info (pure Repl) $
      progDesc "Open GHCi with access to sandbox packages."

    , command "shell" $
      (info (pure Shell) $
      progDesc "Launch a sub-shell with access to sandbox packages.")
    ]

filepath :: ReadM FilePath
filepath = fromText . T.pack <$> str

text :: ReadM T.Text
text = T.pack <$> str
