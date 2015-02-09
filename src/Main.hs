{-# LANGUAGE OverloadedStrings #-}

-- |

module Main where

import           Cache

import           System.Environment
import           System.IO
import qualified Filesystem as FP

import Control.Applicative
import           Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Foldable

main :: IO ()
main = do
    args <- getArgs
    cout <- dryrun args
    name <- thisPackageName
    case excludePkg name <$> parseOnly dryrunVersions cout of
     Left er -> hPutStrLn stderr er
     Right versions -> do
         makeHscacheDir
         home <- FP.getHomeDirectory
         T.writeFile "hscache.nix" $ nixText home versions
         traverse_ createDerivation versions
