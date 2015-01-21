-- |

module Main where

import           Cache

import           System.Environment
import           System.IO

import Control.Applicative
import           Data.Attoparsec.Text
import qualified Data.Text as T

main :: IO ()
main = do
    args <- getArgs
    cout <- T.pack <$> dryrun args
    stderrOrStdout $ parseOnly dryrunVersions cout

stderrOrStdout :: Show a => Either String a -> IO ()
stderrOrStdout e = case e of
    Left er -> hPutStrLn stderr er
    Right v -> print v
