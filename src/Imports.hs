-- | Common imports; a sort of alternate Prelude for this project
-- alone.  Intended to be used with NoImplicitPrelude

module Imports
       ( module Prelude
       , module Shelly
       , module Control.Applicative
       , module Data.Char
       , module Data.Foldable
       , module Data.Traversable
       , module Data.Monoid
       , Text
       ) where

import           Prelude             (Bool (..), Double, Either (..), Eq (..),
                                      Float, Floating (..), Fractional (..),
                                      Functor (..), IO, Int, Integer,
                                      Monad (..), Num (..), Ord (..), Real (..),
                                      Show (..), const, error, filter, flip,
                                      fmap, length, not, realToFrac, return,
                                      round, zip, (!!), ($), (.), (/), (=<<))

import           Shelly              hiding (find)

import           Control.Applicative
import           Data.Char           (isSpace)
import           Data.Foldable
import Data.Traversable
import           Data.Monoid
import           Data.Text           (Text)
