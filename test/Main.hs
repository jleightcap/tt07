{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Clash.Prelude

-- propositions over the register machine.

import Control.Monad (void)
import Fractran
import Test.QuickCheck

prop_foo :: Bool -> Bool
prop_foo _ = True

return []

main :: IO ()
main = void $quickCheckAll
