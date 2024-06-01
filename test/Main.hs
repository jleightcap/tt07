{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Clash.Prelude
-- propositions over the register machine.

import Control.Monad (void)
import Fractran
import Test.QuickCheck

instance Arbitrary AIter where
  arbitrary = arbitrary ADone

prop_pack :: AIter -> Bool
prop_pack _ = True

return []

main :: IO ()
main = void $quickCheckAll
