{-
 - Copyright (c) 2024 Jack Leightcap
 - SPDX-License-Identifier: Apache-2.0
 -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Fractran where

import Clash.Prelude

data PrimeStream = PS (BitVector 8)

instance Num PrimeStream where
    (+) (PS a) (PS b) = PS (a + b)

type Stream = BitVector 8

topEntity :: ( "clk" ::: Clock System
             , "rst" ::: Reset System
             , "acc_i" ::: Signal System PrimeStream
             , "frac" ::: Signal System PrimeStream
             ) -> "acc_o" ::: Signal System PrimeStream
topEntity ( clk, rst, acc, frac ) = (+) <$> acc <*> frac
