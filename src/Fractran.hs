{-
 - Copyright (c) 2024 Jack Leightcap
 - SPDX-License-Identifier: Apache-2.0
 -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Fractran where

import Clash.Prelude

type Stream = BitVector 8

topEntity :: ( "clk" ::: Clock System
             , "rst" ::: Reset System
             , "acc_i" ::: Signal System Stream
             , "frac" ::: Signal System Stream
             ) -> "acc_o" ::: Signal System Stream
topEntity ( clk, rst, acc, frac ) = (+) <$> acc <*> frac
