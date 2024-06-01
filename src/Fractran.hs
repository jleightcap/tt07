{-
 - Copyright (c) 2024 Jack Leightcap
 - SPDX-License-Identifier: Apache-2.0
 -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Fractran where

import Clash.Prelude

type State = Bit

type AStream = BitVector 8

type FStream = BitVector 8

type SStream = BitVector 8

type OStream = (Bit, SStream)

mooreF :: State -> (AStream, FStream) -> State
mooreF _ _ = low

mooreO :: State -> OStream
mooreO _ = (low, 0)

mooreM :: (HiddenClockResetEnable dom) => Signal dom (AStream, FStream) -> Signal dom OStream
mooreM = moore mooreF mooreO low

-- ROM with program
-- 	- program counter points to current factor degree of fraction
-- 	- 8-bit unsigned, 0b11111111 reseved as sentinel 'STOP' value
-- 	- program counter incremenets with clock, pauses at STOP
-- 	- break from STOP either un-pauses counter, or resets to zero
-- have two banks of writable memory for the accumulator, with flip flop toggle output enable
-- 	- both banks share the same address register and data bus
-- 	- accumulator counter points to
-- two phase:
-- 	- 1) read value from ROM and accumulate RAM bank: add terms
-- 	- 2) writeback value to scratch RAM bank
-- add together the unsigned value acc and the signed value frac.
-- 	- if any value is negative, don't toggle the banks
{-# ANN
  topEntity
  ( Synthesize
      { t_name = "Fractran",
        t_inputs =
          [ PortProduct
              ""
              [ PortName "clk",
                PortName "rst",
                PortName "en",
                PortName "accumulator",
                PortName "fraction"
              ]
          ],
        t_output =
          (PortProduct "" [PortName "oe", PortName "scratch"])
      }
  )
  #-}
topEntity ::
  ( Clock System,
    Reset System,
    Enable System,
    Signal System (AStream, FStream)
  ) ->
  Signal System OStream
topEntity (clk, rst, en, degree) = exposeClockResetEnable (mooreM degree) clk rst en
