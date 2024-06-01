{-
 - Copyright (c) 2024 Jack Leightcap
 - SPDX-License-Identifier: Apache-2.0
 -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}

module Fractran where

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
-- 	- 2) writeback value to degree RAM bank
-- add together the unsigned value acc and the signed value frac.
-- 	- if any value is negative, don't toggle the banks

import Clash.Prelude
import Data.Maybe (fromMaybe, isNothing)

--
-- two inputs from ROM/RAM
-- 1. 'accumulator', natural number, iterated by (only negative) prime term degrees
-- 2. 'fraction', ...fractional, iterated by signed prime term degrees

-- (1) accumulator iterator: exhausted, a positive value, or flag a negative degree
data AIter = ADone | A (BitVector 8) deriving (Generic, NFDataX)

instance BitPack AIter where
  type BitSize AIter = 8
  pack ADone = 0b11111111
  pack (A a) = pack a
  unpack 0b11111111 = ADone
  unpack a = A a

-- (2) fraction iterator
data FIter = FDone | F (BitVector 8) deriving (Generic, NFDataX)

instance BitPack FIter where
  type BitSize FIter = 8
  pack FDone = 0b11111111
  pack (F f) = pack f
  unpack 0b11111111 = FDone
  unpack f = F f

-- compare two prime term degrees: sum of degrees is degree if product
-- - AIter degrees are 8-bit unsigned with two reserved values, in [0,255 - 2]
-- - FIter degrees are 8-bit one's complement with "second zero" acting as sentinel, in [-127,127]
(%+) :: AIter -> FIter -> Maybe AIter
ADone %+ FDone = Just ADone
a %+ FDone = Just a
-- FIXME: these two cases, addition of one's complement signed and unsigned numbers
ADone %+ (F f) = if bitToBool (msb f) then Nothing else Just (A f)
(A a) %+ (F f) = Just sum
  where
    sum = A (a + f)

-- moore machine:
-- 1. define iterated State type
-- 2. function State -> Input -> State
-- 3. function State -> Output

-- (1) define State, contains all data needed to compute Output and next State
data Phase = Read | Write deriving (Generic, NFDataX)

data State = MkState
  { phase :: Phase,
    term :: Maybe AIter,
    count :: BitVector 4
  }
  deriving (Generic, NFDataX)

-- (2) process State based on input iterators
mooreF :: State -> (AIter, FIter) -> State
mooreF MkState {phase = Write, term, count = c'} _ =
  MkState
    { phase = Read,
      term,
      count = c' + 1
    }
mooreF MkState {phase = Read, term = _, count = c'} (a, f) =
  MkState
    { phase = Write,
      term = a %+ f,
      count = c' + 1
    }

-- (3) process State into Output
data Control = MkCntl
  { -- RAM write enable
    we :: Bit,
    -- [DEBUG] prime index
    idx :: BitVector 4
  }

instance BitPack Control where
  type BitSize Control = 8
  pack MkCntl {we, idx} = pack (we :> 0 :> 0 :> 0 :> Nil) ++# idx
  unpack = undefined

data Output = MkOut
  { cntl :: BitVector 8,
    -- computed factor degree
    degree :: AIter,
    -- flag negative result, abort iteration
    halt :: Bit
  }

mooreO :: State -> Output
mooreO MkState {phase, term = t', count} =
  MkOut
    { cntl =
        pack $
          MkCntl
            { we = case phase of
                Read -> low
                Write -> high,
              idx = count
            },
      degree = fromMaybe (A 0) t', -- unused value in case of halt, unwrap with dummy value
      halt = boolToBit $ isNothing t'
    }

--
-- toplevel: wrap moore machine into clock domain, generate toplevel
--
mooreM :: (HiddenClockResetEnable dom) => Signal dom (AIter, FIter) -> Signal dom Output
mooreM = moore mooreF mooreO s0
  where
    s0 = MkState {phase = Read, term = Just (A 0), count = 0}

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
          (PortProduct "" [PortName "we", PortName "degree", PortName "halt"])
      }
  )
  #-}
topEntity ::
  ( Clock System,
    Reset System,
    Enable System,
    Signal System (AIter, FIter)
  ) ->
  Signal System Output
topEntity (clk, rst, en, iters) = exposeClockResetEnable (mooreM iters) clk rst en
