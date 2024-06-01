{-
 - Copyright (c) 2024 Jack Leightcap
 - SPDX-License-Identifier: Apache-2.0
 -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}

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
ADone %+ (F f) = if msb f == low then Just (A f) else Nothing
a %+ FDone = Just a
(A a) %+ (F f)
  | msb f == low = Just $ A ((a + f) `mod` (0b1111111 - 1))
  | a >= negate f = Just $ A (a - negate f + 1)
  | otherwise = Nothing

-- moore machine:
-- 1. define iterated State type
-- 2. function State -> Input -> State
-- 3. function State -> Output

-- (1) define State, contains all data needed to compute Output and next State
data Phase = Read | Write deriving (Generic, NFDataX)

data State = MkState
  { phase :: Phase,
    result :: Maybe AIter,
    count :: BitVector 6
  }
  deriving (Generic, NFDataX)

-- (2) process State based on input iterators
mooreF :: State -> (AIter, FIter) -> State
mooreF MkState {phase = Write, result, count} (a, f) =
  MkState
    { phase = Read,
      result = a %+ f,
      count = count
    }
mooreF MkState {phase = Read, result, count} (a, f) =
  MkState
    { phase = Write,
      result = a %+ f,
      count = count + 1
    }

-- (3) process State into Output
data Output = MkOut
  { -- computed factor degree
    degree :: BitVector 8,
    -- RAM write enable
    we :: Bit,
    -- flag negative result, abort iteration
    halt :: Bit,
    -- [DEBUG] prime index
    ocount :: BitVector 6
  }

mooreO :: State -> Output
mooreO MkState {phase, result, count} =
  MkOut
    { degree = pack $ fromMaybe (A 0) result,
      we = case phase of
        Read -> low
        Write -> high,
      halt = boolToBit $ isNothing result,
      ocount = count
    }

--
-- toplevel: wrap moore machine into clock domain, generate toplevel
--
mooreM :: (HiddenClockResetEnable dom) => Signal dom (AIter, FIter) -> Signal dom Output
mooreM = moore mooreF mooreO s0
  where
    s0 = MkState {phase = Read, result = Just (A 0), count = 0}

{-# ANN
  topEntity
  ( Synthesize
      { t_name = "Fractran",
        t_inputs =
          [ PortName "accumulator",
            PortName "fraction",
            PortName "clk",
            PortName "rst",
            PortName "en"
          ],
        t_output =
          (PortProduct "" [PortName "degree", PortName "we", PortName "halt", PortName "count"])
      }
  )
  #-}
topEntity ::
  Signal System (BitVector 8) ->
  Signal System (BitVector 8) ->
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System Output
topEntity acc frac = exposeClockResetEnable (mooreM input)
  where
    input = liftA2 (\a b -> (unpack a, unpack b)) acc frac
