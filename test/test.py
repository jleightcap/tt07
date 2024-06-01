#
# Copyright (c) 2024 Jack Leightcap
# SPDX-License-Identifier: Apache-2.0
#

import random
import cocotb
from cocotb.clock import Clock
from cocotb.triggers import ClockCycles

async def setup(dut):
    dut._log.info("Start")

    # Set the clock period to 10 us (100 KHz)
    clock = Clock(dut.clk, 10, units="us")
    cocotb.start_soon(clock.start())

    # Reset
    dut._log.info("Reset")
    dut.ena.value = 1
    dut.accumulator.value = 0
    dut.fraction.value = 0
    dut.rst_n.value = 0
    await ClockCycles(dut.clk, 1)

@cocotb.test()
async def phase_alternate(dut):
    dut._log.info("Test alternating read/write phases")
    await cocotb.start(setup(dut))

    await ClockCycles(dut.clk, 1)
    assert dut.count.value == 0
    assert dut.we == 0

@cocotb.test()
async def signed_unsigned_addition(dut):
    await cocotb.start(setup(dut))

    dut.fraction.value = frac = random.randint(0, 50)
    dut.accumulator.value = acc = random.randint(0,50)
    await ClockCycles(dut.clk, 2)
    assert dut.degree.value == frac + acc

@cocotb.test()
async def fraction_exhausted(dut):
    """
    fraction iterator consumed before accumulator iterator
    """

    await setup(dut)

    dut.fraction.value = 1
    dut.accumulator.value = 2
    await ClockCycles(dut.clk, 2)
    assert dut.degree.value == 3

    # fraction terminates
    dut.fraction.value = 0b11111111
    dut.accumulator.value = 10
    await ClockCycles(dut.clk, 2)
    assert dut.degree.value == 10

    # accumulator also terminates, output terminated
    dut.accumulator.value = 0b11111111
    await ClockCycles(dut.clk, 2)
    assert dut.degree.value == 0b11111111
