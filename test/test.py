#
# Copyright (c) 2024 Jack Leightcap
# SPDX-License-Identifier: Apache-2.0
#

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
    await ClockCycles(dut.clk, 1)
    assert dut.count.value == 1
    assert dut.we != 0

@cocotb.test()
async def signed_unsigned_addition(dut):
    await cocotb.start(setup(dut))

    dut.fraction.value = 19
    dut.accumulator.value = 38
    await ClockCycles(dut.clk, 2)
    assert dut.degree.value == 19 + 38

@cocotb.test()
async def fraction_exhausted(dut):
    """
    fraction iterator consumed before accumulator iterator
    """

    await cocotb.start(setup(dut))

    dut.fraction.value = 1
    dut.accumulator.value = 2
    await ClockCycles(dut.clk, 1)
