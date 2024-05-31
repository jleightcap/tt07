/*
 * Copyright (c) 2024 Jack Leightcap
 * SPDX-License-Identifier: Apache-2.0
 */

`default_nettype none

module tt_um_jleightcap (
    input  wire [7:0] ui_in,    // Dedicated inputs
    output wire [7:0] uo_out,   // Dedicated outputs
    input  wire [7:0] uio_in,   // IOs: Input path
    output wire [7:0] uio_out,  // IOs: Output path
    output wire [7:0] uio_oe,   // IOs: Enable path (active high: 0=input, 1=output)
    input  wire       ena,      // always 1 when the design is powered, so you can ignore it
    input  wire       clk,      // clock
    input  wire       rst_n     // reset_n - low to reset
);

  assign uio_oe = 8'b00000000;
  assign uio_out = 0;
  topEntity tt(clk, ~rst_n, ui_in, uio_in, uo_out);

  // List all unused inputs to prevent warnings
  wire _unused = &{ena, 1'b0};

endmodule
