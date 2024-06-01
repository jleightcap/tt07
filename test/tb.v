/*
 * Copyright (c) 2024 Jack Leightcap
 * SPDX-License-Identifier: Apache-2.0
 */

`default_nettype none `timescale 1ns / 1ps

module tb ();

  initial begin
    $dumpfile("tb.vcd");
    $dumpvars(0, tb);
    #1;
  end

  reg clk;
  reg rst_n;
  reg ena;
  reg [7:0] fraction;
  reg [7:0] accumulator;
  wire [7:0] degree;
  wire we, halt;
  wire [5:0] count;

  tt_um_jleightcap user_project (
`ifdef GL_TEST
      .VPWR(1'b1),
      .VGND(1'b0),
`endif
      .ui_in(fraction),
      .uo_out(degree),
      .uio_in(accumulator),
      .uio_out({we, halt, count}),
      .uio_oe({we, we, we, we, we, we, we, we}),
      .ena(ena),
      .clk(clk),
      .rst_n(rst_n)
  );
endmodule
