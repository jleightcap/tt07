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

  wire we, halt;
  wire [5:0] count;
  Fractran ft (
      .clk(clk),
      .rst(rst_n),
      .en(ena),
      .accumulator(uio_in),
      .fraction(ui_in),
      .we(we),
      .degree(uo_out),
      .halt(halt),
      .count(count)
  );
  assign uio_out = {we, halt, count};
  assign uio_oe = {we, we, we, we, we, we, we, we};

endmodule
