/* AUTOMATICALLY GENERATED VERILOG-2001 SOURCE CODE.
** GENERATED BY CLASH 1.6.3. DO NOT MODIFY.
*/
module topEntity
    ( // Inputs
      input  c$ds // clock
    , input  c$ds_0 // reset
    , input [7:0] c$ds_1
    , input [7:0] c$ds_2

      // Outputs
    , output wire [7:0] c$case_alt
    );
  // src/Fractran.hs:19:1-9
  wire [7:0] acc;
  // src/Fractran.hs:19:1-9
  wire [7:0] frac;

  assign c$case_alt = acc + frac;

  assign acc = c$ds_1;

  assign frac = c$ds_2;


endmodule

