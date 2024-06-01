/* AUTOMATICALLY GENERATED VERILOG-2001 SOURCE CODE.
** GENERATED BY CLASH 1.6.3. DO NOT MODIFY.
*/
module Fractran
    ( // Inputs
      input  clk // clock
    , input  rst // reset
    , input  en // enable
    , input [17:0] accumulator

      // Outputs
    , output wire [7:0] we
    , output wire [8:0] degree
    , output wire  halt
    );
  // src/Fractran.hs:134:1-88
  reg [14:0] s1 = {1'd0,   {1'b1,{1'b1,8'b00000000}},   4'b0000};
  wire  c$app_arg;
  wire [7:0] c$app_arg_0;
  wire [8:0] c$app_arg_1;
  wire [0:0] c$app_arg_2;
  // src/Fractran.hs:117:1-6
  wire [8:0] v;
  wire [14:0] c$case_alt_0;
  wire [3:0] c$app_arg_3;
  // src/Fractran.hs:57:7-8
  wire [7:0] ipv;
  wire [9:0] c$case_alt_1;
  // src/Fractran.hs:57:7-8
  wire [7:0] a;
  reg [9:0] c$case_alt_2;
  wire [9:0] result;
  wire [9:0] c$case_alt_3;
  wire [9:0] result_0;
  // src/Fractran.hs:82:1-6
  wire [8:0] f;
  // src/Fractran.hs:82:1-6
  wire [8:0] a_0;
  wire [9:0] c$app_arg_selection_3;
  wire [9:0] c$app_arg_selection_6;
  wire [0:0] c$case_alt_selection_res;
  wire [7:0] c$bv;
  wire [17:0] c$case_alt;

  // register begin
  always @(posedge clk or  posedge  rst) begin : s1_register
    if ( rst) begin
      s1 <= {1'd0,   {1'b1,{1'b1,8'b00000000}},   4'b0000};
    end else if (en) begin
      s1 <= c$case_alt_0;
    end
  end
  // register end

  assign c$app_arg = s1[14:14] ? (1'b1) : (1'b0);

  assign c$app_arg_0 = {(({(c$app_arg),   1'b0,   1'b0,   1'b0})),s1[3:0]};

  assign c$app_arg_selection_3 = s1[13:4];

  assign c$app_arg_1 = c$app_arg_selection_3[9:9] ? v : {1'b1,8'b00000000};

  assign c$app_arg_selection_6 = s1[13:4];

  assign c$app_arg_2 = c$app_arg_selection_6[9:9] ? 1'b0 : 1'b1;

  assign c$case_alt = {c$app_arg_0,
                       c$app_arg_1,   (c$app_arg_2)};

  assign v = s1[12:4];

  assign c$case_alt_0 = s1[14:14] ? {1'd0,
                                     s1[13:4],   c$app_arg_3} : {1'd1,   result_0,
                                                                 c$app_arg_3};

  assign c$app_arg_3 = s1[3:0] + 4'b0001;

  assign ipv = f[7:0];

  assign c$case_alt_1 = a_0[8:8] ? {1'b1,{1'b1,a + ipv}} : c$case_alt_2;

  assign a = a_0[7:0];

  assign c$bv = (ipv);

  assign c$case_alt_selection_res = (( c$bv[8-1] ));

  always @(*) begin
    case(c$case_alt_selection_res)
      1'b1 : c$case_alt_2 = {1'b0,9'bxxxxxxxxx};
      default : c$case_alt_2 = {1'b1,{1'b1,ipv}};
    endcase
  end

  assign result = f[8:8] ? c$case_alt_1 : {1'b1,a_0};

  assign c$case_alt_3 = f[8:8] ? result : {1'b1,{1'b0,8'bxxxxxxxx}};

  assign result_0 = a_0[8:8] ? result : c$case_alt_3;

  assign f = accumulator[8:0];

  assign a_0 = accumulator[17:9];

  assign we = c$case_alt[17:10];

  assign degree = c$case_alt[9:1];

  assign halt = c$case_alt[0:0];


endmodule

