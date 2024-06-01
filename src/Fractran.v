/* AUTOMATICALLY GENERATED VERILOG-2001 SOURCE CODE.
** GENERATED BY CLASH 1.6.3. DO NOT MODIFY.
*/
module Fractran
    ( // Inputs
      input [7:0] accumulator
    , input [7:0] fraction
    , input  clk // clock
    , input  rst // reset
    , input  en // enable

      // Outputs
    , output wire [7:0] degree
    , output wire  we
    , output wire  halt
    , output wire [5:0] count
    );
  // src/Fractran.hs:148:1-9
  wire [17:0] \input ;
  // src/Fractran.hs:120:1-88
  reg [16:0] s1 = {1'd0,   {1'b1,{1'b1,8'b00000000}},   6'b000000};
  wire  c$app_arg;
  wire [7:0] c$app_arg_0;
  // src/Fractran.hs:107:1-6
  wire [8:0] v;
  wire [7:0] c$case_alt_0;
  // src/Fractran.hs:37:3-6
  wire [7:0] a;
  wire [16:0] result;
  wire [9:0] result_0;
  wire [9:0] c$case_alt_1;
  wire [9:0] c$case_alt_2;
  wire [9:0] c$case_alt_3;
  wire [9:0] c$case_alt_4;
  // src/Fractran.hs:56:7-8
  wire [7:0] ipv;
  wire [7:0] c$app_arg_1;
  wire [9:0] c$case_alt_5;
  wire  c$case_scrut;
  // src/Fractran.hs:56:7-8
  wire [7:0] f;
  // src/Fractran.hs:81:1-6
  wire [8:0] f_0;
  // src/Fractran.hs:81:1-6
  wire [8:0] a_0;
  // src/Fractran.hs:148:1-9
  reg [8:0] c$input_app_arg;
  // src/Fractran.hs:148:1-9
  reg [8:0] c$input_app_arg_0;
  wire [9:0] c$app_arg_selection_3;
  wire [7:0] c$bv;
  wire [15:0] c$case_alt;

  assign \input  = {c$input_app_arg_0,
                    c$input_app_arg};

  // register begin
  always @(posedge clk or  posedge  rst) begin : s1_register
    if ( rst) begin
      s1 <= {1'd0,   {1'b1,{1'b1,8'b00000000}},   6'b000000};
    end else if (en) begin
      s1 <= result;
    end
  end
  // register end

  assign c$case_alt = {c$app_arg_0,   c$app_arg,
                       1'b0,   s1[5:0]};

  assign c$app_arg = s1[16:16] ? (1'b1) : (1'b0);

  assign c$app_arg_selection_3 = s1[15:6];

  assign c$app_arg_0 = c$app_arg_selection_3[9:9] ? c$case_alt_0 : (8'b00000000);

  assign v = s1[14:6];

  assign c$case_alt_0 = v[8:8] ? (a) : 8'b11111111;

  assign a = v[7:0];

  assign result = s1[16:16] ? {1'd0,   result_0,
                               s1[5:0]} : {1'd1,   result_0,
                                           s1[5:0] + 6'b000001};

  assign result_0 = a_0[8:8] ? c$case_alt_1 : c$case_alt_2;

  assign c$case_alt_1 = f_0[8:8] ? c$case_alt_3 : {1'b1,a_0};

  assign c$case_alt_2 = f_0[8:8] ? c$case_alt_5 : {1'b1,{1'b0,8'bxxxxxxxx}};

  assign c$case_alt_3 = c$case_scrut ? {1'b1,{1'b1,(ipv + f) % 8'b01111110}} : c$case_alt_4;

  assign c$case_alt_4 = (ipv >= c$app_arg_1) ? {1'b1,{1'b1,(ipv - c$app_arg_1) + 8'b00000001}} : {1'b0,9'bxxxxxxxxx};

  assign ipv = a_0[7:0];

  assign c$app_arg_1 = -f;

  assign c$case_alt_5 = c$case_scrut ? {1'b1,{1'b1,f}} : {1'b0,9'bxxxxxxxxx};

  assign c$bv = (f);

  assign c$case_scrut = ( c$bv[8-1] ) == (1'b0);

  assign f = f_0[7:0];

  assign f_0 = \input [8:0];

  assign a_0 = \input [17:9];

  always @(*) begin
    case(fraction)
      8'b11111111 : c$input_app_arg = {1'b0,8'bxxxxxxxx};
      default : c$input_app_arg = {1'b1,fraction};
    endcase
  end

  always @(*) begin
    case(accumulator)
      8'b11111111 : c$input_app_arg_0 = {1'b0,8'bxxxxxxxx};
      default : c$input_app_arg_0 = {1'b1,accumulator};
    endcase
  end

  assign degree = c$case_alt[15:8];

  assign we = c$case_alt[7:7];

  assign halt = c$case_alt[6:6];

  assign count = c$case_alt[5:0];


endmodule

