module wb(clk, rf_in, rf_out,
          val_in, wb_en_in, ptr_in);
// TODO: write back register to memory when done (perhaps in select stage)
// unlock register file and write back here.
// consider letting +++++ work
// Check all initial values

parameter NCORES;

input  [NCORES*(1+1+1+16+16)-1:0] rf_in;
output reg [NCORES*(1+1+1+16+16)-1:0] rf_out;

reg wb_en = 1'b0;
reg [15:0] val;
reg [15:0] ptr;
reg [NCORES*(1+1+1+16+16)-1:0] rf;
reg [NCORES*(1+1+1+16+16)-1:0] nrf; // TODO, use rf_write to get this.

always @(posedge clk) begin
    wb_en <= wb_en_in;
    val <= val_in;
    ptr <= ptr_in;
    rf <= rf_in;
    rf_out <= nrf;
end

endmodule

