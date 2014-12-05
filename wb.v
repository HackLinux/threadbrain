// Writes back to registers. If a register's value was changed, this unlocks
// the register for future use and writes the new value to it. Not in charge of
// storing the register back in memory.
module wb(clk, rf_in, rf_out,
          val_in, wb_en_in, ptr_in,
          core_stall);
parameter NCORES;

input  clk;
input  [NCORES*(1+1+1+16+16)-1:0] rf_in;
input  [15:0] val_in;
input  wb_en_in;
input  [15:0] ptr_in;
input  core_stall;

output reg [NCORES*(1+1+1+16+16)-1:0] rf_out;

reg wb_en = 1'b0;
reg [15:0] val;
reg [15:0] ptr;
reg [NCORES*(1+1+1+16+16)-1:0] rf;

wire valids [NCORES];
wire retrs [NCORES]; // if a register is being retrived from memory.
wire lockeds [NCORES];
wire [15:0] tags [NCORES];
wire [15:0] vals [NCORES];

reg nvalids [NCORES];
reg nretrs [NCORES]; // if a register is being retrived from memory.
reg nlockeds [NCORES];
reg [15:0] ntags [NCORES];
reg [15:0] nvals [NCORES];

genvar i;
generate
    for (i=0; i < NCORES; i=i+1) begin : RF_LOOKUP
        rf_read #(NCORES) (i, rf_in, valids[i], retrs[i], lockeds[i], tags[i], vals[i]);
    end
endgenerate

always @(*) begin
	integer i;
	for (i=0; i<NCORES; i=i+1) begin
		rf_out[i*(1+1+1+16+16) +: 1+1+1+16+16] = {nvalids[i],
                                                  nretrs[i],
                                                  nlockeds[i],
                                                  ntags[i],
                                                  nvals[i]};
	end
end

always @(*) begin
    integer i;
    
    for (i=0; i<NCORES; i=i+1) begin
        nvalids[i] = valids[i];
        nretrs[i] = retrs[i];
        nlockeds[i] = lockeds[i];
        ntags[i] = tags[i];
        nvals[i] = vals[i];

        if (wb_en && tags[i] == ptr && valids[i]) begin
            nlockeds[i] = 1'b0;
            nvals[i] = val;
        end
    end
end

always @(posedge clk) begin
    wb_en <= core_stall ? 1'b0 : wb_en_in;
    val <= val_in;
    ptr <= ptr_in;
end

endmodule

