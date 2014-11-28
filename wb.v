module wb(clk, rf_in, rf_out,
          val_in, wb_en_in, ptr_in);
// TODO: write back register to memory when done (perhaps in select stage)
// consider letting +++++ work
// Check all initial values

parameter NCORES;

input  [NCORES*(1+1+1+16+16)-1:0] rf_in;
output [NCORES*(1+1+1+16+16)-1:0] rf_out;

reg wb_en = 1'b0;
reg [15:0] val;
reg [15:0] ptr;
reg [NCORES*(1+1+1+16+16)-1:0] rf;

wire [15:0] tags [NCORES];

wire nvalids [NCORES];
wire nretrs [NCORES]; // if a register is being retrived from memory.
wire nlockeds [NCORES];
wire [15:0] ntags [NCORES];
wire [15:0] nvals [NCORES];

genvar i;
generate
    for (i=0; i < NCORES; i=i+1) begin : RF_LOOKUP
        #(NCORES) rf_read(.num(i), .rf_in(rf), .tag(tags[i]));
    end
endgenerate

#(NCORES) rf_write(rf, nvalids, nretrs, nlockeds, ntags, nvals, rf_out);

always @(*) begin
    integer i;
    
    for (i=0; i<NCORES; i=i+1) begin
        nvalids[i] = valids[i];
        nretrs[i] = retrs[i];
        nlockeds[i] = lockeds[i];
        ntags[i] = tags[i];
        nvals[i] = vals[i];

        if (wb_en && tags[i] == ptr) begin
            nlockeds[i] = 1'b0;
            nvals[i] = val;
        end
    end
end

always @(posedge clk) begin
    wb_en <= wb_en_in;
    val <= val_in;
    ptr <= ptr_in;
    rf <= rf_in;
end

endmodule

