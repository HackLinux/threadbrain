module select(ins, ptr, clk, stall, out_ins,
              branch_en,
              val, 
              mem_en_in, mem_en_out,
              mem_data_in, mem_addr_out, // different for every select
              in_regs, out_regs);

parameter NCORES;

parameter PLUS  = 4'h1;
parameter MINUS = 4'h2;
parameter BRZ   = 4'h5;

input  [15:0] ins;
input  [15:0] ptr;
input  clk;
input  [NCORES*(1+1+1+16+16)-1:0] in_regs; 
input  mem_en_in;
input  [15:0] mem_data_in;
input  branch_en,

output [15:0] out_ins = ins;
output [15:0] mem_addr_out = ptr;
output reg mem_en_out;
output reg stall;
output reg [15:0] val;
output [NCORES*(1+1+1+16+16)-1:0] out_regs; 

wire valids [NCORES];
wire retrs [NCORES]; // if a register is being retrived from memory.
wire lockeds [NCORES];
wire [15:0] tags [NCORES];
wire [15:0] vals [NCORES];

wire nvalids [NCORES];
wire nretrs [NCORES]; // if a register is being retrived from memory.
wire nlockeds [NCORES];
wire [15:0] ntags [NCORES];
wire [15:0] nvals [NCORES];


genvar i;
generate
    for (i=0; i < NCORES; i=i+1) begin : RF_LOOKUP
        #(NCORES) rf_read(i, rf_in, valids[i], retrs[i], lockeds[i], tags[i], vals[i]);
    end
endgenerate

rf_write(rf_in, nvalids, nretrs, nlockeds, ntags, nvals, rf_out);

reg [$clog2(NCORES)-1:0] free_reg;
// Find first free register.
always @(*) begin
    integer i;
    free_reg = 0;

    for (i=0; i<NCORES; i=i+1) begin
        if (!valids[i] && !retrs[i]) begin
            free_reg = i;        
        end
    end
end

wire need_reg = ins[15:12] == PLUS || ins[15:12] == MINUS || ins[15:12] == BRZ;
wire lock_reg = ins[15:12] == PLUS || ins[15:12] == MINUS;
reg found_reg;
reg branch_en1;
reg mem_en;
reg mem_en1;
reg mem_en2;
reg [$clog2(NCORES)-1:0] mem_dest;
reg [$clog2(NCORES)-1:0] mem_dest1;
reg [$clog2(NCORES)-1:0] mem_dest2;

wire branching = branch_en | branch_en1;

always @(*) begin
    integer i;

    found_reg = 1'b0;
    stall = 1'b0;
    val = 16'hdead;
    mem_en_out = mem_en_in;
    mem_en = 1'b0;
    mem_dest = free_reg;
    
    for (i=0; i<NCORES; i=i+1) begin
        nvalids[i] = valids[i];
        nretrs[i] = retrs[i];
        nlockeds[i] = lockeds[i];
        ntags[i] = tags[i];
        nvals[i] = vals[i];
            
        if (!branching) begin
            // The register is available -- maybe lock it.
            if (need_reg && 
                valids[i] && 
                !lockeds[i] && 
                tags[i] == ptr) begin
            
                val = vals[i];
                found_reg = 1'b1;
                nlockeds[i] = lock_reg;
            // The register is being retrieved -- stall.
            end else if (need_reg && tags[i] == ptr && retrs[i]) begin
                stall = 1'b1;
                found_reg = 1'b1;
            end
        end 
    end

    // Jut retrieved the register from memory.
    if (mem_en2) begin
        val = mem_data_in;
        found_reg = 1'b1;
        nvalids[mem_dest2] = 1'b1;
        nretrs[mem_dest2] = 1'b0;
        nlockeds[i] = lock_reg;
        nvals[i] = mem_data_in;  
        stall = 1'b0;
    end

    if (!branching && !found_reg && need_reg) begin
            stall = 1'b1;
            if (!mem_en_in) begin
                mem_en_out = 1'b1;
                mem_en = 1'b1;
                mem_dest = free_reg;
                nretrs[free_reg] = 1'b1;
                ntags[free_reg] = ptr;
            end
    end
end

always @(posedge clk) begin
    mem_en1 <= mem_en;
    mem_en2 <= mem_en1;
    mem_dest1 <= mem_dest;
    mem_dest2 <= mem_dest1;
    branch_en1 <= branch_en;
end

endmodule

