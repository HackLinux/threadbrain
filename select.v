module select(ins, ptr, clk, stall, out_ins,
              branch_en,
              val, 
              ld_en_in, ld_en_out,
              ld_data_in, 
              ld_addr_in, ld_addr_out,
              st_en_in, st_en_out,
              st_data_in, st_data_out, 
              st_addr_in, st_addr_out,
              core_en_in, core_en_out,
              rf_in, rf_out);

parameter NCORES;

// Should lock.
parameter PLUS  = 4'h1;
parameter MINUS = 4'h2;

// Don't lock.
parameter BRZ   = 4'h5;
parameter PRINT = 4'h9;

// Disable coreeeee
parameter END = 4'hA;

input  [15:0] ins;
input  [15:0] ptr;
input  clk;
input  [NCORES*(1+1+1+16+16)-1:0] rf_in; 
input  ld_en_in;
input  [15:0] ld_data_in;
input  [15:0] ld_addr_in;
input  [15:0] st_data_in;
input  [15:0] st_addr_in;
input  st_en_in;
input  branch_en;
input  core_en_in;

output [15:0] out_ins = stall || branching ? 16'h0000 : ins;
output reg [15:0] ld_addr_out;
output reg ld_en_out;
output reg st_en_out;
output reg [15:0] st_addr_out;
output reg [15:0] st_data_out;
output reg stall;
output reg [15:0] val;
output reg [NCORES*(1+1+1+16+16)-1:0] rf_out; 
output core_en_out = !branching && !should_st && ins[15:12] == END ? 1'b0 : core_en_in;


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

reg [$clog2(NCORES)-1:0] free_reg;
reg found_free_reg;
// Find first free register.
always @(*) begin
    integer i;
    free_reg = 0;
    found_free_reg = 1'b0;

    for (i=0; i<NCORES; i=i+1) begin
        if (!found_free_reg && !valids[i] && !retrs[i]) begin
            free_reg = i;        
            found_free_reg = 1'b1;
        end
    end
end

wire need_reg = ins[15:12] == PLUS  || 
                ins[15:12] == MINUS || 
                ins[15:12] == BRZ   ||
                ins[15:12] == PRINT;
wire lock_reg = ins[15:12] == PLUS || ins[15:12] == MINUS;

reg found_reg;
reg branch_en1 = 1'b0;
reg ld_en;
reg ld_en1 = 1'b0;
reg ld_en2 = 1'b0;
reg [$clog2(NCORES)-1:0] ld_dest;
reg [$clog2(NCORES)-1:0] ld_dest1;
reg [$clog2(NCORES)-1:0] ld_dest2;

wire branching = branch_en | branch_en1;

// Keep track so that we can store it when finished.
reg  [1+16-1:0] last_ptr_used = 0;
reg  [1+16-1:0] nlast_ptr_used;
wire should_st = !branching && 
                 ((need_reg && 
                   last_ptr_used[16] && 
                   last_ptr_used[15:0] != ptr[15:0]) ||
                  (last_ptr_used[16] && ins[15:12] == END));

always @(*) begin
    integer i;

    found_reg = 1'b0;
    stall = 1'b0;
    val = 16'hdead;
    ld_en_out = ld_en_in;
    ld_addr_out = ld_addr_in;
    ld_en = 1'b0;
    ld_dest = free_reg;
    st_en_out = st_en_in;
    st_data_out = st_data_in;
    st_addr_out = st_addr_in;

    if (need_reg) begin
        nlast_ptr_used = {1'b1, ptr};
    end else if (ins[15:12] == END) begin
        nlast_ptr_used = 16'h0000;
    end else begin
        nlast_ptr_used = last_ptr_used;
    end

    for (i=0; i<NCORES; i=i+1) begin
        nvalids[i] = valids[i];
        nretrs[i] = retrs[i];
        nlockeds[i] = lockeds[i];
        ntags[i] = tags[i];
        nvals[i] = vals[i];
            
        // Determine if we have the register or should stall/load it.
        if (!should_st && !branching) begin
            // The register is available -- maybe lock it.
            if (need_reg && 
                valids[i] && 
                !lockeds[i] && 
                !retrs[i] &&
                tags[i] == ptr) begin
            
                val = vals[i];
                found_reg = 1'b1;
                nlockeds[i] = lock_reg;
            // The register is being retrieved or is locked -- stall.
            end else if (need_reg && 
                         tags[i] == ptr && 
                         ((valids[i] && lockeds[i]) || retrs[i])) begin
                stall = 1'b1;
                found_reg = 1'b1;
            end
        end 


        // Determine if we should store the old register we were using.
        if (should_st) begin
            stall = 1'b1;
            if (valids[i] && tags[i] == last_ptr_used[15:0]) begin
                if (!lockeds[i]) begin
                    // Store back when we can.
                    if (!st_en_in) begin
                        st_en_out = 1'b1;
                        st_data_out = vals[i];
                        st_addr_out = last_ptr_used[15:0];
                        nvalids[i] = 1'b0;
                        nlockeds[i] = 1'b0;
                        nretrs[i] = 1'b0;
                        if (ins[15:0] == END) begin
                            nlast_ptr_used = 16'h0000;
                        end
                    end else begin
                        nlast_ptr_used = last_ptr_used;
                    end
                end
                // If locked, another core is using it, and will store.
            end
        end
    end

    // Just retrieved the register from memory.
    if (ld_en2) begin
        val = ld_data_in;
        found_reg = 1'b1;
        nvalids[ld_dest2] = 1'b1;
        nretrs[ld_dest2] = 1'b0;
        nlockeds[ld_dest2] = lock_reg;
        nvals[ld_dest2] = ld_data_in;  
        stall = 1'b0;
    end

    // If we need to load a register from memory.
    if (!branching && !found_reg && need_reg && !stall && !should_st) begin
            stall = 1'b1;
            if (!ld_en_in && !st_en_in) begin
                ld_en_out = 1'b1;
                ld_addr_out = ptr;
                ld_en = 1'b1;
                ld_dest = free_reg;
                nretrs[free_reg] = 1'b1;
                ntags[free_reg] = ptr;
            end
    end
end

always @(posedge clk) begin
    ld_en1 <= ld_en;
    ld_en2 <= ld_en1;
    ld_dest1 <= ld_dest;
    ld_dest2 <= ld_dest1;
    branch_en1 <= branch_en;

    last_ptr_used <= nlast_ptr_used;
end

endmodule

