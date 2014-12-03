module alu(clk, ins_in,
           val_in, val_out, wb_en,
           ptr_select, ptr_wb,
           branch_val, branch_en,
           fork_cxt,
           all_ins,
           current_ins,
           stall,
           num_syncs,
           print, print_valid, next_print_valid,
           core_stall);

parameter NCORES;

parameter PLUS  = 4'h1;
parameter MINUS = 4'h2;
parameter INC   = 4'h3;
parameter DEC   = 4'h4;
parameter BRZ   = 4'h5;
parameter BR    = 4'h6; // unconditional branch

parameter SYNC  = 4'h8;
parameter PRINT = 4'h9;

input  clk;
input  [15:0] ins_in;
input  [15:0] val_in;
input  [(1+16+16)-1:0] fork_cxt;
input  [NCORES*16-1:0] all_ins;
input  core_stall;

output reg [15:0] val_out;
output reg wb_en;
output [15:0] ptr_select = nptr;
output [15:0] ptr_wb = ptr;
output reg [15:0] branch_val;
output reg branch_en;
output reg [15:0] print;
output print_valid = ins[15:12] == PRINT;
output next_print_valid = ins_in[15:12] == PRINT;
output reg stall;
output [15:0] current_ins = ins;
output reg [3:0] num_syncs;

reg [15:0] val;
reg [15:0] ins = 16'h0000;
reg [15:0] ptr = 16'd128;
reg [15:0] nptr;

always @(*) begin
    integer i;

    nptr = ptr;
    val_out = 16'hdead;
    wb_en = 1'b0;
    branch_val = 16'hdead;
    branch_en = 1'b0;
    print = 16'hdead;
    stall = 1'b0;
    num_syncs = 4'b0000;

    case (ins[15:12])
        PLUS: begin
            val_out = val + 16'h0001;  
            wb_en = 1'b1;
        end 
        MINUS: begin
            val_out = val - 16'h0001;
            wb_en = 1'b1;
        end
        INC: begin
            nptr = ptr + 16'h0001;
        end
        DEC: begin
            nptr = ptr - 16'h0001;
        end
        BRZ: begin
            if (val == 16'h0000) begin
                    branch_val = {4'b0000, ins[11:0]};
                    branch_en = 1'b1;
            end
        end
        BR: begin
            branch_val = {4'b0000, ins[11:0]};
            branch_en = 1'b1;
        end
        PRINT: begin
            print = val;
        end
        SYNC: begin
            for (i=0; i<NCORES; i=i+1) begin
                 if (all_ins[i*(16)+12 +: 4] == SYNC &&
                     all_ins[i*(16)    +: 8] == ins[0 +: 8]) begin
                    num_syncs = num_syncs + 4'b0001;
                 end
            end    
            if (num_syncs == ins[8 +: 4]) begin
                stall = 1'b0;
            end else begin
                stall = 1'b1; 
            end
        end
    endcase

    // Runs only if the current core is being started.
    if (fork_cxt[(1+16+16)-1]) begin
        nptr = fork_cxt[16 +: 16];
    end
end

always @(posedge clk) begin
    if (!stall && !core_stall) begin
        val <= val_in;
        ins <= stall ? ins : ins_in;
        ptr <= nptr; 
    end
end

endmodule

