module alu(clk, ins_in,
           val_in, val_out, wb_en,
           ptr_select, ptr_wb,
           branch_val, branch_en);

parameter PLUS  = 4'h1;
parameter MINUS = 4'h2;
parameter INC   = 4'h3;
parameter DEC   = 4'h4;
parameter BRZ   = 4'h5;

input  clk;
input  [15:0] ins_in;
input  [15:0] val_in;

output [15:0] val_out;
output wb_en;
output [15:0] ptr_select = nptr;
output [15:0] ptr_wb = ptr;
output [15:0] branch_val;
output branch_en;

reg [15:0] val:
reg [15:0] ins;
reg [15:0] ptr;
reg [15:0] nptr;

always @(*) begin
    nptr = ptr;
    val_out = 16'hdead;
    wb_en = 1'b0;
    branch_val = 16'hdead;
    branch_en = 1'b0;

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
            if (val_in == 16'h0000) begin
                    branch_val = {4'b0000, ins[11:0]};
                    branch_en = 1'b1;
            end
        end
    endcase
end

always @(posedge clk) begin
    val <= val_in;
    ins <= ins_in;
    ptr <= nptr; 
end

endmodule

