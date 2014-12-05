// This module fetches the next instruction from memory. It can stall and
// branch to a new instruction as well. It fetches the same instruction over
// and over again if the core is disabled, and sends noops to the rest of the
// pipeline.
module fetch(clk, 
             core_en,
             branch_en, branch_val, stall,
             fetch_addr, fetch_data, ins,
             fork_cxt); 
input  clk;
input  core_en;
input  branch_en;
input  stall;
input  [15:0] branch_val;
input  [15:0] fetch_data;
input  [(1+16+16)-1:0] fork_cxt;

output [15:0] fetch_addr = next_pc;
output reg [15:0] ins;

reg [15:0] next_pc;
reg [15:0] pc = 16'hffff;

reg core_en1 = 1'b0;
reg core_en2 = 1'b0;

// Handle last-fetched instruction.
always @(*) begin
    if (core_en2) begin
        ins = fetch_data;
    end else begin
        ins = 16'h0000;
    end
end

// Calculate next pc.
always @(*) begin
    if (stall) begin
        next_pc = pc;
    end else if (branch_en) begin
        next_pc = branch_val; 
    end else if (fork_cxt[(1+16+16)-1]) begin
        next_pc = fork_cxt[0 +: 16];
    end else if (!core_en) begin
        next_pc = pc + 16'h0000;
    end else begin
        next_pc = pc + 16'h0001;
    end
end

always @(posedge clk) begin
    pc <= next_pc;

    core_en1 <= core_en;
    core_en2 <= core_en1;

    if (core_en == 1'b0) begin
        core_en1 <= 1'b0;
        core_en2 <= 1'b0;
    end
end

endmodule

