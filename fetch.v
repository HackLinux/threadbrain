module fetch(clk, 
             core_en,
             branch_en, branch_val, stall,
             fetch_addr, fetch_data, ins); 
input  clk;
input  core_en;
input  branch_en;
input  stall;
input  [15:0] branch_val;
input  [15:0] mem_fetch_in;

output [15:0] fetch_addr = next_pc;
output [15:0] ins;

reg [15:0] next_pc;
reg [15:0] pc = 16'h0000;

reg core_en1 = 1'b0;
reg core_en2 = 1'b0;

// Handle last-fetched instruction.
always @(*) begin
    if (core_en2 && core_en) begin
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

