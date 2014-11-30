module fork(ins_in, 
            core_ens_in, core_ens_out,
            core_starts_in, core_starts_out);

parameter NCORES;

parameter FORK = 4'h7;

input  [15:0] ins_in;
input  [NCORES-1:0] core_ens_in;
input  [NCORES*(16)-1:0] core_starts_in; // where each core should start

output reg [NCORES-1:0] core_ens_out;
output reg [NCORES*(16)-1:0] core_starts_out;

wire [15:0] ins;

reg forked_core; // if we have yet forked to a core.

always @(*) begin
    integer i;
    forked_core = 1'b0;
    for (i=0; i<NCORES; i=i+1) begin
            if (ins[15:12] == FORK &&
                !forked_core && !core_ens_in[i]) begin
               core_ens_out[i] = 1'b1;
               core_starts_out[i] = {4'h0,ins[11:0]};
               forked_core = 1'b1; 
            end else begin
               core_ens_out[i] = core_ens_in[i];
               core_starts_out[i] = core_starts_in[i];
            end
    end
end

always @(posedge clk) begin
    ins <= ins_in; 
end

endmodule

