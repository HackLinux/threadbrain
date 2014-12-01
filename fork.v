module fork_em(clk, ins_in, ptr,
               core_ens_in, core_ens_out,
               fork_cxt_in, fork_cxt_out);

parameter NCORES;

parameter FORK = 4'h7;

input  clk;
input  [15:0] ins_in;
input  [15:0] ptr; // Same as wb pointer.
input  [NCORES-1:0] core_ens_in;
input  [NCORES*(1+16+16)-1:0] fork_cxt_in;

output reg [NCORES-1:0] core_ens_out;
output reg [NCORES*(1+16+16)-1:0] fork_cxt_out;

reg [15:0] ins;

reg forked_core; // if we have yet forked to a core.

always @(*) begin
    integer i;
    forked_core = 1'b0;
    for (i=0; i<NCORES; i=i+1) begin
            if (ins[15:12] == FORK &&
                !forked_core && !core_ens_in[i]) begin
               core_ens_out[i] = 1'b1;
               fork_cxt_out[i*(1+16+16) +: 1+16+16] = {1'b1,ptr,{4'b0000,ins[11:0]}};
               forked_core = 1'b1; 
            end else begin
               core_ens_out[i] = core_ens_in[i];
               fork_cxt_out[i*(1+16+16) +: 1+16+16] = fork_cxt_in[i*(1+16+16) +: 1+16+16];
            end
    end
end

always @(posedge clk) begin
    ins <= ins_in; 
end

endmodule

