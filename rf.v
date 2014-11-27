module rf_read(num, rf_in, valid, retriving, locked, tag, val);

parameter NCORES;
    
input  [$clog2(NCORES)-1:0] num;
input  [NCORES*(1+1+1+16+16)-1:0] rf_in;

output valid =         rf_in[num*(1+1+16+16)+1+1+16+16 +: 1];
output retriving =     rf_in[num*(1+1+16+16)+1+16+16   +: 1];
output locked =        rf_in[num*(1+1+16+16)+16+16   +: 1];
output [15:0] tag =    rf_in[num*(1+1+16+16)+16      +: 16];
output [15:0] val =    rf_in[num*(1+1+16+16)         +: 16];

endmodule

module rf_write(rf_in, valids, retrs, lockeds, tags, vals, rf_out);

parameter NCORES;

input  [NCORES*(1+1+1+16+16)-1:0] rf_in;
input  valids [NCORES];
input  retrs [NCORES]; // if a register is being retrived from memory.
input  lockeds [NCORES];
input  [15:0] tags [NCORES];
input  [15:0] vals [NCORES];

output [NCORES*(1+1+1+16+16)-1:0] rf_out;

always @(*) begin
    integer i;

    for (i=0; i < NCORES; i=i+1) begin
        rf_out[i] = {valids[i], retrs[i], lockeds[i], tags[i], vals[i]};
    end
end

endmodule



module rf_write(num, rf_out, valid, locked, tag, val, rf_out);

parameter NCORES;
    
input  [$clog2(NCORES)-1:0] num;
input  [NCORES*(1+1+16+16)-1:0] rf_in;

endmodule

