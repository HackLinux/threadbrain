// The form of a register is:
// [valid] [retrieving from mem] [locked] [16' memory pointer] [16' value]
module rf_read(num, rf_in, valid, retriving, locked, tag, val);

parameter NCORES;
    
input  [$clog2(NCORES)-1:0] num;
input  [NCORES*(1+1+1+16+16)-1:0] rf_in;

output valid =         rf_in[num*(1+1+1+16+16)+1+1+16+16 +: 1];
output retriving =     rf_in[num*(1+1+1+16+16)+1+16+16   +: 1];
output locked =        rf_in[num*(1+1+1+16+16)+16+16     +: 1];
output [15:0] tag =    rf_in[num*(1+1+1+16+16)+16        +: 16];
output [15:0] val =    rf_in[num*(1+1+1+16+16)           +: 16];

endmodule

