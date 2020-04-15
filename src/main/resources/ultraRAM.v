module URAM(
	clk,
	raddr,
	din,
	waddr,
	dout,
	we
);

parameter aw = 16;
parameter dw = 16;

input clk;
input we;
input [aw-1: 0] raddr;
input [aw-1: 0] waddr;
input [dw-1: 0] din;
output reg [dw-1: 0] dout;

(* ram_style="ultra" *)
reg [dw-1: 0] mem[(1 << aw) - 1 : 0];

always@(posedge clk)
begin
	if(we)
		mem[waddr] <= din;
    dout <= mem[raddr];
end

endmodule
