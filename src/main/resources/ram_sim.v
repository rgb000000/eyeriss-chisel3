module ram_sim(
	clk,
	addr,
	din,
	//waddr,
	dout,
	we
);

parameter aw = 16;
parameter dw = 16;
parameter path = "/home/l-b/prj/eyeriss-chisel3/src/main/resources/ram.mem";

input clk;
input we;
input [aw-1: 0] addr;
//input [aw-1: 0] waddr;
input [dw-1: 0] din;
output reg [dw-1: 0] dout;

reg [dw-1: 0] mem[(1 << aw) - 1 : 0];

// assign dout = mem[addr];

always@(posedge clk)
begin
	if(we)
		mem[addr] <= din;
    dout <= mem[addr];
end

initial
 begin
 $readmemh(path, mem);
 //$vcdplusmemon;
 end

endmodule
