module byte_rotation (input logic clk, reset_n, start,
                      input logic [15:0] message_addr, size, output_addr,
							 output logic done, mem_clk, mem_we, 
							 output logic [15:0] mem_addr,
							 output logic [31:0] mem_write_data,
							 input logic [31:0] mem_read_data);
  enum logic [1:0] {IDLE=2'b00, READ=2'b01, WRITE=2'b10, DONE=2'b11} state;
  
  logic [15:0] count;

  function logic [31:0] byte_rotate (input logic [31:0] value);
      byte_rotate = {value[23:16], value[15:8], value[7:0], value[31:24]};
  endfunction
  
  assign mem_clk=clk;
  assign mem_write_data = byte_rotate(mem_read_data);
  
  always_ff @(posedge clk, negedge reset_n)
  begin
    if (!reset_n) begin
	   state <= IDLE;
	   done <= 0;
      mem_we <= 0;
	 end else
		case (state)
		  IDLE:
		    if (start) begin
			   count <= size;
            mem_we <= 0;
			   state <= READ;
			 end
		  READ:
		    if(count>0) begin
			   mem_addr <= message_addr+size-count;
            mem_we <= 0;
				state <= WRITE;
			 end else begin
			   state <= DONE;
				done <= 1;
			 end
		  WRITE: 
		    if (count>0) begin
			   mem_addr <= output_addr+size-count;
            mem_we <= 1;
				count <= count - 1;
				state <= READ;
			 end
		  DONE: 
			  state <= IDLE;
		  endcase
	end
	
endmodule			 
			   