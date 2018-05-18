// Simplified SHA-256 Module
// By Siddharth Atre & Kejia Ren

module simplified_sha256 (input logic clk, reset_n, start, 
									input logic[15:0] message_addr, output_addr,
									output logic done, mem_clk, mem_we,
									output logic [15:0] mem_addr,
									output logic [31:0] mem_write_data,
									input logic [31:0] mem_read_data);
									
  enum logic[2:0] {IDLE=3'b000, PREP1=3'b001, PREP2=3'b010, COMPUTE=3'b011, POST=3'b100, WRITE=3'b101, DONE=3'b110} state;
	
  logic [15:0] wc; //write counters
  logic block;
  logic [15:0] t;
  logic [31:0] h[8];
  logic [31:0] w[16];
  logic [31:0] a, b, c, d, e, f, g, H;
  	
  parameter int k[0:63] = '{
  32'h428a2f98,32'h71374491,32'hb5c0fbcf,32'he9b5dba5,32'h3956c25b,32'h59f111f1,32'h923f82a4,32'hab1c5ed5,
  32'hd807aa98,32'h12835b01,32'h243185be,32'h550c7dc3,32'h72be5d74,32'h80deb1fe,32'h9bdc06a7,32'hc19bf174,
  32'he49b69c1,32'hefbe4786,32'h0fc19dc6,32'h240ca1cc,32'h2de92c6f,32'h4a7484aa,32'h5cb0a9dc,32'h76f988da,
  32'h983e5152,32'ha831c66d,32'hb00327c8,32'hbf597fc7,32'hc6e00bf3,32'hd5a79147,32'h06ca6351,32'h14292967,
  32'h27b70a85,32'h2e1b2138,32'h4d2c6dfc,32'h53380d13,32'h650a7354,32'h766a0abb,32'h81c2c92e,32'h92722c85,
  32'ha2bfe8a1,32'ha81a664b,32'hc24b8b70,32'hc76c51a3,32'hd192e819,32'hd6990624,32'hf40e3585,32'h106aa070, 
  32'h19a4c116,32'h1e376c08,32'h2748774c,32'h34b0bcb5,32'h391c0cb3,32'h4ed8aa4a,32'h5b9cca4f,32'h682e6ff3,
  32'h748f82ee,32'h78a5636f,32'h84c87814,32'h8cc70208,32'h90befffa,32'ha4506ceb,32'hbef9a3f7,32'hc67178f2
  };
  
  function logic [31:0] rightrotate(input logic [31:0] x,
                                  input logic [7:0] r);
  begin
    rightrotate = (x >> r) | (x << (32-r));
  end
  endfunction
  
	
  function logic [255:0] sha256_op(input logic [31:0] a, b, c, d, e, f, g, h, w,
                                 input logic [7:0] t);
  logic [31:0] S1, S0, ch, maj, t1, t2; // internal signals
  begin
    S1 = rightrotate(e, 6) ^ rightrotate(e, 11) ^ rightrotate(e, 25);
    ch = (e & f) ^ ((~e) & g);
    t1 = h + S1 + ch + k[t] + w;
    S0 = rightrotate(a, 2) ^ rightrotate(a, 13) ^ rightrotate(a, 22);
    maj = (a & b) ^ (a & c) ^ (b & c);
    t2 = S0 + maj;

    sha256_op = {t1 + t2, a, b, c, d + t1, e, f, g};
  end
  endfunction
  
  function logic [31:0] wtnew;//function with no inputs
    logic [31:0] s0, s1;
	 
	 s0 = rightrotate(w[1], 7) ^ rightrotate(w[1], 18) ^ (w[1]>>3);
	 s1 = rightrotate(w[14], 17) ^ rightrotate(w[14], 19) ^ (w[14]>>10);
	 wtnew = w[0] + s0 + w[9] + s1;
  endfunction

									
  
  assign mem_clk=clk;	
	
									
  always_ff @(posedge clk, negedge reset_n)
  begin
    if(!reset_n) begin
	   state <= IDLE;
	   done <= 0;
		block <= 0;
	 end else		
	 case(state)
		IDLE:
		    if(start) begin
		      mem_we <= 0;
		      mem_addr <= message_addr;
				t <= 0;
				a <= 32'h6a09e667;
            b <= 32'hbb67ae85;
            c <= 32'h3c6ef372;
            d <= 32'ha54ff53a;
            e <= 32'h510e527f;
            f <= 32'h9b05688c;
            g <= 32'h1f83d9ab;
            H <= 32'h5be0cd19; 
				
				h[0] <= 32'h6a09e667;
            h[1] <= 32'hbb67ae85;
            h[2] <= 32'h3c6ef372;
            h[3] <= 32'ha54ff53a;
            h[4] <= 32'h510e527f;
            h[5] <= 32'h9b05688c;
            h[6] <= 32'h1f83d9ab;
            h[7] <= 32'h5be0cd19; 
			
			   state <= PREP1;
				block <= 0;
		    end
			 
	   PREP1: begin
		    state <= PREP2;
			 mem_addr <= mem_addr + 1;
			 {a, b, c, d, e, f, g, H} <= {h[0], h[1], h[2], h[3], h[4], h[5], h[6], h[7]};
		end
			
	   PREP2: begin
			 w[15] <= mem_read_data;
			 for (int n = 0; n < 15; n++) w[n] <= w[n+1];
		    state <= COMPUTE;
			 mem_addr <= mem_addr + 1;
	   end
			
		COMPUTE: begin
		    if (block == 0) begin 
			   if (t<64) begin 
				  for (int n = 0; n < 15; n++) w[n] <= w[n+1];
		        if (t<15) begin
				    w[15] <= mem_read_data;
					 mem_addr <= mem_addr + 1;
		        end else begin
			       w[15] <= wtnew();
					 mem_addr <= 16;
		        end
				  t <= t + 1;
				  {a, b, c, d, e, f, g, H} <= sha256_op(a, b, c, d, e, f, g, H, w[15], t);
				  state <= COMPUTE;
				end else begin 
				  {h[0], h[1], h[2], h[3], h[4], h[5], h[6], h[7]} <= {h[0] + a, h[1] + b, h[2] + c, h[3] + d, h[4] + e, h[5] + f, h[6] + g, h[7] + H};
				  for (int n = 0; n < 16; n++) w[n] <= 32'h00000000;
				  t <= 0;
				  block <= 1;
				  mem_addr <= message_addr + 16;
				  state <= PREP1;
				end
			 end else begin 
			   if (t<64) begin
				  for (int n = 0; n < 15; n++) w[n] <= w[n+1];
				  if (t<15) begin
		          if (t<3) begin
                  w[15] <= mem_read_data;
					   mem_addr <= mem_addr + 1;
		          end 
				    else if (t == 3 ) begin
				      w[15] <= 32'h80000000;
					 end 
					 else if (t>3 && t<14) begin
					   w[15] <= 32'h00000000;
					 end else begin
					   w[15] <= 32'h00000280;
					 end
				  end else begin
			       w[15] <= wtnew();
		        end
				  t <= t + 1;
				  {a, b, c, d, e, f, g, H} <= sha256_op(a, b, c, d, e, f, g, H, w[15], t);
				  state <= COMPUTE;
				end else begin
				  state <= POST;
				  {h[0], h[1], h[2], h[3], h[4], h[5], h[6], h[7]} <= {h[0] + a, h[1] + b, h[2] + c, h[3] + d, h[4] + e, h[5] + f, h[6] + g, h[7] + H};
				end
			end
		end
			
		POST: begin
		      
			 mem_we <= 1;
			 mem_addr <= output_addr;
			 mem_write_data <= h[0];
          state <= WRITE;
          wc <= 1;			 
		end
			
		WRITE: begin
		    if (wc<8) begin
			   mem_addr <= output_addr + wc;
				mem_write_data <= h[wc];
				state <= WRITE;
				wc <= wc + 1;
   		 end else begin
			   state <= DONE;
			 end
		end
			
		DONE: begin
		    state <= IDLE;
			 done <= 1; 
		end
    endcase	 
  end									
endmodule

