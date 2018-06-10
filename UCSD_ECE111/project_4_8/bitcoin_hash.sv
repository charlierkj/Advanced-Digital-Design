// Bitcoin Hash Module
// By Kejia Ren

module bitcoin_hash (input logic clk, reset_n, start, 
									input logic[15:0] message_addr, output_addr,
									output logic done, mem_clk, mem_we,
									output logic [15:0] mem_addr,
									output logic [31:0] mem_write_data,
									input logic [31:0] mem_read_data);
									
  enum logic[3:0] {IDLE=4'b0000, PREP11=4'b0001, PREP12=4'b0010, PREP13=4'b0011, COMPUTE1=4'b0100, PREP21=4'b0101, PREP22=4'b0110, PREP23=4'b0111, COMPUTE2=4'b1000, PREP31=4'b1001, PREP32=4'b1010, COMPUTE3=4'b1011, WRITE=4'b1100, POST=4'b1101} state;
	
  logic [10:0] t;
  logic [31:0] h[8];
  logic [31:0] h1[8];
  logic [31:0] h2[8];
  logic [31:0] w[16];
  logic [31:0] A, B, C, D, E, F, G, H;
  logic [31:0] temp;
  logic [3:0] index_nonces;
  	
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
  
	
  function logic [255:0] sha256_op(input logic [31:0] A, B, C, D, E, F, G, temp);
  logic [31:0] S1, S0, ch, maj, t1, t2; // internal signals
  begin
    S1 = rightrotate(E, 6) ^ rightrotate(E, 11) ^ rightrotate(E, 25);
    ch = (E & F) ^ ((~E) & G);
    t1 = S1 + ch + temp;
    S0 = rightrotate(A, 2) ^ rightrotate(A, 13) ^ rightrotate(A, 22);
    maj = (A & B) ^ (A & C) ^ (B & C);
    t2 = S0 + maj;

    sha256_op = {t1 + t2, A, B, C, D + t1, E, F, G};
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
	 end else		
	 case(state)
		IDLE:
		    if(start) begin
		      mem_we <= 0;
		      mem_addr <= message_addr;
				index_nonces <= 0;
				
				h[0] <= 32'h6a09e667;
            h[1] <= 32'hbb67ae85;
            h[2] <= 32'h3c6ef372;
            h[3] <= 32'ha54ff53a;
            h[4] <= 32'h510e527f;
            h[5] <= 32'h9b05688c;
            h[6] <= 32'h1f83d9ab;
            h[7] <= 32'h5be0cd19; 
			
			   state <= PREP11;
		    end
			 
	   PREP11: begin
		    state <= PREP12;				
			 mem_addr <= mem_addr + 1;	
		end
			
	   PREP12: begin
		    t <= 0;
		    state <= PREP13;
			 w[15] <= mem_read_data;
			 mem_addr <= mem_addr + 1;
			 {A, B, C, D, E, F, G, H} <= {h[0], h[1], h[2], h[3], h[4], h[5], h[6], h[7]};
	   end
		
		PREP13: begin
		    temp <= w[15] + k[t] + H;
			 for (int n = 0; n < 15; n++) w[n] <= w[n+1];
			 w[15] <= mem_read_data;
			 mem_addr <= mem_addr + 1;
			 state <= COMPUTE1;
			 t <= t + 1;
		end	 
						
		COMPUTE1: begin
			   if (t<65) begin 
				  for (int n = 0; n < 15; n++) w[n] <= w[n+1];
		        if (t<15) begin
				    w[15] <= mem_read_data;
					 mem_addr <= mem_addr + 1;
		        end else begin
			       w[15] <= wtnew();
					 mem_addr <= message_addr + 16;
		        end
				  t <= t + 1;
				  temp <= w[15] + k[t] + G;
				  {A, B, C, D, E, F, G, H} <= sha256_op(A, B, C, D, E, F, G, temp);
				  state <= COMPUTE1;
				end else begin 
				  {h1[0], h1[1], h1[2], h1[3], h1[4], h1[5], h1[6], h1[7]} <= {h[0] + A, h[1] + B, h[2] + C, h[3] + D, h[4] + E, h[5] + F, h[6] + G, h[7] + H};
				  state <= PREP21;
				end
      end
		
		PREP21: begin
		    {h[0], h[1], h[2], h[3], h[4], h[5], h[6], h[7]} <= {h1[0], h1[1], h1[2], h1[3], h1[4], h1[5], h1[6], h1[7]};
			 state <= PREP22;
			 mem_addr <= mem_addr + 1;
		end
		
		PREP22: begin
		    w[15] <= mem_read_data;
			 t <= 0;
		    state <= PREP23;
			 mem_addr <= mem_addr + 1;
			 {A, B, C, D, E, F, G, H} <= {h[0], h[1], h[2], h[3], h[4], h[5], h[6], h[7]};
		end
		
		PREP23: begin
		    temp <= w[15] + k[t] + H;
			 for (int n = 0; n < 15; n++) w[n] <= w[n+1];
			 w[15] <= mem_read_data;
			 mem_addr <= mem_addr + 1;
			 state <= COMPUTE2;
			 t <= t + 1;
		end
		
		COMPUTE2: begin
			   if (t<65) begin
				  for (int n = 0; n < 15; n++) w[n] <= w[n+1];
				  if (t<15) begin
		          if (t<2) begin
                  w[15] <= mem_read_data;
					   mem_addr <= mem_addr + 1;
		          end 
					 else if (t == 2 ) begin
					   w[15] <= index_nonces;
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
				  temp <= w[15] + k[t] + G;
				  t <= t + 1;
				  {A, B, C, D, E, F, G, H} <= sha256_op(A, B, C, D, E, F, G, temp);
				  state <= COMPUTE2;
				end else begin
				  t <= 0;
				  {h2[0], h2[1], h2[2], h2[3], h2[4], h2[5], h2[6], h2[7]} <= {h[0] + A, h[1] + B, h[2] + C, h[3] + D, h[4] + E, h[5] + F, h[6] + G, h[7] + H};
			     h[0] <= 32'h6a09e667;
              h[1] <= 32'hbb67ae85;
              h[2] <= 32'h3c6ef372;
              h[3] <= 32'ha54ff53a;
              h[4] <= 32'h510e527f;
              h[5] <= 32'h9b05688c;
              h[6] <= 32'h1f83d9ab;
              h[7] <= 32'h5be0cd19; 
              state <= 	PREP31;
			  end
		end
		
		
	   PREP31: begin
		    state <= PREP32;
			 w[15] <= h2[t];
			 {A, B, C, D, E, F, G, H} <= {h[0], h[1], h[2], h[3], h[4], h[5], h[6], h[7]};
		end
		
		PREP32: begin
		    temp <= w[15] + k[t] + H;
			 for (int n = 0; n < 15; n++) w[n] <= w[n+1];
			 state <= COMPUTE3;
			 w[15] <= h2[t+1];
			 t <= t + 1;
		end
		
		COMPUTE3: begin
		    if (t<65) begin
				  for (int n = 0; n < 15; n++) w[n] <= w[n+1];
				  if (t<15) begin
		          if (t<7) begin
                  w[15] <= h2[t+1];
		          end 
				    else if (t == 7 ) begin
				      w[15] <= 32'h80000000;
					 end 
					 else if (t>7 && t<14) begin
					   w[15] <= 32'h00000000;
					 end else begin
					   w[15] <= 32'h00000100;
					 end
				  end else begin
			       w[15] <= wtnew();
		        end
				  temp <= w[15] + k[t] + G;
				  t <= t + 1;
				  {A, B, C, D, E, F, G, H} <= sha256_op(A, B, C, D, E, F, G, temp);
				  state <= COMPUTE3;
			  end else begin
				  mem_we <= 1;
			     mem_addr <= output_addr + index_nonces;
				  state <= WRITE;
				  mem_write_data <= h[0] + A;
			  end
		end		
		
	
		WRITE: begin
          if (index_nonces < 15) begin
				state <= POST;
				index_nonces <= index_nonces + 1;
   		 end else begin
			   state <= IDLE;
				done <= 1;
			 end
			 mem_we <= 0;
			 mem_addr <= message_addr + 16;
		end
		
		POST: begin
		    state <= PREP22;
			 mem_addr <= mem_addr + 1;
			 for (int n = 0; n < 8; n++) h[n] <= h1[n];
		end
			
    endcase	 
  end									
endmodule

