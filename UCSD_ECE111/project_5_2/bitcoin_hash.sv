// Bitcoin Hash Module
// By Kejia Ren

module bitcoin_hash (input logic clk, reset_n, start, 
									input logic[15:0] message_addr, output_addr,
									output logic done, mem_clk, mem_we,
									output logic [15:0] mem_addr,
									output logic [31:0] mem_write_data,
									input logic [31:0] mem_read_data);
									
  enum logic[2:0] {IDLE=3'b000, PREP1=3'b001, PREP2=3'b010, PREP3=3'b011, COMPUTE=3'b100, COMPUTE2=3'b101, WRITE=3'b110} state;
	
  parameter num_nonces = 16;
	
  logic [4:0] wc; //write counters
  logic block;
  logic [6:0] t;
  logic [31:0] h[8][num_nonces];
  logic [31:0] h2[num_nonces][8];
  logic [31:0] w[16][num_nonces];
  logic [31:0] A[num_nonces], B[num_nonces], C[num_nonces], D[num_nonces], E[num_nonces], F[num_nonces], G[num_nonces], H[num_nonces];
  logic [31:0] temp[num_nonces];
  logic phase;
  logic [3:0] index_h2;
    	
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
  
  
  function logic [31:0] wtnew(input logic [3:0] n);
    logic [31:0] s0, s1;
	
	 s0 = rightrotate(w[1][n], 7) ^ rightrotate(w[1][n], 18) ^ (w[1][n]>>3);
	 s1 = rightrotate(w[14][n], 17) ^ rightrotate(w[14][n], 19) ^ (w[14][n]>>10);
	 wtnew = w[0][n] + s0+ w[9][n] + s1;      
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
				phase <= 0;
				t <= 0;
				
				for (int n = 0; n < num_nonces; n++) begin
				h[0][n] <= 32'h6a09e667;
            h[1][n] <= 32'hbb67ae85;
            h[2][n] <= 32'h3c6ef372;
            h[3][n] <= 32'ha54ff53a;
            h[4][n] <= 32'h510e527f;
            h[5][n] <= 32'h9b05688c;
            h[6][n] <= 32'h1f83d9ab;
            h[7][n] <= 32'h5be0cd19; 
				end
			
			   state <= PREP1;
				block <= 0;
		    end
			 
	   PREP1: begin
		    state <= PREP2;
			 mem_addr <= mem_addr + 1;			 
		end
			
	   PREP2: begin				
		    state <= PREP3;
			 if (phase == 0) begin
			   for (int n = 0; n < num_nonces; n++) w[15][n] <= mem_read_data;
			   mem_addr <= mem_addr + 1;
			 end else begin
			   for (int n = 0; n < num_nonces; n++) w[15][n] <= h2[n][index_h2];
			   index_h2 <= index_h2 + 1;
			 end
			 for (int n = 0; n < num_nonces; n++) begin
			   for (int m = 0; m < 15; m++) w[m][n] <= w[m+1][n];
			   {A[n], B[n], C[n], D[n], E[n], F[n], G[n], H[n]} <= {h[0][n], h[1][n], h[2][n], h[3][n], h[4][n], h[5][n], h[6][n], h[7][n]};
			 end
	   end
		
		PREP3: begin
			 for (int n = 0; n < num_nonces; n++) begin
			   temp[n] <= w[15][n] + k[t] + H[n];
			   for (int m = 0; m < 15; m++) w[m][n] <= w[m+1][n];
			 end
			 if (phase == 0) begin
			   for (int n = 0; n < num_nonces; n++) w[15][n] <= mem_read_data;
			   mem_addr <= mem_addr + 1;
			   state <= COMPUTE;
			 end else begin
			   state <= COMPUTE2;
			   for (int n = 0; n < num_nonces; n++) w[15][n] <= h2[n][index_h2];
			   index_h2 <= index_h2 + 1;
			 end
		end	 
						
		COMPUTE: begin
		    if (block == 0) begin 
			   if (t<64) begin 
				  for (int n = 0; n < num_nonces; n++) begin
				  for (int m = 0; m < 15; m++)
				  w[m][n] <= w[m+1][n];
				  end
		        if (t<14) begin
				    for (int n = 0; n < num_nonces; n++) w[15][n] <= mem_read_data;
					 mem_addr <= mem_addr + 1;
		        end else begin
			       for (int n = 0; n < num_nonces; n++) w[15][n] <= wtnew(n);
					 mem_addr <= message_addr + 16;
		        end
				  t <= t + 1;
				  for (int n = 0; n < num_nonces; n++) begin
				    temp[n] <= w[15][n] + k[t+1] + G[n];
				    {A[n], B[n], C[n], D[n], E[n], F[n], G[n], H[n]} <= sha256_op(A[n], B[n], C[n], D[n], E[n], F[n], G[n], temp[n]);
				  end
				  state <= COMPUTE;
				end else begin
			     for (int n = 0; n < num_nonces; n++) begin
				    {h[0][n], h[1][n], h[2][n], h[3][n], h[4][n], h[5][n], h[6][n], h[7][n]} <= {h[0][n] + A[n], h[1][n] + B[n], h[2][n] + C[n], h[3][n] + D[n], h[4][n] + E[n], h[5][n] + F[n], h[6][n] + G[n], h[7][n] + H[n]};
                for (int m = 0; m < 16; m++) w[m][n] <= 32'h00000000;
				  end
				  t <= 0;
				  block <= 1;
				  mem_addr <= mem_addr + 1;
				  state <= PREP2;
				end
			 end else begin 
			   if (t<64) begin
				  for (int n = 0; n < num_nonces; n++) begin
				  for (int m = 0; m < 15; m++)
				  w[m][n] <= w[m+1][n];
				  end
				  if (t<14) begin
		          if (t<1) begin
                  for (int n = 0; n < num_nonces; n++) w[15][n] <= mem_read_data;
					   mem_addr <= mem_addr + 1;
		          end 
					 else if (t == 1 ) begin
					   for (int n = 0; n < num_nonces; n++) w[15][n] <= n;
					 end
				    else if (t == 2 ) begin
				      for (int n = 0; n < num_nonces; n++) w[15][n] <= 32'h80000000;
					 end 
					 else if (t>2 && t<13) begin
					   for (int n = 0; n < num_nonces; n++) w[15][n] <= 32'h00000000;
					 end else begin
					   for (int n = 0; n < num_nonces; n++) w[15][n] <= 32'h00000280;
					 end
				  end else begin
			       for (int n = 0; n < num_nonces; n++) w[15][n] <= wtnew(n);
		        end
				  for (int n = 0; n < num_nonces; n++) begin
				    temp[n] <= w[15][n] + k[t+1] + G[n];
					 {A[n], B[n], C[n], D[n], E[n], F[n], G[n], H[n]} <= sha256_op(A[n], B[n], C[n], D[n], E[n], F[n], G[n], temp[n]);
				  end
				  t <= t + 1;
				  state <= COMPUTE;
				end else begin
				  for (int n = 0; n < num_nonces; n++) begin
				    for (int m = 0; m < 16; m++) w[m][n] <= 32'h00000000;
				  
				    h[0][n] <= 32'h6a09e667;
                h[1][n] <= 32'hbb67ae85;
                h[2][n] <= 32'h3c6ef372;
                h[3][n] <= 32'ha54ff53a;
                h[4][n] <= 32'h510e527f;
                h[5][n] <= 32'h9b05688c;
                h[6][n] <= 32'h1f83d9ab;
                h[7][n] <= 32'h5be0cd19; 
					 
					 {h2[n][0], h2[n][1], h2[n][2], h2[n][3], h2[n][4], h2[n][5], h2[n][6], h2[n][7]} <= {h[0][n] + A[n], h[1][n] + B[n], h[2][n] + C[n], h[3][n] + D[n], h[4][n] + E[n], h[5][n] + F[n], h[6][n] + G[n], h[7][n] + H[n]};
				  end
				  t <= 0;
				  index_h2 <= 0;
				  state <= PREP2;
				  phase <= 1;
			  end
			end
		end
		
		COMPUTE2: begin
		    if (t<64) begin
			     for (int n = 0; n < num_nonces; n++) begin
				  for (int m = 0; m < 15; m++)
				  w[m][n] <= w[m+1][n];
				  end
				  if (t<14) begin
		          if (t<6) begin
                  for (int n = 0; n < num_nonces; n++)
                  w[15][n] <= h2[n][index_h2];
					   index_h2 <= index_h2 + 1;
		          end 
				    else if (t == 6 ) begin
				      for (int n = 0; n < num_nonces; n++) w[15][n] <= 32'h80000000;
					 end 
					 else if (t>6 && t<13) begin
					   for (int n = 0; n < num_nonces; n++) w[15][n] <= 32'h00000000;
					 end else begin
					   for (int n = 0; n < num_nonces; n++) w[15][n] <= 32'h00000100;
					 end
				  end else begin
			       for (int n = 0; n < num_nonces; n++) w[15][n] <= wtnew(n);
		        end
				  for (int n = 0; n < num_nonces; n++) begin
				    temp[n] <= w[15][n] + k[t+1] + G[n];
					 {A[n], B[n], C[n], D[n], E[n], F[n], G[n], H[n]} <= sha256_op(A[n], B[n], C[n], D[n], E[n], F[n], G[n], temp[n]);
				  end
				  t <= t + 1;
				  state <= COMPUTE2;
			  end else begin
		        mem_we <= 1;
			     mem_addr <= output_addr;
	           mem_write_data <= h[0][0] + A[0];
              wc <= 1;
				  state <= WRITE;
			  end
		end		 
			
		WRITE: begin
		    if (wc < num_nonces) begin
			   mem_addr <= output_addr + wc;
				mem_write_data <= h[0][wc] + A[wc];
				state <= WRITE;
				wc <= wc + 1;
   		 end else begin
			   state <= IDLE;
				done <= 1;
			 end
		end
			
    endcase	 
  end									
endmodule

