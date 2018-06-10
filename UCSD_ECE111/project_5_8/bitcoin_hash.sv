// Bitcoin Hash Module
// By Kejia Ren

module bitcoin_hash (input logic clk, reset_n, start, 
									input logic[15:0] message_addr, output_addr,
									output logic done, mem_clk, mem_we,
									output logic [15:0] mem_addr,
									output logic [31:0] mem_write_data,
									input logic [31:0] mem_read_data);
									
  enum logic[3:0] {IDLE=4'b0000, PREP11=4'b0001, PREP12=4'b0010, PREP13=4'b0011, COMPUTE1=4'b0100, PREP21=4'b0101, PREP22=4'b0110, COMPUTE2=4'b0111, PREP31=4'b1000, PREP32=4'b1001, COMPUTE3=4'b1010, POST=4'b1011, WRITE=4'b1100} state;
	
  parameter num_nonces = 16;
  
  logic [4:0] wc; //write counters
  logic [6:0] t;
  logic [31:0] h[num_nonces][8];
  logic [31:0] h2[num_nonces][8];
  logic [31:0] w[num_nonces][16];
  logic [31:0] A[num_nonces], B[num_nonces], C[num_nonces], D[num_nonces], E[num_nonces], F[num_nonces], G[num_nonces], H[num_nonces];
  logic [31:0] temp[num_nonces];
  logic [31:0] k1;
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
	
	 s0 = rightrotate(w[n][1], 7) ^ rightrotate(w[n][1], 18) ^ (w[n][1]>>3);
	 s1 = rightrotate(w[n][14], 17) ^ rightrotate(w[n][14], 19) ^ (w[n][14]>>10);
	 wtnew = w[n][0] + s0+ w[n][9] + s1;      
  endfunction
									
  
  assign mem_clk=clk;
 

  
  always_ff @(posedge clk) begin
    case (state)
	   PREP13: begin
	     for (int n = 0; n < num_nonces; n++) begin
		    temp[n] <= w[n][15] + k1 + h[n][7];
			 {A[n], B[n], C[n], D[n], E[n], F[n], G[n], H[n]} <= {h[n][0], h[n][1], h[n][2], h[n][3], h[n][4], h[n][5], h[n][6], h[n][7]};
	     end
		end
		
		COMPUTE1: begin
		  if (t<65) begin
		  for (int n = 0; n < num_nonces; n++) begin
		    temp[n] <= w[n][15] + k1 + G[n];
			 {A[n], B[n], C[n], D[n], E[n], F[n], G[n], H[n]} <= sha256_op(A[n], B[n], C[n], D[n], E[n], F[n], G[n], temp[n]);
		  end
		  end
		end
		
		PREP22: begin
	     for (int n = 0; n < num_nonces; n++) begin
		    temp[n] <= w[n][15] + k1 + h[n][7];
			 {A[n], B[n], C[n], D[n], E[n], F[n], G[n], H[n]} <= {h[n][0], h[n][1], h[n][2], h[n][3], h[n][4], h[n][5], h[n][6], h[n][7]};
	     end
		end 
		
		COMPUTE2: begin
		  if (t<65) begin
		  for (int n = 0; n < num_nonces; n++) begin
		    temp[n] <= w[n][15] + k1 + G[n];
			 {A[n], B[n], C[n], D[n], E[n], F[n], G[n], H[n]} <= sha256_op(A[n], B[n], C[n], D[n], E[n], F[n], G[n], temp[n]);
		  end
		  end
		end
		
		PREP32: begin
	     for (int n = 0; n < num_nonces; n++) begin
		    temp[n] <= w[n][15] + k1 + h[n][7];
			 {A[n], B[n], C[n], D[n], E[n], F[n], G[n], H[n]} <= {h[n][0], h[n][1], h[n][2], h[n][3], h[n][4], h[n][5], h[n][6], h[n][7]};
	     end
		end 
		
		COMPUTE3: begin
		  if (t<65) begin
		  for (int n = 0; n < num_nonces; n++) begin
		    temp[n] <= w[n][15] + k1 + G[n];
			 {A[n], B[n], C[n], D[n], E[n], F[n], G[n], H[n]} <= sha256_op(A[n], B[n], C[n], D[n], E[n], F[n], G[n], temp[n]);
		  end
		  end
		end
		
	 endcase
  end
	   
									
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
				t <= 0;			
			   state <= PREP11;
		    end
			 
	   PREP11: begin
		    state <= PREP12;				
			 mem_addr <= mem_addr + 1;			 
		end
			
	   PREP12: begin
			 for (int n = 0; n < num_nonces; n++) begin
				w[n][15] <= mem_read_data;
			   
				h[n][0] <= 32'h6a09e667;
            h[n][1] <= 32'hbb67ae85;
            h[n][2] <= 32'h3c6ef372;
            h[n][3] <= 32'ha54ff53a;
            h[n][4] <= 32'h510e527f;
            h[n][5] <= 32'h9b05688c;
            h[n][6] <= 32'h1f83d9ab;
            h[n][7] <= 32'h5be0cd19;
			 
			 end
			 state <= PREP13;
			 mem_addr <= mem_addr + 1;
			 k1 <= k[t];
	   end
		
		PREP13: begin
		    for (int n = 0; n < num_nonces; n++) begin
				for (int m = 0; m < 15; m++) w[n][m] <= w[n][m+1];
				w[n][15] <= mem_read_data;
			 end
			 mem_addr <= mem_addr + 1;
			 state <= COMPUTE1;
			 k1 <= k[t+1];
			 t <= t + 1;
		end	 
						
		COMPUTE1: begin
			   if (t<65) begin 
				  for (int n = 0; n < num_nonces; n++) begin
				    for (int m = 0; m < 15; m++) w[n][m] <= w[n][m+1];
				  end
		        if (t<15) begin
				    for (int n = 0; n < num_nonces; n++) w[n][15] <= mem_read_data;
					 mem_addr <= mem_addr + 1;
		        end else begin
			       for (int n = 0; n < num_nonces; n++) w[n][15] <= wtnew(n);
					 mem_addr <= message_addr + 16;
		        end
				  t <= t + 1;
				  state <= COMPUTE1;
				  k1 <= k[t+1];
				end else begin 
				  t <= 0;
				  mem_addr <= mem_addr + 1;
				  state <= PREP21;
				end
      end
		
		PREP21: begin
		    for (int n = 0; n < num_nonces; n++) begin
				w[n][15] <= mem_read_data;
			   {h[n][0], h[n][1], h[n][2], h[n][3], h[n][4], h[n][5], h[n][6], h[n][7]} <= {h[n][0] + A[n], h[n][1] + B[n], h[n][2] + C[n], h[n][3] + D[n], h[n][4] + E[n], h[n][5] + F[n], h[n][6] + G[n], h[n][7] + H[n]};
			 end
			 state <= PREP22;
			 mem_addr <= mem_addr + 1;
			 k1 <= k[t];
		end
		
		PREP22: begin
		    for (int n = 0; n < num_nonces; n++) begin
				for (int m = 0; m < 15; m++) w[n][m] <= w[n][m+1];
				w[n][15] <= mem_read_data;
			 end
			 mem_addr <= mem_addr + 1;
			 state <= COMPUTE2;
			 k1 <= k[t+1];
			 t <= t + 1;
		end
		
		COMPUTE2: begin
			   if (t<65) begin
				  for (int n = 0; n < num_nonces; n++) begin
				    for (int m = 0; m < 15; m++) w[n][m] <= w[n][m+1];
				  end
				  if (t<15) begin
		          if (t<2) begin
                  for (int n = 0; n < num_nonces; n++) w[n][15] <= mem_read_data;
					   mem_addr <= mem_addr + 1;
		          end 
					 else if (t == 2 ) begin
					   for (int n = 0; n < num_nonces; n++) w[n][15] <= n;
					 end
				    else if (t == 3 ) begin
				      for (int n = 0; n < num_nonces; n++) w[n][15] <= 32'h80000000;
					 end 
					 else if (t>3 && t<14) begin
					   for (int n = 0; n < num_nonces; n++) w[n][15] <= 32'h00000000;
					 end else begin
					   for (int n = 0; n < num_nonces; n++) w[n][15] <= 32'h00000280;
					 end
				  end else begin
			       for (int n = 0; n < num_nonces; n++) w[n][15] <= wtnew(n);
		        end
				  t <= t + 1;
				  state <= COMPUTE2;
				  k1 <= k[t+1];
				end else begin
				  for (int n = 0; n < num_nonces; n++) begin
				    {h2[n][0], h2[n][1], h2[n][2], h2[n][3], h2[n][4], h2[n][5], h2[n][6], h2[n][7]} <= {h[n][0] + A[n], h[n][1] + B[n], h[n][2] + C[n], h[n][3] + D[n], h[n][4] + E[n], h[n][5] + F[n], h[n][6] + G[n], h[n][7] + H[n]};
				  end
				  t <= 0;
				  index_h2 <= 0;
              state <= 	PREP31;
			  end
		end
		
		
	   PREP31: begin
		    for (int n = 0; n < num_nonces; n++) begin
			   w[n][15] <= h2[n][index_h2];
			   
				h[n][0] <= 32'h6a09e667;
            h[n][1] <= 32'hbb67ae85;
            h[n][2] <= 32'h3c6ef372;
            h[n][3] <= 32'ha54ff53a;
            h[n][4] <= 32'h510e527f;
            h[n][5] <= 32'h9b05688c;
            h[n][6] <= 32'h1f83d9ab;
            h[n][7] <= 32'h5be0cd19; 
			 end
			 state <= PREP32;
			 index_h2 <= index_h2 + 1;
			 k1 <= k[t];
		end
		
		PREP32: begin
			 for (int n = 0; n < num_nonces; n++) begin
			   for (int m = 0; m < 15; m++) w[n][m] <= w[n][m+1];
			   w[n][15] <= h2[n][index_h2];
			 end
			 state <= COMPUTE3;
			 index_h2 <= index_h2 + 1;
			 k1 <= k[t+1];
			 t <= t + 1;
		end
		
		COMPUTE3: begin
		    if (t<65) begin
				  for (int n = 0; n < num_nonces; n++) begin
				    for (int m = 0; m < 15; m++) w[n][m] <= w[n][m+1];
				  end
				  if (t<15) begin
		          if (t<7) begin
					   for (int n = 0; n < num_nonces; n++)
                  w[n][15] <= h2[n][index_h2];
					   index_h2 <= index_h2 + 1;
		          end 
				    else if (t == 7 ) begin
				      for (int n = 0; n < num_nonces; n++) w[n][15] <= 32'h80000000;
					 end 
					 else if (t>7 && t<14) begin
					   for (int n = 0; n < num_nonces; n++) w[n][15] <= 32'h00000000;
					 end else begin
					   for (int n = 0; n < num_nonces; n++) w[n][15] <= 32'h00000100;
					 end
				  end else begin
			       for (int n = 0; n < num_nonces; n++) w[n][15] <= wtnew(n);
		        end
				  t <= t + 1;
				  state <= COMPUTE3;
				  k1 <= k[t+1];
			  end else begin
			     for (int n = 0; n < num_nonces; n++) begin
				    h[n][0] <= h[n][0] + A[n];
				  end
				  state <= POST;
			  end
		end

      POST: begin
       	 mem_we <= 1;
			 mem_addr <= output_addr;
		    mem_write_data <= h[0][0];
		    state <= WRITE;
		    wc <= 1;
		end		
	
		WRITE: begin
          if (wc < num_nonces) begin
			   mem_addr <= output_addr + wc;
				mem_write_data <= h[wc][0];
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

