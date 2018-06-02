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
	
  logic [7:0] wc; //write counters
  logic block;
  logic [7:0] t;
  logic [31:0] h0[num_nonces], h1[num_nonces], h2[num_nonces], h3[num_nonces], h4[num_nonces], h5[num_nonces], h6[num_nonces], h7[num_nonces];
  logic [31:0] h2_0[8], h2_1[8], h2_2[8], h2_3[8], h2_4[8], h2_5[8], h2_6[8], h2_7[8], h2_8[8], h2_9[8], h2_10[8], h2_11[8], h2_12[8], h2_13[8], h2_14[8], h2_15[8];
  logic [31:0] w0[num_nonces], w1[num_nonces], w2[num_nonces], w3[num_nonces], w4[num_nonces], w5[num_nonces], w6[num_nonces], w7[num_nonces], w8[num_nonces], w9[num_nonces], w10[num_nonces], w11[num_nonces], w12[num_nonces], w13[num_nonces], w14[num_nonces], w15[num_nonces];
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
  
  function logic [511:0] wtnew;//function with no inputs
    logic [31:0] s0[num_nonces], s1[num_nonces];
	 
	 for (int n = 0; n < num_nonces; n++) begin
	   s0[n] = rightrotate(w1[n], 7) ^ rightrotate(w1[n], 18) ^ (w1[n]>>3);
	   s1[n] = rightrotate(w14[n], 17) ^ rightrotate(w14[n], 19) ^ (w14[n]>>10);
	 end
	 
	 wtnew = {w0[0] + s0[0]+ w9[0] + s1[0],
	          w0[1] + s0[1]+ w9[1] + s1[1],
	          w0[2] + s0[2]+ w9[2] + s1[2],
	          w0[3] + s0[3]+ w9[3] + s1[3],
	          w0[4] + s0[4]+ w9[4] + s1[4],
	          w0[5] + s0[5]+ w9[5] + s1[5],
	          w0[6] + s0[6]+ w9[6] + s1[6],
	          w0[7] + s0[7]+ w9[7] + s1[7],
	          w0[8] + s0[8]+ w9[8] + s1[8],
	          w0[9] + s0[9]+ w9[9] + s1[9],
	          w0[10] + s0[10]+ w9[10] + s1[10],
	          w0[11] + s0[11]+ w9[11] + s1[11],
	          w0[12] + s0[12]+ w9[12] + s1[12],
	          w0[13] + s0[13]+ w9[13] + s1[13],
	          w0[14] + s0[14]+ w9[14] + s1[14],
	          w0[15] + s0[15]+ w9[15] + s1[15]};
          
  endfunction

									
  
  assign mem_clk=clk;	
	
	


  always_ff @(posedge clk) begin
    case (state)
		  
	   PREP2: begin
		  for (int n = 0; n < num_nonces; n++) begin
		    {A[n], B[n], C[n], D[n], E[n], F[n], G[n], H[n]} <= {h0[n], h1[n], h2[n], h3[n], h4[n], h5[n], h6[n], h7[n]};
	     end
		end
		
		COMPUTE: begin
		  if (t<64) begin
		    for (int n = 0; n < num_nonces; n++) begin
			   {A[n], B[n], C[n], D[n], E[n], F[n], G[n], H[n]} <= sha256_op(A[n], B[n], C[n], D[n], E[n], F[n], G[n], temp[n]);
			 end		
		  end
		end
		
		COMPUTE2: begin
		  if (t<64) begin
		    for (int n = 0; n < num_nonces; n++) begin
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
				h0[n] <= 32'h6a09e667;
            h1[n] <= 32'hbb67ae85;
            h2[n] <= 32'h3c6ef372;
            h3[n] <= 32'ha54ff53a;
            h4[n] <= 32'h510e527f;
            h5[n] <= 32'h9b05688c;
            h6[n] <= 32'h1f83d9ab;
            h7[n] <= 32'h5be0cd19; 
				end
			
			   state <= PREP1;
				block <= 0;
		    end
			 
	   PREP1: begin
		    state <= PREP2;
			 mem_addr <= mem_addr + 1;			 
		end
			
	   PREP2: begin
			 for (int n = 0; n < num_nonces; n++) begin
			   w0[n] <= w1[n];
				w1[n] <= w2[n];
				w2[n] <= w3[n];
				w3[n] <= w4[n];
				w4[n] <= w5[n];
				w5[n] <= w6[n];
				w6[n] <= w7[n];
				w7[n] <= w8[n];
				w8[n] <= w9[n];
				w9[n] <= w10[n];
				w10[n] <= w11[n];
				w11[n] <= w12[n];
				w12[n] <= w13[n];
				w13[n] <= w14[n];
				w14[n] <= w15[n];
			 end
				
		    state <= PREP3;
			 if (phase == 0) begin
			 for (int n = 0; n < num_nonces; n++)
			 w15[n] <= mem_read_data;
			 mem_addr <= mem_addr + 1;
			 end else begin
			 w15[0] <= h2_0[index_h2];
			 w15[1] <= h2_1[index_h2];
			 w15[2] <= h2_2[index_h2];
			 w15[3] <= h2_3[index_h2];
			 w15[4] <= h2_4[index_h2];
			 w15[5] <= h2_5[index_h2];
			 w15[6] <= h2_6[index_h2];
			 w15[7] <= h2_7[index_h2];
			 w15[8] <= h2_8[index_h2];
			 w15[9] <= h2_9[index_h2];
			 w15[10] <= h2_10[index_h2];
			 w15[11] <= h2_11[index_h2];
			 w15[12] <= h2_12[index_h2];
			 w15[13] <= h2_13[index_h2];
			 w15[14] <= h2_14[index_h2];
			 w15[15] <= h2_15[index_h2];
			 
			 index_h2 <= index_h2 + 1;
			 end
	   end
		
		PREP3: begin
			 for (int n = 0; n < num_nonces; n++) begin
			     temp[n] <= w15[n] + k[t] + H[n];
				  w0[n] <= w1[n];
				  w1[n] <= w2[n];
				  w2[n] <= w3[n];
				  w3[n] <= w4[n];
				  w4[n] <= w5[n];
				  w5[n] <= w6[n];
				  w6[n] <= w7[n];
				  w7[n] <= w8[n];
				  w8[n] <= w9[n];
				  w9[n] <= w10[n];
				  w10[n] <= w11[n];
				  w11[n] <= w12[n];
				  w12[n] <= w13[n];
				  w13[n] <= w14[n];
				  w14[n] <= w15[n];
			 end
			 if (phase == 0) begin
			 for (int n = 0; n < num_nonces; n++)
			 w15[n] <= mem_read_data;
			 mem_addr <= mem_addr + 1;
			 state <= COMPUTE;
			 end else begin
			 state <= COMPUTE2;
			 w15[0] <= h2_0[index_h2];
			 w15[1] <= h2_1[index_h2];
			 w15[2] <= h2_2[index_h2];
			 w15[3] <= h2_3[index_h2];
			 w15[4] <= h2_4[index_h2];
			 w15[5] <= h2_5[index_h2];
			 w15[6] <= h2_6[index_h2];
			 w15[7] <= h2_7[index_h2];
			 w15[8] <= h2_8[index_h2];
			 w15[9] <= h2_9[index_h2];
			 w15[10] <= h2_10[index_h2];
			 w15[11] <= h2_11[index_h2];
			 w15[12] <= h2_12[index_h2];
			 w15[13] <= h2_13[index_h2];
			 w15[14] <= h2_14[index_h2];
			 w15[15] <= h2_15[index_h2];
			 index_h2 <= index_h2 + 1;
			 end
		end	 
						
		COMPUTE: begin
		    if (block == 0) begin 
			   if (t<64) begin 
				  for (int n = 0; n < num_nonces; n++) begin
				  w0[n] <= w1[n];
				  w1[n] <= w2[n];
				  w2[n] <= w3[n];
				  w3[n] <= w4[n];
				  w4[n] <= w5[n];
				  w5[n] <= w6[n];
				  w6[n] <= w7[n];
				  w7[n] <= w8[n];
				  w8[n] <= w9[n];
				  w9[n] <= w10[n];
				  w10[n] <= w11[n];
				  w11[n] <= w12[n];
				  w12[n] <= w13[n];
				  w13[n] <= w14[n];
				  w14[n] <= w15[n];
				  end
		        if (t<14) begin
				    for (int n = 0; n < num_nonces; n++)
			       w15[n] <= mem_read_data;
					 mem_addr <= mem_addr + 1;
		        end else begin
			       {w15[0], w15[1], w15[2], w15[3], w15[4], w15[5], w15[6], w15[7], w15[8], w15[9], w15[10], w15[11], w15[12], w15[13], w15[14], w15[15]} <= wtnew();
					 mem_addr <= 16;
		        end
				  t <= t + 1;
				  for (int n = 0; n < num_nonces; n++) begin
				    temp[n] <= w15[n] + k[t+1] + G[n];
				  end
				  state <= COMPUTE;
				end else begin
			     for (int n = 0; n < num_nonces; n++) begin
				    {h0[n], h1[n], h2[n], h3[n], h4[n], h5[n], h6[n], h7[n]} <= {h0[n] + A[n], h1[n] + B[n], h2[n] + C[n], h3[n] + D[n], h4[n] + E[n], h5[n] + F[n], h6[n] + G[n], h7[n] + H[n]};
                w0[n] <= 32'h00000000;
					 w1[n] <= 32'h00000000;
					 w2[n] <= 32'h00000000;
					 w3[n] <= 32'h00000000;
					 w4[n] <= 32'h00000000;
					 w5[n] <= 32'h00000000;
					 w6[n] <= 32'h00000000;
					 w7[n] <= 32'h00000000;
					 w8[n] <= 32'h00000000;
					 w9[n] <= 32'h00000000;
					 w10[n] <= 32'h00000000;
					 w11[n] <= 32'h00000000;
					 w12[n] <= 32'h00000000;
					 w13[n] <= 32'h00000000;
					 w14[n] <= 32'h00000000;
					 w15[n] <= 32'h00000000;
				  end
				  t <= 0;
				  block <= 1;
				  mem_addr <= mem_addr + 1;
				  state <= PREP2;
				end
			 end else begin 
			   if (t<64) begin
				  for (int n = 0; n < num_nonces; n++) begin
				  w0[n] <= w1[n];
				  w1[n] <= w2[n];
				  w2[n] <= w3[n];
				  w3[n] <= w4[n];
				  w4[n] <= w5[n];
				  w5[n] <= w6[n];
				  w6[n] <= w7[n];
				  w7[n] <= w8[n];
				  w8[n] <= w9[n];
				  w9[n] <= w10[n];
				  w10[n] <= w11[n];
				  w11[n] <= w12[n];
				  w12[n] <= w13[n];
				  w13[n] <= w14[n];
				  w14[n] <= w15[n];
				  end
				  if (t<14) begin
		          if (t<1) begin
                  for (int n = 0; n < num_nonces; n++) w15[n] <= mem_read_data;
					   mem_addr <= mem_addr + 1;
		          end 
					 else if (t == 1 ) begin
					   for (int n = 0; n < num_nonces; n++) w15[n] <= n;
					 end
				    else if (t == 2 ) begin
				      for (int n = 0; n < num_nonces; n++) w15[n] <= 32'h80000000;
					 end 
					 else if (t>2 && t<13) begin
					   for (int n = 0; n < num_nonces; n++) w15[n] <= 32'h00000000;
					 end else begin
					   for (int n = 0; n < num_nonces; n++) w15[n] <= 32'h00000280;
					 end
				  end else begin
			       {w15[0], w15[1], w15[2], w15[3], w15[4], w15[5], w15[6], w15[7], w15[8], w15[9], w15[10], w15[11], w15[12], w15[13], w15[14], w15[15]} <= wtnew();
		        end
				  for (int n = 0; n < num_nonces; n++) begin
				    temp[n] <= w15[n] + k[t+1] + G[n];
				  end
				  t <= t + 1;
				  state <= COMPUTE;
				end else begin
				  for (int n = 0; n < num_nonces; n++) begin
				  w0[n] <= 32'h00000000;
				  w1[n] <= 32'h00000000;
				  w2[n] <= 32'h00000000;
				  w3[n] <= 32'h00000000;
				  w4[n] <= 32'h00000000;
				  w5[n] <= 32'h00000000;
				  w6[n] <= 32'h00000000;
				  w7[n] <= 32'h00000000;
				  w8[n] <= 32'h00000000;
				  w9[n] <= 32'h00000000;
				  w10[n] <= 32'h00000000;
			  	  w11[n] <= 32'h00000000;
			     w12[n] <= 32'h00000000;
				  w13[n] <= 32'h00000000;
				  w14[n] <= 32'h00000000;
				  w15[n] <= 32'h00000000;
				  
				  h0[n] <= 32'h6a09e667;
              h1[n] <= 32'hbb67ae85;
              h2[n] <= 32'h3c6ef372;
              h3[n] <= 32'ha54ff53a;
              h4[n] <= 32'h510e527f;
              h5[n] <= 32'h9b05688c;
              h6[n] <= 32'h1f83d9ab;
              h7[n] <= 32'h5be0cd19; 
				  end
				  t <= 0;
				  index_h2 <= 0;
				  {h2_0[0], h2_0[1], h2_0[2], h2_0[3], h2_0[4], h2_0[5], h2_0[6], h2_0[7]} <= {h0[0] + A[0], h1[0] + B[0], h2[0] + C[0], h3[0] + D[0], h4[0] + E[0], h5[0] + F[0], h6[0] + G[0], h7[0] + H[0]};
				  {h2_1[0], h2_1[1], h2_1[2], h2_1[3], h2_1[4], h2_1[5], h2_1[6], h2_1[7]} <= {h0[1] + A[1], h1[1] + B[1], h2[1] + C[1], h3[1] + D[1], h4[1] + E[1], h5[1] + F[1], h6[1] + G[1], h7[1] + H[1]};
				  {h2_2[0], h2_2[1], h2_2[2], h2_2[3], h2_2[4], h2_2[5], h2_2[6], h2_2[7]} <= {h0[2] + A[2], h1[2] + B[2], h2[2] + C[2], h3[2] + D[2], h4[2] + E[2], h5[2] + F[2], h6[2] + G[2], h7[2] + H[2]};
				  {h2_3[0], h2_3[1], h2_3[2], h2_3[3], h2_3[4], h2_3[5], h2_3[6], h2_3[7]} <= {h0[3] + A[3], h1[3] + B[3], h2[3] + C[3], h3[3] + D[3], h4[3] + E[3], h5[3] + F[3], h6[3] + G[3], h7[3] + H[3]};
				  {h2_4[0], h2_4[1], h2_4[2], h2_4[3], h2_4[4], h2_4[5], h2_4[6], h2_4[7]} <= {h0[4] + A[4], h1[4] + B[4], h2[4] + C[4], h3[4] + D[4], h4[4] + E[4], h5[4] + F[4], h6[4] + G[4], h7[4] + H[4]};
				  {h2_5[0], h2_5[1], h2_5[2], h2_5[3], h2_5[4], h2_5[5], h2_5[6], h2_5[7]} <= {h0[5] + A[5], h1[5] + B[5], h2[5] + C[5], h3[5] + D[5], h4[5] + E[5], h5[5] + F[5], h6[5] + G[5], h7[5] + H[5]};
				  {h2_6[0], h2_6[1], h2_6[2], h2_6[3], h2_6[4], h2_6[5], h2_6[6], h2_6[7]} <= {h0[6] + A[6], h1[6] + B[6], h2[6] + C[6], h3[6] + D[6], h4[6] + E[6], h5[6] + F[6], h6[6] + G[6], h7[6] + H[6]};
				  {h2_7[0], h2_7[1], h2_7[2], h2_7[3], h2_7[4], h2_7[5], h2_7[6], h2_7[7]} <= {h0[7] + A[7], h1[7] + B[7], h2[7] + C[7], h3[7] + D[7], h4[7] + E[7], h5[7] + F[7], h6[7] + G[7], h7[7] + H[7]};
				  {h2_8[0], h2_8[1], h2_8[2], h2_8[3], h2_8[4], h2_8[5], h2_8[6], h2_8[7]} <= {h0[8] + A[8], h1[8] + B[8], h2[8] + C[8], h3[8] + D[8], h4[8] + E[8], h5[8] + F[8], h6[8] + G[8], h7[8] + H[8]};
				  {h2_9[0], h2_9[1], h2_9[2], h2_9[3], h2_9[4], h2_9[5], h2_9[6], h2_9[7]} <= {h0[9] + A[9], h1[9] + B[9], h2[9] + C[9], h3[9] + D[9], h4[9] + E[9], h5[9] + F[9], h6[9] + G[9], h7[9] + H[9]};
				  {h2_10[0], h2_10[1], h2_10[2], h2_10[3], h2_10[4], h2_10[5], h2_10[6], h2_10[7]} <= {h0[10] + A[10], h1[10] + B[10], h2[10] + C[10], h3[10] + D[10], h4[10] + E[10], h5[10] + F[10], h6[10] + G[10], h7[10] + H[10]};
				  {h2_11[0], h2_11[1], h2_11[2], h2_11[3], h2_11[4], h2_11[5], h2_11[6], h2_11[7]} <= {h0[11] + A[11], h1[11] + B[11], h2[11] + C[11], h3[11] + D[11], h4[11] + E[11], h5[11] + F[11], h6[11] + G[11], h7[11] + H[11]};
				  {h2_12[0], h2_12[1], h2_12[2], h2_12[3], h2_12[4], h2_12[5], h2_12[6], h2_12[7]} <= {h0[12] + A[12], h1[12] + B[12], h2[12] + C[12], h3[12] + D[12], h4[12] + E[12], h5[12] + F[12], h6[12] + G[12], h7[12] + H[12]};
				  {h2_13[0], h2_13[1], h2_13[2], h2_13[3], h2_13[4], h2_13[5], h2_13[6], h2_13[7]} <= {h0[13] + A[13], h1[13] + B[13], h2[13] + C[13], h3[13] + D[13], h4[13] + E[13], h5[13] + F[13], h6[13] + G[13], h7[13] + H[13]};
				  {h2_14[0], h2_14[1], h2_14[2], h2_14[3], h2_14[4], h2_14[5], h2_14[6], h2_14[7]} <= {h0[14] + A[14], h1[14] + B[14], h2[14] + C[14], h3[14] + D[14], h4[14] + E[14], h5[14] + F[14], h6[14] + G[14], h7[14] + H[14]};
				  {h2_15[0], h2_15[1], h2_15[2], h2_15[3], h2_15[4], h2_15[5], h2_15[6], h2_15[7]} <= {h0[15] + A[15], h1[15] + B[15], h2[15] + C[15], h3[15] + D[15], h4[15] + E[15], h5[15] + F[15], h6[15] + G[15], h7[15] + H[15]};
				  state <= PREP2;
				  phase <= 1;
			  end
			end
		end
		
		COMPUTE2: begin
		    if (t<64) begin
			     for (int n = 0; n < num_nonces; n++) begin
				  w0[n] <= w1[n];
				  w1[n] <= w2[n];
				  w2[n] <= w3[n];
				  w3[n] <= w4[n];
				  w4[n] <= w5[n];
				  w5[n] <= w6[n];
				  w6[n] <= w7[n];
				  w7[n] <= w8[n];
				  w8[n] <= w9[n];
				  w9[n] <= w10[n];
				  w10[n] <= w11[n];
				  w11[n] <= w12[n];
				  w12[n] <= w13[n];
				  w13[n] <= w14[n];
				  w14[n] <= w15[n];
				  end
				  if (t<14) begin
		          if (t<6) begin
                  w15[0] <= h2_0[index_h2];
			         w15[1] <= h2_1[index_h2];
			         w15[2] <= h2_2[index_h2];
			         w15[3] <= h2_3[index_h2];
			         w15[4] <= h2_4[index_h2];
			         w15[5] <= h2_5[index_h2];
			         w15[6] <= h2_6[index_h2];
			         w15[7] <= h2_7[index_h2];
			         w15[8] <= h2_8[index_h2];
			         w15[9] <= h2_9[index_h2];
			         w15[10] <= h2_10[index_h2];
			         w15[11] <= h2_11[index_h2];
			         w15[12] <= h2_12[index_h2];
			         w15[13] <= h2_13[index_h2];
			         w15[14] <= h2_14[index_h2];
			         w15[15] <= h2_15[index_h2];
					   index_h2 <= index_h2 + 1;
		          end 
				    else if (t == 6 ) begin
				      for (int n = 0; n < num_nonces; n++) w15[n] <= 32'h80000000;
					 end 
					 else if (t>6 && t<13) begin
					   for (int n = 0; n < num_nonces; n++) w15[n] <= 32'h00000000;
					 end else begin
					   for (int n = 0; n < num_nonces; n++) w15[n] <= 32'h00000100;
					 end
				  end else begin
			       {w15[0], w15[1], w15[2], w15[3], w15[4], w15[5], w15[6], w15[7], w15[8], w15[9], w15[10], w15[11], w15[12], w15[13], w15[14], w15[15]} <= wtnew();
		        end
				  for (int n = 0; n < num_nonces; n++) begin
				    temp[n] <= w15[n] + k[t+1] + G[n];
				  end
				  t <= t + 1;
				  state <= COMPUTE2;
			  end else begin
		        mem_we <= 1;
			     mem_addr <= output_addr;
	           mem_write_data <= h0[0] + A[0];
              wc <= 1;
				  state <= WRITE;
			  end
		end		 
			
		WRITE: begin
		    if (wc < num_nonces) begin
			   mem_addr <= output_addr + wc;
				mem_write_data <= h0[wc] + A[wc];
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

