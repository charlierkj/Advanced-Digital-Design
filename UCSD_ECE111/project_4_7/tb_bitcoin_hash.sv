module tb_bitcoin_hash();

parameter NUM_NONCES = 16;

logic          clk, reset_n, start;
logic   [15:0] message_addr, output_addr;
logic          done, mem_clk, mem_we;
logic   [15:0] mem_addr;
logic   [31:0] mem_write_data;
logic   [31:0] mem_read_data;

logic   [31:0] dpsram[0:16383]; // each row has 32 bits
logic   [31:0] dpsram_tb[0:16383]; // for result testing, testbench only

logic   [31:0] message_seed = 32'h01234567; // modify message_seed to test your design

logic   [31:0] h0[NUM_NONCES];
logic   [31:0] h1[NUM_NONCES];
logic   [31:0] h2[NUM_NONCES];
logic   [31:0] h3[NUM_NONCES];
logic   [31:0] h4[NUM_NONCES];
logic   [31:0] h5[NUM_NONCES];
logic   [31:0] h6[NUM_NONCES];
logic   [31:0] h7[NUM_NONCES];

logic   [31:0] fh0, fh1, fh2, fh3, fh4, fh5, fh6, fh7;
logic   [31:0] a, b, c, d, e, f, g, h;

logic   [31:0] s1, s0;
logic   [31:0] w[64];

int            num_errors;
int            cycles;
int            m, n, t;

// instantiate your design
bitcoin_hash bitcoin_hash_inst (clk, reset_n, start, message_addr, output_addr, done,
    mem_clk, mem_we, mem_addr, mem_write_data, mem_read_data);

// SHA256 K constants
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

// SHA256 hash round
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

// right rotation
function logic [31:0] rightrotate(input logic [31:0] x,
                                  input logic [7:0] r);
begin
    rightrotate = (x >> r) | (x << (32-r));
end
endfunction

// clock generator
always begin
    #10;
    clk = 1'b1;
    #10
    clk = 1'b0;
end

// main testbench
initial
begin
// PRELIMINARIES

    // RESET HASH CO-PROCESSOR

    @(posedge clk) reset_n = 0;
    for (m = 0; m < 2; m++) @(posedge clk);
    reset_n = 1;
    for (m = 0; m < 2; m++) @(posedge clk);

    // SET MESSAGE LOCATION

    message_addr = 32'd0;
    output_addr  = 32'd1000;

    // CREATE AND DISPLAY 19 WORD HEADER

    $display("---------------");
    $display("19 WORD HEADER:");
    $display("---------------");

    for (m = 0; m < 19; m++) begin // data generation
        if (m == 0)
            dpsram[message_addr+m] = message_seed;
        else
            dpsram[message_addr+m] = (dpsram[message_addr+m-1]<<1)|(dpsram[message_addr+m-1]>>31);
        dpsram_tb[m] = dpsram[message_addr+m];
        $display("%x", dpsram[message_addr+m]);
    end
    $display("***************************\n");

    // START PROCESSOR

    start = 1'b1;
    for (m = 0; m < 2; m++) @(posedge clk);
    start = 1'b0;

    // PERFORM PADDING OF MESSAGE

    dpsram_tb[19] = 32'h00000000; // THIS WILL BE REPLACED BY THE NONCE VALUE
    dpsram_tb[20] = 32'h80000000;
    for (m = 21; m < 31; m++) begin
        dpsram_tb[m] = 32'h00000000;
    end
    dpsram_tb[31] = 32'd640; // SIZE = 640 BITS

// 1. COMPUTE FIRST BLOCK OF FIRST HASH

    // WORD EXPANSION

    for (t = 0; t < 64; t++) begin
        if (t < 16) begin
            w[t] = dpsram_tb[t];
        end else begin
            s0 = rightrotate(w[t-15], 7) ^ rightrotate(w[t-15], 18) ^ (w[t-15] >> 3);
            s1 = rightrotate(w[t-2], 17) ^ rightrotate(w[t-2], 19) ^ (w[t-2] >> 10);
            w[t] = w[t-16] + s0 + w[t-7] + s1;
        end
    end

    // INITIAL HASH

    fh0 = 32'h6a09e667;
    fh1 = 32'hbb67ae85;
    fh2 = 32'h3c6ef372;
    fh3 = 32'ha54ff53a;
    fh4 = 32'h510e527f;
    fh5 = 32'h9b05688c;
    fh6 = 32'h1f83d9ab;
    fh7 = 32'h5be0cd19;

    a = 32'h6a09e667;
    b = 32'hbb67ae85;
    c = 32'h3c6ef372;
    d = 32'ha54ff53a;
    e = 32'h510e527f;
    f = 32'h9b05688c;
    g = 32'h1f83d9ab;
    h = 32'h5be0cd19;

    // HASH ROUNDS

    for (t = 0; t < 64; t++) begin
        {a, b, c, d, e, f, g, h} = sha256_op(a, b, c, d, e, f, g, h, w[t], t);
    end

    // FINAL HASH FOR FIRST BLOCK

    fh0 = fh0 + a;
    fh1 = fh1 + b;
    fh2 = fh2 + c;
    fh3 = fh3 + d;
    fh4 = fh4 + e;
    fh5 = fh5 + f;
    fh6 = fh6 + g;
    fh7 = fh7 + h;

// 2. COMPUTE SECOND BLOCK FOR FIRST HASH FOR EACH NONCE

    for (n = 0; n < NUM_NONCES; n++) begin

        // WORD EXPANSION

        dpsram_tb[19] = n; // REPLACE THE NONCE VALUE

        for (t = 0; t < 64; t++) begin
            if (t < 16) begin
                w[t] = dpsram_tb[t+16];
            end else begin
                s0 = rightrotate(w[t-15], 7) ^ rightrotate(w[t-15], 18) ^ (w[t-15] >> 3);
                s1 = rightrotate(w[t-2], 17) ^ rightrotate(w[t-2], 19) ^ (w[t-2] >> 10);
                w[t] = w[t-16] + s0 + w[t-7] + s1;
            end
        end

        // INITIAL HASH

        h0[n] = fh0;
        h1[n] = fh1;
        h2[n] = fh2;
        h3[n] = fh3;
        h4[n] = fh4;
        h5[n] = fh5;
        h6[n] = fh6;
        h7[n] = fh7;

        a = fh0;
        b = fh1;
        c = fh2;
        d = fh3;
        e = fh4;
        f = fh5;
        g = fh6;
        h = fh7;

        // HASH ROUNDS

        for (t = 0; t < 64; t++) begin
            {a, b, c, d, e, f, g, h} = sha256_op(a, b, c, d, e, f, g, h, w[t], t);
        end

        // FINAL HASH FOR SECOND BLOCK

        h0[n] = h0[n] + a;
        h1[n] = h1[n] + b;
        h2[n] = h2[n] + c;
        h3[n] = h3[n] + d;
        h4[n] = h4[n] + e;
        h5[n] = h5[n] + f;
        h6[n] = h6[n] + g;
        h7[n] = h7[n] + h;
    end

// 3. COMPUTE SECOND HASH FOR EACH NONCE

    for (n = 0; n < NUM_NONCES; n++) begin

        // WORD EXPANSION

        w[0] = h0[n];
        w[1] = h1[n];
        w[2] = h2[n];
        w[3] = h3[n];
        w[4] = h4[n];
        w[5] = h5[n];
        w[6] = h6[n];
        w[7] = h7[n];

        w[8] = 32'h80000000; // padding
        for (t = 9; t < 15; t++) begin
            w[t] = 32'h00000000;
        end
        w[15] = 32'd256; // SIZE = 256 BITS

        for (t = 16; t < 64; t++) begin
            s0 = rightrotate(w[t-15], 7) ^ rightrotate(w[t-15], 18) ^ (w[t-15] >> 3);
            s1 = rightrotate(w[t-2], 17) ^ rightrotate(w[t-2], 19) ^ (w[t-2] >> 10);
            w[t] = w[t-16] + s0 + w[t-7] + s1;
        end

        // INITIAL HASH

        h0[n] = 32'h6a09e667;
        h1[n] = 32'hbb67ae85;
        h2[n] = 32'h3c6ef372;
        h3[n] = 32'ha54ff53a;
        h4[n] = 32'h510e527f;
        h5[n] = 32'h9b05688c;
        h6[n] = 32'h1f83d9ab;
        h7[n] = 32'h5be0cd19;

        a = 32'h6a09e667;
        b = 32'hbb67ae85;
        c = 32'h3c6ef372;
        d = 32'ha54ff53a;
        e = 32'h510e527f;
        f = 32'h9b05688c;
        g = 32'h1f83d9ab;
        h = 32'h5be0cd19;

        // HASH ROUNDS

        for (t = 0; t < 64; t++) begin
            {a, b, c, d, e, f, g, h} = sha256_op(a, b, c, d, e, f, g, h, w[t], t);
        end

        // FINAL HASH FOR SECOND HASH

        h0[n] = h0[n] + a;
        h1[n] = h1[n] + b;
        h2[n] = h2[n] + c;
        h3[n] = h3[n] + d;
        h4[n] = h4[n] + e;
        h5[n] = h5[n] + f;
        h6[n] = h6[n] + g;
        h7[n] = h7[n] + h;
    end

// WAIT UNTIL EVERY IS DONE, THEN DISPLAY HASH RESULTS

    wait (done == 1);

// DISPLAY HASH RESULTS

    $display("---------------------");
    $display("COMPARE HASH RESULTS:");
    $display("---------------------");

    num_errors = 0;
    for (n = 0; n < NUM_NONCES; n++) begin
        if (h0[n] !== dpsram[output_addr+n]) begin
            $display("Correct H0[%2d] = %x\tYour H0[%2d] = %x\tERROR", n, h0[n], n, dpsram[output_addr+n]);
            num_errors = num_errors + 1;
        end else begin
            $display("Correct H0[%2d] = %x\tYour H0[%2d] = %x", n, h0[n], n, dpsram[output_addr+n]);
        end
    end
    $display("***************************\n");

    if (num_errors === 0)
        $display("CONGRATULATIONS! All your hash results are correct!\n");
    else
        $display("ERROR! %2d hash results are wrong!\n", num_errors);

    $display("Total number of cycles: %d\n\n", cycles);

    $display("***************************\n");

    $stop;
end

// memory model
always @(posedge mem_clk)
begin
    if (mem_we) // write
        dpsram[mem_addr] = mem_write_data;
    else // read
        mem_read_data = dpsram[mem_addr];
end

// track # of cycles
always @(posedge clk)
begin
    if (!reset_n)
        cycles = 0;
    else
        cycles = cycles + 1;
end

endmodule
