module tb_byte_rotation;

logic        clk, reset_n, start;
logic [15:0] message_addr, size, output_addr;
logic        done, mem_clk, mem_we;
logic [15:0] mem_addr;
logic [31:0] mem_write_data, mem_read_data;

logic [31:0] dpsram[0:4095];
logic [31:0] dpsram_test[0:4095];
logic [31:0] message_seed = 32'h01234567; // modify message_seed to test your design

int message_length = 16; // in words // change this number to test your design
int error_flag;
int cycles;

// instantiate you design
byte_rotation br (clk, reset_n, start, message_addr, size, output_addr, done,
	mem_clk, mem_we, mem_addr, mem_write_data, mem_read_data);
	
// byte rotate
function logic [31:0] byte_rotate(input logic [31:0] value);
    byte_rotate = {value[23:16], value[15:8], value[7:0], value[31:24]};
endfunction

// clock generator
always
begin
    #10;
    clk = 1'b1;
    #10
    clk = 1'b0;
end

// main testbench
initial
begin

// reset processor

    @(posedge clk) reset_n = 0;
    for (int k = 0; k < 2; k++) @(posedge clk);
    reset_n = 1;
    for (int k = 0; k < 2; k++) @(posedge clk);

// display message

    $display("-------\n");
    $display("Message\n");
    $display("-------\n");

    dpsram[0] = message_seed;
    dpsram_test[0] = byte_rotate(message_seed); // byte rotation // for testbench only

    $display("%x\n", dpsram[0]);

    for (int k = 1; k < message_length; k++) begin // data generation
        dpsram[k] = (dpsram[k-1]<<1)|(dpsram[k-1]>>31);
        dpsram_test[k] = byte_rotate(dpsram[k]); // byte rotation
        $display("%x\n", dpsram[k]);
    end

// set inputs to module

    message_addr = 32'h0;
    size = message_length;
    output_addr = message_length;
    start = 1'b1;
    for (int k = 0; k < 2; k++) @(posedge clk);
    start = 1'b0;

// wait until done

    wait (done == 1);

// display converted message

    $display("--------------------------\n");
    $display("correct converted message:\n");
    $display("--------------------------\n");

    for (int k = 0; k < message_length; k++) begin // data generation
        $display("%x\n", dpsram_test[k]);
    end

    error_flag = 0;

    $display("-----------------------\n");
    $display("your converted message:\n");
    $display("-----------------------\n");

    for (int k = 0; k < message_length; k++) begin // data generation
        if (dpsram[message_length+k] === dpsram_test[k]) begin
            $display("%x\n", dpsram[message_length+k]);
        end else begin // incorrect !!!
            $display("%x\t %x\t **ERROR**\n", dpsram[message_length+k], dpsram_test[k]);
            error_flag = 1;
        end
    end

    if (error_flag) begin
        $display("ERROR! At least one of the converted word is wrong!\n");
    end else begin
        $display("Congratulations! You have the correct converted results!\n");
        $display("Total number of cycles: %d\n\n", cycles);
    end

    $stop;
end

// memory model
always @(posedge mem_clk)
begin
    if (mem_we == 1'b1) // write
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
