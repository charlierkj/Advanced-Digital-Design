module tb_fibonacci_calculator;

  logic clk, reset_n;
  logic [4:0] input_s;
  logic begin_fibo;
  logic [15:0] fibo_out;
  logic done;

  // instantiate you design
  fibonacci_calculator uut(clk, reset_n, input_s, begin_fibo, fibo_out, done);

  // Clock Generator
  always
  begin
	#10;
	clk = 1'b1;
	#10
	clk = 1'b0;
  end

  initial
  begin
	/* ------------- Input of 5 ------------- */
        // Reset
	@(posedge clk) reset_n = 0;
	for (int k = 0; k < 2; k++) @(posedge clk);
	reset_n = 1;
	begin_fibo = 1'b0;
	for (int k = 0; k < 2; k++) @(posedge clk);
	
	// Inputs into module/ Assert begin_fibo
	input_s = 5;
	begin_fibo = 1'b1;
	for (int k = 0; k < 2; k++) @(posedge clk);
	begin_fibo = 1'b0;
	
	// Wait until calculation is done	
	wait (done == 1);

	// Idle cycles before next input
	for (int k = 0; k < 2; k++) @(posedge clk);

	// Display
	$display("\n---------------------\n");
	$display("Input: 5\n");

	if (fibo_out === 5)
	    $display("CORRECT RESULT: %d, GOOD JOB!\n", fibo_out);
	else
	    $display("INCORRECT RESULT: %d, SHOULD BE: 5\n", fibo_out);

	$display("---------------------\n");


	/* ------------- Input of 9 ------------- */
        // Reset
	@(posedge clk) reset_n = 0;
	for (int k = 0; k < 2; k++) @(posedge clk);
	reset_n = 1;
	begin_fibo = 1'b0;
	for (int k = 0; k < 2; k++) @(posedge clk);
	
	// Inputs into module/ Assert begin_fibo
	input_s = 9;
	begin_fibo = 1'b1;
	for (int k = 0; k < 2; k++) @(posedge clk);
	begin_fibo = 1'b0;
	
	// Wait until calculation is done	
	wait (done == 1);

	// Idle cycles before next input
	for (int k = 0; k < 2; k++) @(posedge clk);

	// Display
	$display("\n---------------------\n");
	$display("Input: 9\n");

	if (fibo_out === 34)
	    $display("CORRECT RESULT: %d, GOOD JOB!\n", fibo_out);
	else
	    $display("INCORRECT RESULT: %d, SHOULD BE: 34\n", fibo_out);

	$display("---------------------\n");



	/* ------------- Input of 12 ------------- */
        // Reset
	@(posedge clk) reset_n = 0;
	for (int k = 0; k < 2; k++) @(posedge clk);
	reset_n = 1;
	begin_fibo = 1'b0;
	for (int k = 0; k < 2; k++) @(posedge clk);
	
	// Inputs into module/ Assert begin_fibo
	input_s = 12;
	begin_fibo = 1'b1;
	for (int k = 0; k < 2; k++) @(posedge clk);
	begin_fibo = 1'b0;
	
	// Wait until calculation is done	
	wait (done == 1);

	// Idle cycles before next input
	for (int k = 0; k < 2; k++) @(posedge clk);

	// Display
	$display("\n---------------------\n");
	$display("Input: 12\n");

	if (fibo_out === 144)
	    $display("CORRECT RESULT: %d, GOOD JOB!\n", fibo_out);
	else
	    $display("INCORRECT RESULT: %d, SHOULD BE: 144\n", fibo_out);

	$display("---------------------\n");

	$stop;
  end
endmodule
