library IEEE;
use ieee.std_logic_1164.all;
use work.all;

entity bus_tb is end entity;

architecture test of bus_tb is
  signal a, b, c, i, j, clk1, clk2: std_logic := '0';
  signal clock_mask: std_logic := '1';
begin
  
  c <= a;
  c <= b;
  
  PROCESS
	BEGIN
		FOR i IN std_ulogic LOOP --take each line from opc5_vector and connect it to a wire
			a<=i;
			c<=i;
			WAIT ON clk1;-- UNTIL clk1='1';

		END LOOP;
		
	END PROCESS;

	PROCESS
	BEGIN
		FOR j IN std_ulogic LOOP --take each line from opc5_vector and connect it to a wire
			b<=j;
			c<=j;
			WAIT ON clk2;-- UNTIL clk2='1';
		END LOOP;
		
	END PROCESS;

	clock_mask <= '0' AFTER 270 ps; -- end simulation

	clk1<=NOT(clk1) AND clock_mask AFTER 10 ps;
	clk2<=NOT(clk2) AND clock_mask AFTER 90 ps;

end architecture test;