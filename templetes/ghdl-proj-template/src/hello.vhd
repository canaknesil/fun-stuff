--  Hello world program
use std.textio.all; -- Imports the standard textio package.
library ieee;
use ieee.std_logic_1164.all;

--  Defines a design entity, without any ports.
entity hello_world is
end hello_world;

architecture behaviour of hello_world is
  
  
  signal sig: std_logic := '0';

begin
  process
    variable l : line;
    variable c : String(1 to 1);
  begin
    write (l, String'("Hello world!"));
    writeline (output, l);

    if (sig = '0') then
      c := "0";
    else
      c := "1";
    end if;

    write (l, String'(c));
    writeline (output, l);

    wait;
  end process;
end behaviour;