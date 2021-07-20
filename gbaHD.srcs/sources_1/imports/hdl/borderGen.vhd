-----------------------------------------------------------------------
-- Title: Border Generator
-- Author: zwenergy
-----------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
entity borderGen is 
  generic(
    xMin : integer;
    xMax : integer;
    yMin : integer;
    yMax : integer
  );
  port (
    x : in integer range xMin to xMax;
    y : in integer range yMin to yMax; 
    rgb : out std_logic_vector( 23 downto 0 )
  );
end borderGen;
architecture rtl of borderGen is
begin

  rgb <= ( others => '0' );
  
end rtl;
