-----------------------------------------------------------------------
-- Title: Grid generator
-- Author: zwenergy
-----------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.all;

entity gridGen is
  generic (
    gridLineChange : unsigned( 7 downto 0 )
  );
  port (
    pxlInRgb : in std_logic_vector( 23 downto 0 );
    gridAct : in std_logic;
    brightGrid : in std_logic;
    pxlOutRgb : out std_logic_vector( 23 downto 0 )
  );
end gridGen;

architecture rtl of gridGen is

-- signal pxlInRed, pxlInGreen, pxlInBlue : std_logic_vector ( 7 downto 0 );
-- signal pxlOutRed, pxlOutGreen, pxlOutBlue : std_logic_vector ( 7 downto 0 );

alias pxlInRed : std_logic_vector(7 downto 0) is pxlInRgb(7 downto 0);
alias pxlInBlue : std_logic_vector(7 downto 0) is pxlInRgb(15 downto 8);
alias pxlInGreen : std_logic_vector(7 downto 0) is pxlInRgb(23 downto 16);

alias pxlOutRed : std_logic_vector(7 downto 0) is pxlOutRgb(7 downto 0);
alias pxlOutBlue : std_logic_vector(7 downto 0) is pxlOutRgb(15 downto 8);
alias pxlOutGreen : std_logic_vector(7 downto 0) is pxlOutRgb(23 downto 16);

begin
  
  process( pxlInRed, pxlInGreen, pxlInBlue, pxlInRgb, gridAct, brightGrid ) is
  variable tmpPxlInRed, tmpPxlInGreen, tmpPxlInBlue, 
    tmpGridChange, tmpResRed, tmpResGreen, tmpResBlue : signed( 8 downto 0 );
  begin
    
    if ( gridAct = '0' ) then
      pxlOutRgb <= pxlInRgb;
    else
      tmpPxlInRed := signed( '0' & pxlInRed );
      tmpPxlInGreen := signed( '0' & pxlInGreen );
      tmpPxlInBlue := signed( '0' & pxlInBlue );
      
      tmpGridChange := signed( '0' & gridLineChange );
      if ( brightGrid = '1' ) then
        tmpResRed := tmpPxlInRed + tmpGridChange;
        tmpResGreen := tmpPxlInGreen + tmpGridChange;
        tmpResBlue := tmpPxlInBlue + tmpGridChange;
        
        if ( tmpResRed <= 0 ) then
          tmpResRed := ( others => '1' );
        end if;
        
        if ( tmpResGreen <= 0 ) then
          tmpResGreen := ( others => '1' );
        end if;
        
        if ( tmpResBlue <= 0 ) then
          tmpResBlue := ( others => '1' );
        end if;
      else
        tmpResRed := tmpPxlInRed - tmpGridChange;
        tmpResGreen := tmpPxlInGreen - tmpGridChange;
        tmpResBlue := tmpPxlInBlue - tmpGridChange;
        
        if ( tmpResRed < 0 ) then
          tmpResRed := ( others => '0' );
        end if;
        
        if ( tmpResGreen < 0 ) then
          tmpResGreen := ( others => '0' );
        end if;
        
        if ( tmpResBlue < 0 ) then
          tmpResBlue := ( others => '0' );
        end if;
        
      end if;
      
      pxlOutRed <= std_logic_vector( tmpResRed( 7 downto 0 ) );
      pxlOutGreen <= std_logic_vector( tmpResGreen( 7 downto 0 ) );
      pxlOutBlue <= std_logic_vector( tmpResBlue( 7 downto 0 ) );
     
    end if;
    
  end process;

end rtl;
