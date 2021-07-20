-----------------------------------------------------------------------
-- Title: Top Unit 4x
-- Author: zwenergy
-----------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_bit.all;
Library UNISIM;
use UNISIM.vcomponents.all;

entity topUnit4x is
  port(
    clk : in std_logic; -- 100 MHz
    hdmiTxHPD : in std_logic;
    
    redPxl : in std_logic_vector( 4 downto 0 );
    greenPxl : in std_logic_vector( 4 downto 0 );
    bluePxl : in std_logic_vector( 4 downto 0 );
    vsync : in std_logic;
    dclk : in std_logic;
    
    controllerMCUIn : in std_logic;
    
    audioLIn : in std_logic;
    audioRIn : in std_logic;
    
    gbaclk : out std_logic;
    
    hdmiTxCEC : inout std_logic;
    hdmiTxRSCL : inout std_logic;
    hdmiTxRSDA : inout std_logic;
    
    hdmiTxRedP : out std_logic;
    hdmiTxRedN : out std_logic;
    hdmiTxBlueP : out std_logic;
    hdmiTxBlueN : out std_logic;
    hdmiTxGreenP : out std_logic;
    hdmiTxGreenN : out std_logic;
    hdmiTxClkP : out std_logic;
    hdmiTxClkN : out std_logic  
  );
end topUnit4x;

architecture rtl of topUnit4x is

signal cx : std_logic_vector( 10 downto 0 );
signal cy : std_logic_vector( 9 downto 0 );
constant AUDIO_BIT_WIDTH : integer := 16;
constant AUDIO_RATE : integer := 48000;
constant WAVE_RATE : integer := 480;
signal tmds_clock : std_logic;

signal skipTo : std_logic := '0';
signal skipToY : std_logic_vector(9 downto 0);


signal audio_sample_word : std_logic_vector(AUDIO_BIT_WIDTH - 1 downto 0);
signal audio_sample_word_dampened : std_logic_vector(AUDIO_BIT_WIDTH - 1 downto 0); -- This is to avoid giving you a heart attack -- it'll be really loud if it uses the full dynamic range.
signal clock_audio : std_logic;
signal audio_counter : unsigned(10 downto 0) := "00000000000";

-- Pixelclk
signal pxlClk : std_logic;
signal pxlClk5x : std_logic;
signal clkLock : std_logic;
signal clkFB : std_logic;

signal redPxlCap, greenPxlCap, bluePxlCap : std_logic_vector( 7 downto 0 );
--signal rgbPxlCap : std_logic_vector( 23 downto 0 );
signal validPxlCap : std_logic;
signal pxlCntCap : std_logic_vector( 7 downto 0 );
signal lineValidCap : std_logic;
signal newFrameGBA :std_logic;

signal prevLineCurPxlRedBuf, prevLineCurPxlGreenBuf, prevLineCurPxlBlueBuf,
  curLineCurPxlRedBuf, curLineCurPxlGreenBuf, curLineCurPxlBlueBuf,
  nextLineCurPxlRedBuf, nextLineCurPxlGreenBuf, nextLineCurPxlBlueBuf: std_logic_vector( 7 downto 0 );
--signal prevLineCurPxlRgbBuf, curLineCurPxlRgbBuf, nextLineCurPxlRgbBuf: std_logic_vector( 23 downto 0 );
signal pxlCntReadToCache, pxlCntReadToBuffer : std_logic_vector( 7 downto 0 );
signal nextLineRead : std_logic;
signal writeReadSameLine : std_logic;
signal newFrameBuff : std_logic;

--signal redEnc, greenEnc, blueEnc : std_logic_vector( 9 downto 0 );
signal rgbEnc, rgbTest : std_logic_vector( 23 downto 0 );

-- Serializer.
signal redSHIFT1 : std_logic;
signal redSHIFT2 : std_logic;
signal greenSHIFT1 : std_logic;
signal greenSHIFT2 : std_logic;
signal blueSHIFT1 : std_logic;
signal blueSHIFT2 : std_logic;

signal gbaclk_int : std_logic;
signal gbaclk2x : std_logic;

-- Pixel grid config.
signal doPxlGrid, bgrid, pxlGridToggle, smooth2x, smooth4x : std_logic;

-- Line cache.
--signal curLineCurPxlRed, curLineCurPxlBlue, curLineCurPxlGreen,
--  curLinePrevPxlRed, curLinePrevPxlBlue, curLinePrevPxlGreen,
--  curLineNextPxlRed, curLineNextPxlBlue, curLineNextPxlGreen,
--  prevLineCurPxlRed, prevLineCurPxlBlue, prevLineCurPxlGreen,
--  prevLinePrevPxlRed, prevLinePrevPxlBlue, prevLinePrevPxlGreen,
--  prevLineNextPxlRed, prevLineNextPxlBlue, prevLineNextPxlGreen,
--  nextLineCurPxlRed, nextLineCurPxlBlue, nextLineCurPxlGreen,
--  nextLinePrevPxlRed, nextLinePrevPxlBlue, nextLinePrevPxlGreen,
--  nextLineNextPxlRed, nextLineNextPxlBlue, nextLineNextPxlGreen : std_logic_vector( 7 downto 0 );
signal curLineCurPxlRgb, curLinePrevPxlRgb, curLineNextPxlRgb,
  prevLineCurPxlRgb, prevLinePrevPxlRgb, prevLineNextPxlRgb,
  nextLineCurPxlRgb, nextLinePrevPxlRgb, nextLineNextPxlRgb : std_logic_vector( 23 downto 0 );
  
alias curLineCurPxlRed : std_logic_vector(7 downto 0) is curLineCurPxlRgb(7 downto 0);
alias curLineCurPxlGreen : std_logic_vector(7 downto 0) is curLineCurPxlRgb(15 downto 8);
alias curLineCurPxlBlue : std_logic_vector(7 downto 0) is curLineCurPxlRgb(23 downto 16);

alias curLinePrevPxlRed : std_logic_vector(7 downto 0) is curLinePrevPxlRgb(7 downto 0);
alias curLinePrevPxlGreen : std_logic_vector(7 downto 0) is curLinePrevPxlRgb(15 downto 8);
alias curLinePrevPxlBlue : std_logic_vector(7 downto 0) is curLinePrevPxlRgb(23 downto 16);

alias curLineNextPxlRed : std_logic_vector(7 downto 0) is curLineNextPxlRgb(7 downto 0);
alias curLineNextPxlGreen : std_logic_vector(7 downto 0) is curLineNextPxlRgb(15 downto 8);
alias curLineNextPxlBlue : std_logic_vector(7 downto 0) is curLineNextPxlRgb(23 downto 16);

alias prevLineCurPxlRed : std_logic_vector(7 downto 0) is prevLineCurPxlRgb(7 downto 0);
alias prevLineCurPxlGreen : std_logic_vector(7 downto 0) is prevLineCurPxlRgb(15 downto 8);
alias prevLineCurPxlBlue : std_logic_vector(7 downto 0) is prevLineCurPxlRgb(23 downto 16);

alias prevLinePrevPxlRed : std_logic_vector(7 downto 0) is prevLinePrevPxlRgb(7 downto 0);
alias prevLinePrevPxlGreen : std_logic_vector(7 downto 0) is prevLinePrevPxlRgb(15 downto 8);
alias prevLinePrevPxlBlue : std_logic_vector(7 downto 0) is prevLinePrevPxlRgb(23 downto 16);

alias prevLineNextPxlRed : std_logic_vector(7 downto 0) is prevLineNextPxlRgb(7 downto 0);
alias prevLineNextPxlGreen : std_logic_vector(7 downto 0) is prevLineNextPxlRgb(15 downto 8);
alias prevLineNextPxlBlue : std_logic_vector(7 downto 0) is prevLineNextPxlRgb(23 downto 16);

alias nextLineCurPxlRed : std_logic_vector(7 downto 0) is nextLineCurPxlRgb(7 downto 0);
alias nextLineCurPxlGreen : std_logic_vector(7 downto 0) is nextLineCurPxlRgb(15 downto 8);
alias nextLineCurPxlBlue : std_logic_vector(7 downto 0) is nextLineCurPxlRgb(23 downto 16);

alias nextLinePrevPxlRed : std_logic_vector(7 downto 0) is nextLinePrevPxlRgb(7 downto 0);
alias nextLinePrevPxlGreen : std_logic_vector(7 downto 0) is nextLinePrevPxlRgb(15 downto 8);
alias nextLinePrevPxlBlue : std_logic_vector(7 downto 0) is nextLinePrevPxlRgb(23 downto 16);

alias nextLineNextPxlRed : std_logic_vector(7 downto 0) is nextLineNextPxlRgb(7 downto 0);
alias nextLineNextPxlGreen : std_logic_vector(7 downto 0) is nextLineNextPxlRgb(15 downto 8);
alias nextLineNextPxlBlue : std_logic_vector(7 downto 0) is nextLineNextPxlRgb(23 downto 16);

-- Main reset.
signal rst : std_logic := '1';

-- Out.
signal redSer, greenSer, blueSer : std_logic;

signal tmds : std_logic_vector(2 downto 0); -- Aka rgbSer

  
begin

  rst <= not clkLock;
  gbaclk <= gbaclk_int;

  audio_sample_word_dampened <= "000000" & audio_sample_word((AUDIO_BIT_WIDTH-1) downto (AUDIO_BIT_WIDTH-10));
    
  clock_audio <= '1' when (pxlClk = '1' and audio_counter = 10#1546#) else '0';
  
  audioConfig : process( pxlClk ) is
    begin
      if rising_edge( pxlClk ) then
        if (audio_counter = to_unsigned(1546, audio_counter'length)) then
          audio_counter <= "00000000000";
        else
          audio_counter <= audio_counter + 1;
        end if;
      end if;
    end process;
      
  hdmiTxCEC <= 'Z';
  hdmiTxRSCL <= 'Z';
  hdmiTxRSDA <= 'Z';
  
  -- Generate Clks.
  mmcm_inst : MMCME2_BASE
    generic map(
      CLKOUT0_DIVIDE_F => 1.0, --1.0
      CLKOUT1_DIVIDE => 10, --10
      CLKOUT2_DIVIDE => 2, --2
      CLKOUT3_DIVIDE => 89, --100
      CLKFBOUT_MULT_F => 8.375, --8.375
      DIVCLK_DIVIDE => 1, --1
      CLKIN1_PERIOD => 11.242 -- 10.0
    )
    port map(
      CLKIN1 => clk,
      RST => '0',
      CLKFBIN => clkFB,
      CLKOUT0 => open,
      CLKOUT1 => pxlClk,
      CLKOUT2 => pxlClk5x,
      CLKOUT3 => gbaClk2x,
      CLKFBOUT => clkFB,
      LOCKED => clkLock,
      PWRDWN => '0'
    );
    
--     Generate the actual GBA clk.
    process( gbaclk2x, rst ) is
        variable gbaClkCounter : integer := 0;
    begin
      if ( rst = '1' ) then
        gbaclk_int <= '1';

--      elsif ( gbaClkCounter = 20 ) then
--            gbaclk_int <= not gbaclk_int;
--            gbaClkCounter := 0;
--        else
--            gbaClkCounter := gbaClkCounter + 1;
        
      elsif rising_edge( gbaclk2x ) then
        gbaclk_int <= not gbaclk_int;
      end if;
    end process;
    
  -- The clock instance.
--  clk_inst : entity work.hdmi_pll_xilinx( rtl )
--    port map(
--      clk_out1 => pxlClk,
--      clk_out2 => pxlClk5x,
--      clk_out3 => gbaclk_int, -- 2x here
--      clk_lock => clkLock,
--      clk_in1 => clk
--    );
    
  -- The capture interface.
  cap_inst : entity work.captureGBA( rtl )
    generic map(
      clkPeriodNS => 12.0
    )
    port map(
      clk => pxlClk,
      rst => rst,
      redPxl => redPxl,
      bluePxl => bluePxl,
      greenPxl => greenPxl,
      vsync => vsync,
      dclk => dclk,
      redPxlOut => redPxlCap,
      greenPxlOut => greenPxlCap,
      bluePxlOut => bluePxlCap,
--      rgbPxlOut => rgbPxlCap,
      validPxlOut => validPxlCap,
      pxlCnt => pxlCntCap,
      validLine => lineValidCap,
      newFrame => newFrameGBA
    );
      
  -- Line buffer.
  buf_inst : entity work.lineBuffer( rtl ) 
    generic map(
      nrStgs => 16
    )
    port map(
      clkW => pxlClk,
      clkR => pxlClk,
      rst => rst,
      redIn => redPxlCap,
      greenIn => greenPxlCap,
      blueIn => bluePxlCap,
--      rgbIn => rgbPxlCap,
      wEn => validPxlCap,
      pxlCntWrite => pxlCntCap,
      pushLine => lineValidCap,
      newFrameIn => newFrameGBA,
      redOutPrev => prevLineCurPxlRedBuf,
      greenOutPrev => prevLineCurPxlGreenBuf,
      blueOutPrev => prevLineCurPxlBlueBuf,
      redOutCur => curLineCurPxlRedBuf,
      greenOutCur => curLineCurPxlGreenBuf,
      blueOutCur => curLineCurPxlBlueBuf,
      redOutNext => nextLineCurPxlRedBuf,
      greenOutNext => nextLineCurPxlGreenBuf,
      blueOutNext => nextLineCurPxlBlueBuf,
--      rgbOutPrev => prevLineCurPxlRgbBuf,
--      rgbOutCur => curLineCurPxlRgbBuf,
--      rgbOutNext => nextLineCurPxlRgbBuf,

      pxlCntRead => pxlCntReadToBuffer,
      pullLine => nextLineRead,
      sameLine => writeReadSameLine,
      newFrameOut => newFrameBuff
    );
    
  -- Line cache.
  lineCache_inst : entity work.lineCache( rtl )
  port map(
    clk => pxlClk,
    rst => rst,
    curPxlCnt => pxlCntReadToCache,
    lineChange => nextLineRead,
    curLineCurPxlRedIn => curLineCurPxlRedBuf,
    curLineCurPxlGreenIn => curLineCurPxlGreenBuf,
    curLineCurPxlBlueIn => curLineCurPxlBlueBuf,
    prevLineCurPxlRedIn => prevLineCurPxlRedBuf,
    prevLineCurPxlGreenIn => prevLineCurPxlGreenBuf,
    prevLineCurPxlBlueIn => prevLineCurPxlBlueBuf,
    nextLineCurPxlRedIn => nextLineCurPxlRedBuf,
    nextLineCurPxlGreenIn => nextLineCurPxlGreenBuf,
    nextLineCurPxlBlueIn => nextLineCurPxlBlueBuf,    
    
    curLineCurPxlRedOut => curLineCurPxlRed,
    curLineCurPxlGreenOut => curLineCurPxlGreen,
    curLineCurPxlBlueOut => curLineCurPxlBlue,
    prevLineCurPxlRedOut => prevLineCurPxlRed,
    prevLineCurPxlGreenOut => prevLineCurPxlGreen,
    prevLineCurPxlBlueOut => prevLineCurPxlBlue,
    nextLineCurPxlRedOut => nextLineCurPxlRed,
    nextLineCurPxlGreenOut => nextLineCurPxlGreen,
    nextLineCurPxlBlueOut => nextLineCurPxlBlue,
    
    curLineNextPxlRedOut => curLineNextPxlRed,
    curLineNextPxlGreenOut => curLineNextPxlGreen,
    curLineNextPxlBlueOut => curLineNextPxlBlue,
    prevLineNextPxlRedOut => prevLineNextPxlRed,
    prevLineNextPxlGreenOut => prevLineNextPxlGreen,
    prevLineNextPxlBlueOut => prevLineNextPxlBlue,
    nextLineNextPxlRedOut => nextLineNextPxlRed,
    nextLineNextPxlGreenOut => nextLineNextPxlGreen,
    nextLineNextPxlBlueOut => nextLineNextPxlBlue,
    
    curLinePrevPxlRedOut => curLinePrevPxlRed,
    curLinePrevPxlGreenOut => curLinePrevPxlGreen,
    curLinePrevPxlBlueOut => curLinePrevPxlBlue,
    prevLinePrevPxlRedOut => prevLinePrevPxlRed,
    prevLinePrevPxlGreenOut => prevLinePrevPxlGreen,
    prevLinePrevPxlBlueOut => prevLinePrevPxlBlue,
    nextLinePrevPxlRedOut => nextLinePrevPxlRed,
    nextLinePrevPxlGreenOut => nextLinePrevPxlGreen,
    nextLinePrevPxlBlueOut => nextLinePrevPxlBlue,
    
--    curLineCurPxlRgbIn => curLineCurPxlRgbBuf,

--    prevLineCurPxlRgbIn => prevLineCurPxlRgbBuf,
--    nextLineCurPxlRgbIn => nextLineCurPxlRgbBuf,    
    
--    curLineCurPxlRgbOut => curLineCurPxlRgb,
--    prevLineCurPxlRgbOut => prevLineCurPxlRgb,
--    nextLineCurPxlRgbOut => nextLineCurPxlRgb,
    
--    curLineNextPxlRgbOut => curLineNextPxlRgb,
--    prevLineNextPxlRgbOut => prevLineNextPxlRgb,
--    nextLineNextPxlRgbOut => nextLineNextPxlRgb,
    
--    curLinePrevPxlRgbOut => curLinePrevPxlRgb,
--    prevLinePrevPxlRgbOut => prevLinePrevPxlRgb,
--    nextLinePrevPxlRgbOut => nextLinePrevPxlRgb,
    
    pxlCntRead => pxlCntReadToBuffer
  );
  
  -- imageGen test
    
--    imageGenTest : process( pxlClk ) is
--    begin
--      if rising_edge( pxlClk ) then
--        if (cx(5) = '0' and cy(5) = '0') then
--          rgbTest <= ( others => '0' );
--        else
--          rgbTest <= ( others => '1' );
--        end if;
--      end if;
--    end process;
      
     

  -- Image gen.
  imgGen_inst : entity work.imageGen( rtl )
    port map(
      pxlClk => pxlClk,
      rst => rst,
      
      countX => cx,
      countY => cy,
      skipToY => skipToY,
      skipTo => skipTo,
      
      curLineCurPxlRgbIn => curLineCurPxlRgb, --curLineCurPxlRgb,
      prevLineCurPxlRgbIn => prevLineCurPxlRgb,
      nextLineCurPxlRgbIn => nextLineCurPxlRgb,
      
      curLineNextPxlRgbIn => curLineNextPxlRgb,
      prevLineNextPxlRgbIn => prevLineNextPxlRgb,
      nextLineNextPxlRgbIn => nextLineNextPxlRgb,
      
      curLinePrevPxlRgbIn => curLinePrevPxlRgb,
      prevLinePrevPxlRgbIn => prevLinePrevPxlRgb,
      nextLinePrevPxlRgbIn => nextLinePrevPxlRgb,
      
      sameLine => writeReadSameLine,
      newFrameIn => newFrameBuff,
      audioLIn => audioLIn,
      audioRIn => audioRIn,
      pxlGrid => doPxlGrid,
      brightGrid => bgrid,
      smooth2x => smooth2x,
      smooth4x => smooth4x,
      nextLine => nextLineRead,
      curPxl => pxlCntReadToCache,
      rgbDat => rgbEnc
    );
    
    -- Audio
    audio_inst : entity work.sawtooth (rtl)
      generic map(
        BIT_WIDTH => AUDIO_BIT_WIDTH,
        SAMPLE_RATE => AUDIO_RATE,
        WAVE_RATE => WAVE_RATE
      )
      port map(
        clk_audio => clock_audio,
        level => audio_sample_word
      );
      
      
    -- HDMI test
    
--    hdmiTest : process( pxlClk ) is
--    begin
--      if rising_edge( pxlClk ) then
--        if (cx(5) = '0' and cy(5) = '0') then
--          rgbTest <= ( others => '0' );
--        else
--          rgbTest <= ( others => '1' );
--        end if;
--      end if;
--    end process;

    
    -- HDMI
    hdmi_inst : entity work.hdmi ( rtl )
      generic map(
        VIDEO_ID_CODE => 4,
        VIDEO_REFRESH_RATE => 60.0,
        AUDIO_RATE => AUDIO_RATE,
        AUDIO_BIT_WIDTH => AUDIO_BIT_WIDTH
      )
      port map (
        clk_pixel_x5 => pxlClk5x,
        clk_pixel => pxlClk,
        clk_audio => clock_audio,
        rgb => rgbEnc,
        audio_sample_word => audio_sample_word_dampened,
        tmds => tmds,
        tmds_clock => tmds_clock,
        cx => cx,
        cy => cy,
        reset => rst,
        skipTo => skipTo,
        skipToLine => skipToY
      );

    
    -- Controller communication and pixel grid config.
    controllerComm_inst : entity work.controllerComm( rtl )
      port map(
        clk => pxlClk,
        rst => rst,
        datIn => controllerMCUIn,
        pxlGridToggle => pxlGridToggle
    );
      
    pxlGridConfig : process( pxlClk ) is
    begin
      if rising_edge( pxlClk ) then
        if ( rst = '1' ) then
          doPxlGrid <= '0';
          bgrid <= '0';
          smooth2x <= '0';
          smooth4x <= '0';
        else
          if ( pxlGridToggle = '1' ) then
            if ( smooth2x = '0' and smooth4x = '0' and doPxlGrid = '0' ) then
              smooth2x <= '1';
              smooth4x <= '0';
              doPxlGrid <= '0';
              bgrid <= '0';
            elsif ( smooth2x = '1' and smooth4x = '0' and doPxlGrid = '0' ) then
              smooth2x <= '0';
              smooth4x <= '1';
              doPxlGrid <= '0';
              bgrid <= '0';
            elsif ( smooth2x = '0' and smooth4x = '1' and doPxlGrid = '0' ) then
              smooth2x <= '0';
              smooth4x <= '0';
              doPxlGrid <= '1';
              bgrid <= '0';
            elsif ( bgrid = '0' ) then
              smooth2x <= '0';
              smooth4x <= '0';
              doPxlGrid <= '1';
              bgrid <= '1';
            else
              smooth2x <= '0';
              smooth4x <= '0';
              doPxlGrid <= '0';
              bgrid <= '0';
            end if;
          end if;
        end if;
      end if;
    end process;
      
      
    
    -- Serialize.
--    redSerM : OSERDESE2
--      generic map(
--        DATA_RATE_OQ => "DDR",
--        DATA_RATE_TQ => "SDR",
--        DATA_WIDTH => 10,
--        TRISTATE_WIDTH => 1,
--        SERDES_MODE => "MASTER"
--      )      
--      port map(
--        OQ => redSer,
--        OFB => open,
--        TQ => open,
--        TFB => open,
--        SHIFTOUT1 => open,
--        SHIFTOUT2 => open,
--        CLK => pxlClk5x,
--        CLKDIV => pxlClk,
--        D8 => redEnc( 7 ),
--        D7 => redEnc( 6 ),
--        D6 => redEnc( 5 ),
--        D5 => redEnc( 4 ),
--        D4 => redEnc( 3 ),
--        D3 => redEnc( 2 ),
--        D2 => redEnc( 1 ),
--        D1 => redEnc( 0 ),
--        TCE => '0',
--        OCE => '1',
--        TBYTEIN => '0',
--        TBYTEOUT => open,
--        RST => rst,
--        SHIFTIN1 => redSHIFT1,
--        SHIFTIN2 => redSHIFT2,
--        T1 => '0',
--        T2 => '0',
--        T3 => '0',
--        T4 => '0'
--      );
      
--    redSerS : OSERDESE2
--    generic map(
--      DATA_RATE_OQ => "DDR",
--      DATA_RATE_TQ => "SDR",
--      DATA_WIDTH => 10,
--      TRISTATE_WIDTH => 1,
--      SERDES_MODE => "SLAVE"
--    )      
--    port map(
--      OQ => open,
--      OFB => open,
--      TQ => open,
--      TFB => open,
--      SHIFTOUT1 => redSHIFT1,
--      SHIFTOUT2 => redSHIFT2,
--      CLK => pxlClk5x,
--      CLKDIV => pxlClk,
--      D8 => '0',
--      D7 => '0',
--      D6 => '0',
--      D5 => '0',
--      D4 => redEnc( 9 ),
--      D3 => redEnc( 8 ),
--      D2 => '0',
--      D1 => '0',
--      TCE => '0',
--      OCE => '1',
--      TBYTEIN => '0',
--      TBYTEOUT => open,
--      RST => rst,
--      SHIFTIN1 => '0',
--      SHIFTIN2 => '0',
--      T1 => '0',
--      T2 => '0',
--      T3 => '0',
--      T4 => '0'
--    );
    
--    greenSerM : OSERDESE2
--      generic map(
--        DATA_RATE_OQ => "DDR",
--        DATA_RATE_TQ => "SDR",
--        DATA_WIDTH => 10,
--        TRISTATE_WIDTH => 1,
--        SERDES_MODE => "MASTER"
--      )      
--      port map(
--        OQ => greenSer,
--        OFB => open,
--        TQ => open,
--        TFB => open,
--        SHIFTOUT1 => open,
--        SHIFTOUT2 => open,
--        CLK => pxlClk5x,
--        CLKDIV => pxlClk,
--        D8 => greenEnc( 7 ),
--        D7 => greenEnc( 6 ),
--        D6 => greenEnc( 5 ),
--        D5 => greenEnc( 4 ),
--        D4 => greenEnc( 3 ),
--        D3 => greenEnc( 2 ),
--        D2 => greenEnc( 1 ),
--        D1 => greenEnc( 0 ),
--        TCE => '0',
--        OCE => '1',
--        TBYTEIN => '0',
--        TBYTEOUT => open,
--        RST => rst,
--        SHIFTIN1 => greenSHIFT1,
--        SHIFTIN2 => greenSHIFT2,
--        T1 => '0',
--        T2 => '0',
--        T3 => '0',
--        T4 => '0'
--      );
      
--    greenSerS : OSERDESE2
--    generic map(
--      DATA_RATE_OQ => "DDR",
--      DATA_RATE_TQ => "SDR",
--      DATA_WIDTH => 10,
--      TRISTATE_WIDTH => 1,
--      SERDES_MODE => "SLAVE"
--    )      
--    port map(
--      OQ => open,
--      OFB => open,
--      TQ => open,
--      TFB => open,
--      SHIFTOUT1 => greenSHIFT1,
--      SHIFTOUT2 => greenSHIFT2,
--      CLK => pxlClk5x,
--      CLKDIV => pxlClk,
--      D8 => '0',
--      D7 => '0',
--      D6 => '0',
--      D5 => '0',
--      D4 => greenEnc( 9 ),
--      D3 => greenEnc( 8 ),
--      D2 => '0',
--      D1 => '0',
--      TCE => '0',
--      OCE => '1',
--      TBYTEIN => '0',
--      TBYTEOUT => open,
--      RST => rst,
--      SHIFTIN1 => '0',
--      SHIFTIN2 => '0',
--      T1 => '0',
--      T2 => '0',
--      T3 => '0',
--      T4 => '0'
--    );
    
    
--  blueSerM : OSERDESE2
--      generic map(
--        DATA_RATE_OQ => "DDR",
--        DATA_RATE_TQ => "SDR",
--        DATA_WIDTH => 10,
--        TRISTATE_WIDTH => 1,
--        SERDES_MODE => "MASTER"
--      )      
--      port map(
--        OQ => blueSer,
--        OFB => open,
--        TQ => open,
--        TFB => open,
--        SHIFTOUT1 => open,
--        SHIFTOUT2 => open,
--        CLK => pxlClk5x,
--        CLKDIV => pxlClk,
--        D8 => blueEnc( 7 ),
--        D7 => blueEnc( 6 ),
--        D6 => blueEnc( 5 ),
--        D5 => blueEnc( 4 ),
--        D4 => blueEnc( 3 ),
--        D3 => blueEnc( 2 ),
--        D2 => blueEnc( 1 ),
--        D1 => blueEnc( 0 ),
--        TCE => '0',
--        OCE => '1',
--        TBYTEIN => '0',
--        TBYTEOUT => open,
--        RST => rst,
--        SHIFTIN1 => blueSHIFT1,
--        SHIFTIN2 => blueSHIFT2,
--        T1 => '0',
--        T2 => '0',
--        T3 => '0',
--        T4 => '0'
--      );
      
--    blueSerS : OSERDESE2
--    generic map(
--      DATA_RATE_OQ => "DDR",
--      DATA_RATE_TQ => "SDR",
--      DATA_WIDTH => 10,
--      TRISTATE_WIDTH => 1,
--      SERDES_MODE => "SLAVE"
--    )      
--    port map(
--      OQ => open,
--      OFB => open,
--      TQ => open,
--      TFB => open,
--      SHIFTOUT1 => blueSHIFT1,
--      SHIFTOUT2 => blueSHIFT2,
--      CLK => pxlClk5x,
--      CLKDIV => pxlClk,
--      D8 => '0',
--      D7 => '0',
--      D6 => '0',
--      D5 => '0',
--      D4 => blueEnc( 9 ),
--      D3 => blueEnc( 8 ),
--      D2 => '0',
--      D1 => '0',
--      TCE => '0',
--      OCE => '1',
--      TBYTEIN => '0',
--      TBYTEOUT => open,
--      RST => rst,
--      SHIFTIN1 => '0',
--      SHIFTIN2 => '0',
--      T1 => '0',
--      T2 => '0',
--      T3 => '0',
--      T4 => '0'
--    );
    
  -- Out.
  rDiff:OBUFDS
    port map(
      I => tmds(2), --rSer
      O => hdmiTxRedP,
      OB => hdmiTxRedN
    );
    
  gDiff:OBUFDS
    port map(
      I => tmds(1), -- gSer
      O => hdmiTxGreenP,
      OB => hdmiTxGreenN
    );
    
  bDiff:OBUFDS
    port map(
      I => tmds(0), --bSer
      O => hdmiTxblueP,
      OB => hdmiTxBlueN
    );

    clkDiff:OBUFDS
    port map(
      I => tmds_clock,
      O => hdmiTxClkP,
      OB => hdmiTxClkN
    );
  
  
    
  end rtl;
