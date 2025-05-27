library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity rgb_driver is
    Port (
        clock: in std_logic;
        sw: in std_logic;
        BTNU : in std_logic;
        adc2: in std_logic_vector(1 downto 0);
        adc10: in std_logic_vector(1 downto 0);
        tx_o: out std_logic;
        led: out std_logic_vector(15 downto 0);
        LED16_B: out std_logic
    );
end rgb_driver;

architecture Behavioral of rgb_driver is

COMPONENT xadc_wiz_0
  PORT (
    di_in : IN STD_LOGIC_VECTOR(15 DOWNTO 0);
    daddr_in : IN STD_LOGIC_VECTOR(6 DOWNTO 0);
    den_in : IN STD_LOGIC;
    dwe_in : IN STD_LOGIC;
    drdy_out : OUT STD_LOGIC;
    do_out : OUT STD_LOGIC_VECTOR(15 DOWNTO 0);
    dclk_in : IN STD_LOGIC;
    reset_in : IN STD_LOGIC;
    vp_in : IN STD_LOGIC;
    vn_in : IN STD_LOGIC;
    vauxp2 : IN STD_LOGIC;
    vauxn2 : IN STD_LOGIC;
    vauxp10 : IN STD_LOGIC;
    vauxn10 : IN STD_LOGIC;
    user_temp_alarm_out : OUT STD_LOGIC;
    vccint_alarm_out : OUT STD_LOGIC;
    vccaux_alarm_out : OUT STD_LOGIC;
    ot_out : OUT STD_LOGIC;
    channel_out : OUT STD_LOGIC_VECTOR(4 DOWNTO 0);
    eoc_out : OUT STD_LOGIC;
    alarm_out : OUT STD_LOGIC;
    eos_out : OUT STD_LOGIC;
    busy_out : OUT STD_LOGIC 
  );
END COMPONENT;

COMPONENT fifo_generator_0
  PORT (
    clk : IN STD_LOGIC;
    rst : IN STD_LOGIC;
    din : IN STD_LOGIC_VECTOR(15 DOWNTO 0);
    wr_en : IN STD_LOGIC;
    rd_en : IN STD_LOGIC;
    dout : OUT STD_LOGIC_VECTOR(15 DOWNTO 0);
    full : OUT STD_LOGIC;
    empty : OUT STD_LOGIC 
  );
END COMPONENT;

COMPONENT debounce 
generic (
c_clkfreq	: integer := 100_000_000;
c_debtime	: integer := 1000;
c_initval	: std_logic	:= '0'
);
port (
clk			: in std_logic;
signal_i	: in std_logic;
signal_o	: out std_logic
);
end COMPONENT;

COMPONENT uart_tx 
generic (
c_clkfreq		: integer := 100_000_000;
c_baudrate		: integer := 115_200;
c_stopbit		: integer := 2
);
port (
clk				: in std_logic;
din_i			: in std_logic_vector (7 downto 0);
tx_start_i		: in std_logic;
tx_o			: out std_logic;
tx_done_tick_o	: out std_logic
);
end COMPONENT;



-------------------ADC signals---------------------
signal daddr_in: std_logic_vector(6 downto 0);
signal den_in : std_logic;
signal drdy_out : std_logic;
signal do_out: std_logic_Vector(15 downto 0);
signal user_temp_alarm_out: std_logic;
signal vccint_alarm_out: std_logic;
signal vccaux_alarm_out: std_logic;
signal ot_out: std_logic;
signal alarm_out: std_logic;
signal busy_out: std_logic;
------------------FiFo signals---------------------
signal reset_in : std_logic := '0';
signal wr_en: std_logic;
signal rd_en: std_logic;
signal fifo_full: std_logic;
signal fifodo_out: std_logic_vector(15 downto 0);
signal fifo_empty: std_logic;
------------------UART signals---------------------
signal MSBDo_out: std_logic_vector(7 downto 0);
signal tx_start_i: std_logic := '0';
signal tx_done_tick_o: std_logic;
-----------------DEB BUTTONS-------------------------
signal deb_BTNU: std_logic := '0'; -- start 
signal hold_deb_BTNU: std_logic := '0';
-----------------------------------------------------
signal contiune: std_logic := '0'; 
signal channel_process_start_signal: std_logic := '0'; 
signal system_start_i: std_logic := '0';
signal system_stop_i: std_logic := '0'; 
--------------------DADDRS_IN-------------------------------
TYPE Address_State_Type IS (READ_ADDRES2, READ_ADDRES10);
SIGNAL State : Address_State_Type := READ_ADDRES2;  
------------- switching between chanelles -------------------
type Do_out_bit is(do_out1_first, do_out1_last, do_out2_first, do_out2_last);
signal durum: Do_out_bit := do_out1_first; 
signal do_out1_first_done_tick: std_logic;
signal do_out1_last_done_tick: std_logic;
signal do_out2_first_done_tick: std_logic;
signal do_out2_last_done_tick: std_logic; 
----------------switch case----------------------------------
type switch is(channel2, channel10);
signal anahtar: switch := channel2;
-------------------LED CHANNELS------------------------------
signal channel2_led0: std_logic_vector(7 downto 0);
signal channel2_led1: std_logic_vector(7 downto 0);
signal channel10_led0: std_logic_vector(7 downto 0);
signal channel10_led1: std_logic_vector(7 downto 0);
--------------------------------------------------------------

begin
Xadc: xadc_wiz_0
  PORT MAP (
    di_in => (others => '0'),
    daddr_in => daddr_in,
    den_in => den_in,
    dwe_in => '0',
    drdy_out => drdy_out,
    do_out => do_out,
    dclk_in => clock,
    reset_in => reset_in,
    vp_in => '0',
    vn_in => '0',
    vauxp2 => adc2(0),
    vauxn2 => adc2(1),
    vauxp10 => adc10(0),
    vauxn10 => adc10(1), 
    user_temp_alarm_out => user_temp_alarm_out,
    vccint_alarm_out    => vccint_alarm_out,
    vccaux_alarm_out    => vccaux_alarm_out,
    ot_out => ot_out,
    channel_out => open,
    eoc_out => open,
    alarm_out => alarm_out,
    eos_out => open,
    busy_out => busy_out
 );   

Fifo: fifo_generator_0
    PORT MAP (
        clk => clock,
        rst => reset_in,
        din => do_out,
        wr_en => wr_en,
        rd_en => rd_en, 
        dout => fifodo_out,
        full => fifo_full,
        empty => fifo_empty
    ); 
Uart: uart_tx  
generic map(
c_clkfreq		=> 100_000_000,
c_baudrate		=> 115_200,
c_stopbit		=> 2
)   
port map(          
clk				=> clock,
din_i			=> MSBDo_out,
tx_start_i		=> tx_start_i,
tx_o			=> tx_o,
tx_done_tick_o	=> tx_done_tick_o
);
btnu_start_debounce: debounce 
generic map(
c_clkfreq   => 100_000_000,
c_debtime   => 1000,
c_initval   => '0'          
)
port map(  
clk			=>clock,
signal_i	=>BTNU,
signal_o	=>deb_BTNU
);

sw1:process(sw)
    begin
        if (sw = '0') then
            anahtar <= channel2;
        else
            anahtar <= channel10;
        end if;
end process;   

Led_process: process(clock)
begin
   if(rising_edge(clock)) then
        case anahtar is
                when channel2 => 
                    if(do_out1_first_done_tick = '1') then
                        led(15 downto 8) <= channel2_led0;
                    end if;
                    if(do_out1_last_done_tick = '1') then    
                        led(7 downto 0) <= channel2_led1; 
                    end if; 
                when channel10 =>    
                    if(do_out2_first_done_tick = '1') then 
                        led(15 downto 8) <= channel10_led0;
                    end if;    
                    if(do_out2_last_done_tick = '1') then
                        led(7 downto 0) <= channel10_led1;  
                    end if;
            end case;
    end if;       
end process;

button_process: process(deb_BTNU)  
begin 
    if(deb_BTNU = '1') then 
        hold_deb_BTNU <= not hold_deb_BTNU;
    end if; 
    if (hold_deb_BTNU = '1' and deb_BTNU = '0') then
    system_start_i <= '1'; 
    LED16_B <= '1'; 
    end if;
end process;

data_process: process(clock)
begin
    if(rising_edge(clock)) then            
            if(system_start_i = '1') then            
                rd_en <= '0';
                tx_start_i <= '0';
                do_out1_first_done_tick <= '0';
                do_out1_last_done_tick <= '0';
                do_out2_first_done_tick <= '0';
                do_out2_last_done_tick <= '0';
                durum <= do_out1_first; 
                  
                case durum is
                    when do_out1_first => 
                        MSBDo_out <= fifodo_out(15 downto 8); 
                        rd_en <= '1'; 
                        tx_start_i <= '1';
                        do_out1_first_done_tick <= '1';
                        channel2_led0 <= fifodo_out(15 downto 8);
                        durum <= do_out1_last;  
                             
                    when do_out1_last =>  
                        MSBDo_out <= fifodo_out(7 downto 4) & "0000";      
                        rd_en <= '1'; 
                        tx_start_i <= '1';
                        do_out1_last_done_tick <= '1';
                        channel2_led1 <= fifodo_out(7 downto 4) & "0000";
                        durum <= do_out2_first;  
                             
                    when do_out2_first =>   
                        MSBDo_out <= fifodo_out(15 downto 8);                        
                        rd_en <= '1'; 
                        tx_start_i <= '1';
                        do_out2_first_done_tick <= '1';
                        channel10_led0 <= fifodo_out(15 downto 8);
                        durum <= do_out2_last;  
                             
                    when do_out2_last => 
                        MSBDo_out <= fifodo_out(7 downto 4) & "0000";                          
                        rd_en <= '1'; 
                        tx_start_i <= '1';
                        do_out2_last_done_tick <= '1';
                        channel10_led1 <= fifodo_out(7 downto 4) & "0000";                       
                        durum <= do_out1_first;                           
                    when others =>
                        durum <= do_out1_first;  
                end case;
                end if;                  
    end if;  
end process;
 
channel_process: process(clock)
begin
    if(rising_edge(clock)) then         
        if(system_start_i = '1') then 
              case State is
                when READ_ADDRES2 =>
                    daddr_in <= "0010010";   
                    wr_en <= '1'; 
                    den_in <= '1';
                    drdy_out <= '1';      
                    if do_out1_last_done_tick = '1' then
                        State <= READ_ADDRES10;
                    end if;
                when READ_ADDRES10 =>
                    daddr_in <= "0011010";  
                    wr_en <= '1'; 
                    den_in <= '1';
                    drdy_out <= '1';        
                    if(do_out2_last_done_tick = '1') THEN
                        State <= READ_ADDRES2;
                    end if; 
                end case;
         end if;       
       end if; 
end process;
end Behavioral;


