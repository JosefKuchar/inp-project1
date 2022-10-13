-- cpu.vhd: Simple 8-bit CPU (BrainFuck interpreter)
-- Copyright (C) 2022 Brno University of Technology,
--                    Faculty of Information Technology
-- Author(s): Josef Kuchar <xkucha28@stud.fit.vutbr.cz>
--
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

-- ----------------------------------------------------------------------------
--                        Entity declaration
-- ----------------------------------------------------------------------------
entity cpu is
  port (
    CLK : in std_logic; -- hodinovy signal
    RESET : in std_logic; -- asynchronni reset procesoru
    EN : in std_logic; -- povoleni cinnosti procesoru

    -- synchronni pamet RAM
    DATA_ADDR : out std_logic_vector(12 downto 0); -- adresa do pameti
    DATA_WDATA : out std_logic_vector(7 downto 0); -- mem[DATA_ADDR] <- DATA_WDATA pokud DATA_EN='1'
    DATA_RDATA : in std_logic_vector(7 downto 0); -- DATA_RDATA <- ram[DATA_ADDR] pokud DATA_EN='1'
    DATA_RDWR : out std_logic; -- cteni (0) / zapis (1)
    DATA_EN : out std_logic; -- povoleni cinnosti

    -- vstupni port
    IN_DATA : in std_logic_vector(7 downto 0); -- IN_DATA <- stav klavesnice pokud IN_VLD='1' a IN_REQ='1'
    IN_VLD : in std_logic; -- data platna
    IN_REQ : out std_logic; -- pozadavek na vstup data

    -- vystupni port
    OUT_DATA : out std_logic_vector(7 downto 0); -- zapisovana data
    OUT_BUSY : in std_logic; -- LCD je zaneprazdnen (1), nelze zapisovat
    OUT_WE : out std_logic -- LCD <- OUT_DATA pokud OUT_WE='1' a OUT_BUSY='0'
  );
end cpu;
-- ----------------------------------------------------------------------------
--                      Architecture declaration
-- ----------------------------------------------------------------------------
architecture behavioral of cpu is
  type states is (S_RESET, S_READ, S_PROCESS);
  signal current_state : states := S_RESET;
  signal next_state : states := S_RESET;

  -- ptr
  signal ptr : std_logic_vector(11 downto 0);
  signal ptr_inc : std_logic;
  signal ptr_dec : std_logic;
  -- pc
  signal pc : std_logic_vector(11 downto 0);
  signal pc_inc : std_logic;
  signal pc_dec : std_logic;

  signal mux_addr : std_logic;

  signal mux_wdata : std_logic_vector(1 downto 0);
begin

  -- State switching logic
  p_state_switch : process (CLK, RESET)
  begin
    if RESET = '1' then
      current_state <= S_RESET;
    elsif rising_edge(CLK) then
      current_state <= next_state;
    end if;
  end process;

  -- Get next state
  p_state_decision : process (current_state)
  begin
    next_state <= current_state;
    case current_state is
      when S_RESET =>
        next_state <= S_PROCESS;
      when others => null;
    end case;
  end process;

  -- Output
  p_output : process (current_state)
  begin
    case current_state is
      when S_RESET =>
        DATA_EN <= '0';
        IN_REQ <= '0';
        OUT_WE <= '0';
      when S_PROCESS =>
        DATA_RDWR <= '1';
        mux_wdata <= "10";
        DATA_EN <= '1';
      when others => null;
    end case;
  end process;

  p_pc : process (CLK, RESET)
  begin
    if RESET = '1' then
      pc <= x"000";
    elsif rising_edge(CLK) then
      if pc_inc = '1' then
        pc <= pc + 1;
      elsif pc_dec = '1' then
        pc <= pc - 1;
      end if;
    end if;
  end process;

  p_ptr : process (CLK, RESET)
  begin
    if RESET = '1' then
      ptr <= x"000";
    elsif rising_edge(CLK) then
      if ptr_inc = '1' then
        ptr <= ptr + 1;
      elsif ptr_dec = '1' then
        ptr <= ptr - 1;
      end if;
    end if;
  end process;

  p_mux_addr : process (CLK, RESET)
  begin
    if RESET = '1' then
    elsif rising_edge(CLK) then
      if mux_addr = '0' then
        DATA_ADDR <= "0" & pc;
      else
        DATA_ADDR <= "1" & ptr;
      end if;
    end if;
  end process;

  p_mux_wdata : process (CLK, RESET)
  begin
    if RESET = '1' then
    elsif rising_edge(CLK) then
      case mux_wdata is
        when "00" =>
          DATA_WDATA <= IN_DATA;
        when "01" =>
          DATA_WDATA <= DATA_RDATA - 1;
        when "10" =>
          DATA_WDATA <= DATA_RDATA + 1;
        when others => null;
      end case;
    end if;
  end process;

end behavioral;
