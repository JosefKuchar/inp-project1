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
  -- FSM
  type states is (
    S_RESET,
    S_FETCH,
    S_DECODE,
    S_INC,
    S_DEC,
    S_SKIP, S_PREV,
    S_MOVE_RIGHT,
    S_MOVE_LEFT,
    S_PRINT, S_PRINT_WAIT,
    S_INPUT_REQ, S_INPUT_WAIT, S_INPUT,
    S_LCHECK, S_RCHECK, S_LL, S_LR, S_RL, S_RR, S_RINIT,
    S_HALT
  );
  signal current_state : states := S_RESET;
  signal next_state : states := S_RESET;

  -- Program counter
  signal pc : std_logic_vector(11 downto 0);
  signal pc_inc : std_logic;
  signal pc_dec : std_logic;

  -- Data pointer
  signal ptr : std_logic_vector(11 downto 0);
  signal ptr_inc : std_logic;
  signal ptr_dec : std_logic;

  -- Memory address multiplexor
  signal mux_addr : std_logic;

  -- Data write multiplexor
  signal mux_wdata : std_logic_vector(1 downto 0);

  -- Bracket/brace counter - [] ()
  signal cnt : signed(11 downto 0);
  signal cnt_inc : std_logic;
  signal cnt_dec : std_logic;
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
  p_state_decision : process (current_state, EN, DATA_RDATA, OUT_BUSY, IN_VLD, cnt)
  begin
    next_state <= current_state;
    if EN = '1' then
      -- All states fall to fetch by default
      next_state <= S_FETCH;
      case current_state is
        when S_FETCH =>
          next_state <= S_DECODE;
        when S_DECODE =>
          -- We are in left bracket/paren loop - ( [
          if cnt > 0 then
            case DATA_RDATA is
              when x"5B" => -- '['
                next_state <= S_LL;
              when x"5D" => -- ']'
                next_state <= S_LR;
              when x"28" => -- '('
                next_state <= S_LL;
              when x"29" => -- ')'
                next_state <= S_LR;
              when others => -- Other characters
                next_state <= S_SKIP;
            end case;
            -- We are in right bracket/parent loop - ) ]
          elsif cnt < 0 then
            case DATA_RDATA is
              when x"5B" => -- '['
                next_state <= S_RL;
              when x"5D" => -- ']'
                next_state <= S_RR;
              when x"28" => -- '('
                next_state <= S_RL;
              when x"29" => -- ')'
                next_state <= S_RR;
              when others => -- Other characters
                next_state <= S_PREV;
            end case;
            -- Normal decoding
          else
            case DATA_RDATA is
              when x"2B" => -- '+'
                next_state <= S_INC;
              when x"2D" => -- '-'
                next_state <= S_DEC;
              when x"3E" => -- '>'
                next_state <= S_MOVE_RIGHT;
              when x"3C" => -- '<'
                next_state <= S_MOVE_LEFT;
              when x"2E" => -- '.'
                next_state <= S_PRINT_WAIT;
              when x"2C" => -- ','
                next_state <= S_INPUT_REQ;
              when x"5B" => -- '['
                next_state <= S_LCHECK;
              when x"5D" => -- ']'
                next_state <= S_RCHECK;
              when x"28" => -- '('
                next_state <= S_SKIP;
              when x"29" => -- ')'
                next_state <= S_RCHECK;
              when x"00" => -- '\0'
                next_state <= S_HALT;
              when others => -- Other characters (comments)
                next_state <= S_SKIP;
            end case;
          end if;
        when S_PRINT_WAIT =>
          if OUT_BUSY = '0' then
            next_state <= S_PRINT;
          end if;
        when S_INPUT_REQ =>
          next_state <= S_INPUT_WAIT;
        when S_INPUT_WAIT =>
          if IN_VLD = '1' then
            next_state <= S_INPUT;
          end if;
        when S_LCHECK =>
          if DATA_RDATA = x"00" then
            next_state <= S_LL;
          else
            next_state <= S_SKIP;
          end if;
        when S_RCHECK =>
          if DATA_RDATA = x"00" then
            next_state <= S_SKIP;
          else
            next_state <= S_RINIT;
          end if;
        when S_RL =>
          if cnt =- 1 then -- -1 because we will add 1 in next cycle
            next_state <= S_SKIP;
          else
            next_state <= S_PREV;
          end if;
        when S_RR =>
          if cnt = 1 then -- 1 because wwe will remove 1 in next cycle
            next_state <= S_SKIP;
          else
            next_state <= S_PREV;
          end if;
        when S_HALT =>
          next_state <= S_HALT;
        when others => null;
      end case;
    end if;
  end process;

  -- FSM output
  p_output : process (current_state)
  begin
    case current_state is
        -- Initialize
      when S_RESET =>
        pc_inc <= '0';
        pc_dec <= '0';
        ptr_inc <= '0';
        ptr_dec <= '0';
        cnt_inc <= '0';
        cnt_dec <= '0';
        DATA_EN <= '0';
        IN_REQ <= '0';
        OUT_WE <= '0';
        MUX_ADDR <= '0';
        -- Fetch instruction from memory
      when S_FETCH =>
        MUX_ADDR <= '0';
        DATA_RDWR <= '0';
        DATA_EN <= '1';
        OUT_WE <= '0';
        pc_inc <= '0';
        pc_dec <= '0';
        ptr_inc <= '0';
        ptr_dec <= '0';
        cnt_inc <= '0';
        cnt_dec <= '0';
        -- Decode current instruction
      when S_DECODE =>
        MUX_ADDR <= '1';
        -- Increment instruction '+'
      when S_INC =>
        DATA_RDWR <= '1';
        DATA_EN <= '1';
        MUX_WDATA <= "10";
        pc_inc <= '1';
        -- Decrement instrucition '-'
      when S_DEC =>
        DATA_RDWR <= '1';
        DATA_EN <= '1';
        MUX_WDATA <= "01";
        pc_inc <= '1';
        -- Move right instruction '>'
      when S_MOVE_RIGHT =>
        ptr_inc <= '1';
        pc_inc <= '1';
        -- Move left instruction '<'
      when S_MOVE_LEFT =>
        ptr_dec <= '1';
        pc_inc <= '1';
        -- Print instruction '.'
      when S_PRINT =>
        OUT_DATA <= DATA_RDATA;
        OUT_WE <= '1';
        pc_inc <= '1';
        -- Request input from keyboard
      when S_INPUT_REQ =>
        IN_REQ <= '1';
        -- Input instruction ','
      when S_INPUT =>
        IN_REQ <= '0';
        DATA_RDWR <= '1';
        DATA_EN <= '1';
        MUX_WDATA <= "00";
        pc_inc <= '1';
        -- Left bracket/brace inside left bracket/brace loop
      when S_LL =>
        cnt_inc <= '1';
        pc_inc <= '1';
        -- Right bracket/brace inside left bracket/brace loop
      when S_LR =>
        cnt_dec <= '1';
        pc_inc <= '1';
        -- Left bracket/brace inside right bracket/brace loop
      when S_RL =>
        cnt_inc <= '1';
        -- Right bracket/brace inside left bracket/brace loop
      when S_RR =>
        cnt_dec <= '1';
        -- Initialize counter when going back in loop
      when S_RINIT =>
        pc_dec <= '1';
        cnt_dec <= '1';
        -- Other characters (comments)
      when S_SKIP =>
        cnt_inc <= '0';
        cnt_dec <= '0';
        pc_inc <= '1';
        -- Go back in loop
      when S_PREV =>
        cnt_inc <= '0';
        cnt_dec <= '0';
        pc_dec <= '1';
      when others => null;
    end case;
  end process;

  -- Program counter
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

  -- Data pointer
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

  -- Memory address multiplexor
  -- 0 = Program counter
  -- 1 = Data pointer
  p_mux_addr : process (CLK, RESET)
  begin
    if mux_addr = '0' then
      DATA_ADDR <= "0" & pc;
    else
      DATA_ADDR <= "1" & ptr;
    end if;
  end process;

  -- Data write multiplexor
  -- 00 = Data from keyboard
  -- 01 = Current cell value - 1
  -- 10 = Current cell value + 1
  p_mux_wdata : process (CLK, RESET)
  begin
    case mux_wdata is
      when "00" =>
        DATA_WDATA <= IN_DATA;
      when "01" =>
        DATA_WDATA <= DATA_RDATA - 1;
      when "10" =>
        DATA_WDATA <= DATA_RDATA + 1;
      when others =>
        DATA_WDATA <= x"00";
    end case;
  end process;

  -- Bracket counter []
  p_cnt : process (CLK, RESET)
  begin
    if RESET = '1' then
      cnt <= x"000";
    elsif rising_edge(CLK) then
      if cnt_inc = '1' then
        cnt <= cnt + 1;
      elsif cnt_dec = '1' then
        cnt <= cnt - 1;
      end if;
    end if;
  end process;
end behavioral;
