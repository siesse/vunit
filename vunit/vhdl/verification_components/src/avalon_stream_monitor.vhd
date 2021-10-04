-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,
-- You can obtain one at http://mozilla.org/MPL/2.0/.
--
-- Copyright (c) 2014-2021, Johannes Vanoverschelde johannes.vanoverschelde82@gmail.com

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

context work.vunit_context;
context work.com_context;
use work.avalon_stream_pkg.all;

entity avalon_stream_monitor is
    generic(
        monitor : avalon_stream_monitor_t);
    port(
        clk   : in  std_logic;
        ready : in  std_logic;
        valid : in  std_logic;
        sop   : in  std_logic;
        eop   : in  std_logic;
        data  : in  std_logic_vector(data_length(monitor) - 1 downto 0);
        empty : in  std_logic_vector(empty_length(monitor) - 1 downto 0)
    );
end entity;

architecture a of avalon_stream_monitor is
begin
    main : process
        variable msg                       : msg_t;
        variable avalon_stream_transaction : avalon_stream_transaction_t(
            data(data'range)
        );
        variable b_sop, b_eop : boolean;
        variable n_empty : natural;
    begin
        wait until (valid and ready) = '1' and rising_edge(clk);

        if is_visible(monitor.p_logger, debug) then
            debug(monitor.p_logger, "data: " & to_nibble_string(data) & ", sop: " & to_string(sop) & ", eop: " & to_string(eop) & " (" & to_integer_string(empty) & ")");
        end if;
        n_empty := to_integer(unsigned(empty));
        if sop='1' then
            b_sop:=true;
        else
            b_sop:=false;
        end if;
        if eop='1' then
            b_eop:=true;
        else
            b_eop:=false;
        end if;
        avalon_stream_transaction := (
                sop   => b_sop,
                eop   => b_eop,
                data  => data,
                empty => n_empty
        );

        msg := new_avalon_stream_transaction_msg(avalon_stream_transaction);
        publish(net, monitor.p_actor, msg);
    end process;

    avalon_stream_protocol_checker_generate : if monitor.p_protocol_checker /= null_avalon_stream_protocol_checker generate
        avalon_stream_protocol_checker_inst : entity work.avalon_stream_protocol_checker
            generic map(
                protocol_checker => monitor.p_protocol_checker)
            port map(
                clk   => clk,
                ready => ready,
                valid => valid,
                sop   => sop,
                eop   => eop,
                data  => data,
                empty => empty
            );
    end generate avalon_stream_protocol_checker_generate;

end architecture;
