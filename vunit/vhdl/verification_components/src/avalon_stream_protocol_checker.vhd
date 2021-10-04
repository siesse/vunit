-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,
-- You can obtain one at http://mozilla.org/MPL/2.0/.
--
-- Copyright (c) 2014-2021, Lars Asplund lars.anders.asplund@gmail.com

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use std.textio.all;

context work.vunit_context;
context work.com_context;
use work.avalon_stream_pkg.all;

entity avalon_stream_protocol_checker is
  generic (
    protocol_checker : avalon_stream_protocol_checker_t);
  port (
    clk   : in std_logic;
    ready : in std_logic := '0';
    valid : in std_logic;
    sop   : in std_logic;
    eop   : in std_logic;
    data  : in std_logic_vector(data_length(protocol_checker)-1 downto 0);
    empty : in std_logic_vector(empty_length(protocol_checker)-1 downto 0)
    );
end entity;

architecture a of avalon_stream_protocol_checker is
  constant rule1_checker : checker_t := new_checker(get_name(protocol_checker.p_logger) & ":rule 1");


  signal transaction : std_logic;
  signal transaction_eop : std_logic;
  signal empty_is_zero : std_logic;

begin
  transaction <= valid and ready;
  transaction_eop <= transaction and eop;
  empty_is_zero <='1' when unsigned(empty)=0 else '0';

  -- EMPTY WORDS ARE ONLY ACCEPTED AT EOP,
  check(rule1_checker, clk, transaction_eop, empty_is_zero , result("Not zero empty only at end of packet"));

end architecture;
