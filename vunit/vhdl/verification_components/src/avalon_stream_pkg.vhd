-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,
-- You can obtain one at http://mozilla.org/MPL/2.0/.
--
-- Copyright (c) 2014-2021, Lars Asplund lars.anders.asplund@gmail.com

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.logger_pkg.all;
use work.stream_master_pkg.all;
use work.stream_slave_pkg.all;
context work.com_context;
context work.data_types_context;

package avalon_stream_pkg is

    type avalon_stream_component_type_t is (null_component, default_component, custom_component);

    type avalon_stream_protocol_checker_t is record
        p_type                 : avalon_stream_component_type_t;
        p_actor                : actor_t;
        p_data_length          : natural;
        p_data_bits_per_symbol : natural;
        p_logger               : logger_t;
        p_max_waits            : natural;
    end record;

    constant null_avalon_stream_protocol_checker : avalon_stream_protocol_checker_t := (
        p_type                 => null_component,
        p_actor                => null_actor,
        p_data_length          => 0,
        p_data_bits_per_symbol => 0,
        p_logger               => null_logger,
        p_max_waits            => 0
    );

    -- The default protocol checker is used to specify that the checker
    -- configuration is defined by the parent component into which the checker is
    -- instantiated.
    constant default_avalon_stream_protocol_checker : avalon_stream_protocol_checker_t := (
        p_type                 => default_component,
        p_actor                => null_actor,
        p_data_length          => 0,
        p_data_bits_per_symbol => 0,
        p_logger               => null_logger,
        p_max_waits            => 0
    );

    type avalon_stream_monitor_t is record
        p_type                 : avalon_stream_component_type_t;
        p_actor                : actor_t;
        p_data_length          : natural;
        p_data_bits_per_symbol : natural;
        p_logger               : logger_t;
        p_protocol_checker     : avalon_stream_protocol_checker_t;
    end record;

    constant null_avalon_stream_monitor : avalon_stream_monitor_t := (
        p_type                 => null_component,
        p_actor                => null_actor,
        p_data_length          => 0,
        p_data_bits_per_symbol => 0,
        p_logger               => null_logger,
        p_protocol_checker     => null_avalon_stream_protocol_checker
    );

    -- The default monitor is used to specify that the monitor
    -- configuration is defined by the parent component into which the monitor is
    -- instantiated.
    constant default_avalon_stream_monitor : avalon_stream_monitor_t := (
        p_type                 => default_component,
        p_actor                => null_actor,
        p_data_length          => 0,
        p_data_bits_per_symbol => 0,
        p_logger               => null_logger,
        p_protocol_checker     => null_avalon_stream_protocol_checker
    );

    type avalon_source_t is record
        valid_high_probability : real range 0.0 to 1.0;
        p_actor                : actor_t;
        p_data_length          : natural;
        p_data_bits_per_symbol : natural;
        p_logger               : logger_t;
    end record;

    type avalon_sink_t is record
        ready_high_probability : real range 0.0 to 1.0;
        -- Private
        p_actor                : actor_t;
        p_data_length          : natural;
        p_data_bits_per_symbol : natural;
        p_logger               : logger_t;
    end record;

    constant avalon_stream_logger : logger_t := get_logger("vunit_lib:avalon_stream_pkg");
    impure function new_avalon_source(data_length            : natural;
                                      valid_high_probability : real     := 1.0;
                                      logger                 : logger_t := avalon_stream_logger;
                                      actor                  : actor_t  := null_actor;
                                      data_bits_per_symbol   : natural  := 0 --if 0 single symbol is assumed 
                                     ) return avalon_source_t;
    impure function new_avalon_sink(data_length            : natural;
                                    ready_high_probability : real     := 1.0;
                                    logger                 : logger_t := avalon_stream_logger;
                                    actor                  : actor_t  := null_actor;
                                    data_bits_per_symbol   : natural  := 0 --if 0 single symbol is assumed
                                   ) return avalon_sink_t;
    impure function data_length(monitor : avalon_stream_monitor_t) return natural;
    impure function data_length(protocol_checker : avalon_stream_protocol_checker_t) return natural;
    impure function data_length(source : avalon_source_t) return natural;
    impure function data_length(sink : avalon_sink_t) return natural;
    impure function empty_length(monitor : avalon_stream_monitor_t) return natural;
    impure function empty_length(protocol_checker : avalon_stream_protocol_checker_t) return natural;
    impure function empty_length(source : avalon_source_t) return natural;
    impure function empty_length(sink : avalon_sink_t) return natural;
    impure function as_stream(source : avalon_source_t) return stream_master_t;
    impure function as_stream(sink : avalon_sink_t) return stream_slave_t;

    constant push_avalon_stream_msg        : msg_type_t := new_msg_type("push avalon stream");
    constant pop_avalon_stream_msg         : msg_type_t := new_msg_type("pop avalon stream");
    constant avalon_stream_transaction_msg : msg_type_t := new_msg_type("avalon stream transaction");

    procedure push_avalon_stream(signal net    : inout network_t;
                                 avalon_source : avalon_source_t;
                                 data          : std_logic_vector;
                                 sop           : std_logic        := '0';
                                 eop           : std_logic        := '0';
                                 empty         : natural:=0);

    procedure pop_avalon_stream(signal   net   : inout network_t;
                                avalon_sink    : avalon_sink_t;
                                variable data  : inout std_logic_vector;
                                variable sop   : inout std_logic;
                                variable eop   : inout std_logic);
                                
    procedure pop_avalon_stream(signal   net   : inout network_t;
                                avalon_sink    : avalon_sink_t;
                                variable data  : inout std_logic_vector;
                                variable sop   : inout std_logic;
                                variable eop   : inout std_logic;
                                variable empty : inout natural);

    type avalon_stream_transaction_t is record
        data  : std_logic_vector;
        sop   : boolean;
        eop   : boolean;
        empty : natural;
    end record;

    procedure push_avalon_stream_transaction(msg : msg_t; avalon_stream_transaction : avalon_stream_transaction_t);
    procedure pop_avalon_stream_transaction(
        constant msg                       : in msg_t;
        variable avalon_stream_transaction : out avalon_stream_transaction_t
    );

    impure function new_avalon_stream_transaction_msg(
        avalon_stream_transaction : avalon_stream_transaction_t
    ) return msg_t;

    procedure handle_avalon_stream_transaction(
        variable msg_type           : inout msg_type_t;
        variable msg                : inout msg_t;
        variable avalon_transaction : out avalon_stream_transaction_t
    );
end package;

package body avalon_stream_pkg is

    function ilog2(value : natural) return natural is
        variable res : natural := 0;
        variable pow : natural := 1;
    begin
        while pow < value loop
            pow := pow * 2;
            res := res + 1;
        end loop;
        return res;
    end function;

    impure function new_avalon_source(data_length            : natural;
                                      valid_high_probability : real     := 1.0;
                                      logger                 : logger_t := avalon_stream_logger;
                                      actor                  : actor_t  := null_actor;
                                      data_bits_per_symbol   : natural  := 0
                                     ) return avalon_source_t is
        variable p_actor : actor_t;
    begin
        p_actor := actor when actor /= null_actor else new_actor;

        return (valid_high_probability => valid_high_probability,
                p_actor                => p_actor,
                p_data_length          => data_length,
                p_data_bits_per_symbol => data_bits_per_symbol,
                p_logger               => logger);
    end;

    impure function new_avalon_sink(data_length            : natural;
                                    ready_high_probability : real     := 1.0;
                                    logger                 : logger_t := avalon_stream_logger;
                                    actor                  : actor_t  := null_actor;
                                    data_bits_per_symbol   : natural  := 0
                                   ) return avalon_sink_t is
        variable p_actor : actor_t;
    begin
        p_actor := actor when actor /= null_actor else new_actor;

        return (ready_high_probability => ready_high_probability,
                p_actor                => p_actor,
                p_data_length          => data_length,
                p_data_bits_per_symbol => data_bits_per_symbol,
                p_logger               => logger);
    end;

    impure function data_length(monitor : avalon_stream_monitor_t) return natural is
    begin
        return monitor.p_data_length;
    end;
    
    impure function data_length(protocol_checker : avalon_stream_protocol_checker_t) return natural is
    begin
        return protocol_checker.p_data_length;
    end;
    
    impure function data_length(source : avalon_source_t) return natural is
    begin
        return source.p_data_length;
    end;

    impure function empty_length(source : avalon_source_t) return natural is
    begin
        if source.p_data_bits_per_symbol = 0 then
            return 1;
        else
            return ilog2(source.p_data_length / source.p_data_bits_per_symbol);
        end if;
    end;

    impure function data_length(sink : avalon_sink_t) return natural is
    begin
        return sink.p_data_length;
    end;

    impure function empty_length(monitor : avalon_stream_monitor_t) return natural is
    begin
        if monitor.p_data_bits_per_symbol = 0 then
            return 1;
        else
            return ilog2(monitor.p_data_length / monitor.p_data_bits_per_symbol);
        end if;
    end;
    
    impure function empty_length(protocol_checker : avalon_stream_protocol_checker_t) return natural is
    begin
        if protocol_checker.p_data_bits_per_symbol = 0 then
            return 1;
        else
            return ilog2(protocol_checker.p_data_length / protocol_checker.p_data_bits_per_symbol);
        end if;
    end;
    
    
    impure function empty_length(sink : avalon_sink_t) return natural is
    begin
        if sink.p_data_bits_per_symbol = 0 then
            return 1;
        else
            return ilog2(sink.p_data_length / sink.p_data_bits_per_symbol);
        end if;
    end;

    impure function as_stream(source : avalon_source_t) return stream_master_t is
    begin
        return (p_actor => source.p_actor);
    end;

    impure function as_stream(sink : avalon_sink_t) return stream_slave_t is
    begin
        return (p_actor => sink.p_actor);
    end;

    procedure push_avalon_stream(signal net    : inout network_t;
                                 avalon_source : avalon_source_t;
                                 data          : std_logic_vector;
                                 sop           : std_logic := '0';
                                 eop           : std_logic := '0';
                                 empty         : natural:=0) is
        variable msg                       : msg_t := new_msg(push_avalon_stream_msg);
        variable avalon_stream_transaction : avalon_stream_transaction_t(data(data'length - 1 downto 0));
    begin
        avalon_stream_transaction.data  := data;
        if sop = '1' then
            avalon_stream_transaction.sop := true;
        else
            avalon_stream_transaction.sop := false;
        end if;
        if eop = '1' then
            avalon_stream_transaction.eop := true;
        else
            avalon_stream_transaction.eop := false;
        end if;
        avalon_stream_transaction.empty := empty;
        push_avalon_stream_transaction(msg, avalon_stream_transaction);
        send(net, avalon_source.p_actor, msg);
    end;

    procedure pop_avalon_stream(signal   net   : inout network_t;
                                avalon_sink    : avalon_sink_t;
                                variable data  : inout std_logic_vector;
                                variable sop   : inout std_logic;
                                variable eop   : inout std_logic) is
        variable empty :natural;
    begin
        pop_avalon_stream(net,avalon_sink,data,sop,eop,empty);
    end;
    procedure pop_avalon_stream(signal   net   : inout network_t;
                                avalon_sink    : avalon_sink_t;
                                variable data  : inout std_logic_vector;
                                variable sop   : inout std_logic;
                                variable eop   : inout std_logic;
                                variable empty : inout natural) is
        variable reference                 : msg_t := new_msg(pop_avalon_stream_msg);
        variable reply_msg                 : msg_t;
        variable avalon_stream_transaction : avalon_stream_transaction_t(data(data'length - 1 downto 0));

    begin
        send(net, avalon_sink.p_actor, reference);
        receive_reply(net, reference, reply_msg);
        pop_avalon_stream_transaction(reply_msg, avalon_stream_transaction);
        data := avalon_stream_transaction.data;
        if avalon_stream_transaction.sop then
            sop := '1';
        else
            sop := '0';
        end if;

        if avalon_stream_transaction.eop then
            eop := '1';
        else
            eop := '0';
        end if;
        if avalon_sink.p_data_bits_per_symbol > 0 then
            empty := avalon_stream_transaction.empty;
        else
            empty := 0;
        end if;

        delete(reference);
        delete(reply_msg);
    end;

    procedure push_avalon_stream_transaction(msg : msg_t; avalon_stream_transaction : avalon_stream_transaction_t) is
    begin
        push_std_ulogic_vector(msg, avalon_stream_transaction.data);
        push_boolean(msg, avalon_stream_transaction.sop);
        push_boolean(msg, avalon_stream_transaction.eop);
        push_integer(msg, avalon_stream_transaction.empty);
    end;

    procedure pop_avalon_stream_transaction(
        constant msg                       : in msg_t;
        variable avalon_stream_transaction : out avalon_stream_transaction_t) is
    begin
        avalon_stream_transaction.data  := pop_std_ulogic_vector(msg);
        avalon_stream_transaction.sop   := pop_boolean(msg);
        avalon_stream_transaction.eop   := pop_boolean(msg);
        avalon_stream_transaction.empty := pop_integer(msg);
    end;

    impure function new_avalon_stream_transaction_msg(
        avalon_stream_transaction : avalon_stream_transaction_t
    ) return msg_t is
        variable msg : msg_t;
    begin
        msg := new_msg(avalon_stream_transaction_msg);
        push_avalon_stream_transaction(msg, avalon_stream_transaction);

        return msg;
    end;

    procedure handle_avalon_stream_transaction(
        variable msg_type           : inout msg_type_t;
        variable msg                : inout msg_t;
        variable avalon_transaction : out avalon_stream_transaction_t) is
    begin
        if msg_type = avalon_stream_transaction_msg then
            handle_message(msg_type);

            pop_avalon_stream_transaction(msg, avalon_transaction);
        end if;
    end;

end package body;
