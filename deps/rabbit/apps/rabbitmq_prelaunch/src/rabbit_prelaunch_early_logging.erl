-module(rabbit_prelaunch_early_logging).

-include_lib("kernel/include/logger.hrl").

-include("include/logging.hrl").

-export([setup_early_logging/2,
         main_handler_config/0,
         enable_quick_dbg/1,
         use_colored_logging/0,
         use_colored_logging/1]).

setup_early_logging(#{log_levels := undefined} = Context,
                    LagerEventToStdout) ->
    setup_early_logging(Context#{log_levels => get_default_log_level()},
                        LagerEventToStdout);
setup_early_logging(Context, LagerEventToStdout) ->
    Configured = is_primary_filter_defined(),
    case Configured of
        true  -> ok;
        false -> do_setup_early_logging(Context, LagerEventToStdout)
    end.

get_default_log_level() ->
    #{"prelaunch" => notice}.

do_setup_early_logging(#{log_levels := LogLevels},
                       _LagerEventToStdout) ->
    add_primary_filter(LogLevels),
    ok = logger:update_handler_config(default, main_handler_config()).

is_primary_filter_defined() ->
    #{filters := Filters} = logger:get_primary_config(),
    lists:keymember(rabbitmq_levels_and_categories, 1, Filters).

add_primary_filter(LogLevels) ->
    ok = logger:add_primary_filter(
           rabbitmq_levels_and_categories,
           {primary_logger_filter(LogLevels), #{}}),
    ok = logger:set_primary_config(level, all).

primary_logger_filter(LogLevels) ->
    fun(#{level := Level, meta := Meta} = LogEvent, _) ->
            GlobalMinLevel = maps:get(global, LogLevels, notice),
            case Meta of
                #{domain := [?LOGGER_SUPER_DOMAIN_NAME, Domain | _]} ->
                    MinLevel = maps:get(atom_to_list(Domain),
                                        LogLevels,
                                        GlobalMinLevel),
                    case logger:compare_levels(Level, MinLevel) of
                        lt -> stop;
                        _  -> LogEvent
                    end;
                _ ->
                    case logger:compare_levels(Level, GlobalMinLevel) of
                        lt -> stop;
                        _  -> LogEvent
                    end
            end
    end.

main_handler_config() ->
    #{filter_default => log,
      formatter => {logger_formatter, #{legacy_header => false,
                                        single_line => true}}
     }.

use_colored_logging() ->
    use_colored_logging(rabbit_prelaunch:get_context()).

use_colored_logging(#{log_levels := #{color := true},
                      output_supports_colors := true}) ->
    true;
use_colored_logging(_) ->
    false.

enable_quick_dbg(#{dbg_output := Output, dbg_mods := Mods}) ->
    case Output of
        stdout -> {ok, _} = dbg:tracer(),
                  ok;
        _      -> {ok, _} = dbg:tracer(port, dbg:trace_port(file, Output)),
                  ok
    end,
    {ok, _} = dbg:p(all, c),
    lists:foreach(fun(M) -> {ok, _} = dbg:tp(M, cx) end, Mods).
