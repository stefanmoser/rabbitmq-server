%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2020 VMware, Inc. or its affiliates.  All rights reserved.
%%

-module(rabbit_prelaunch_logging).

-include_lib("kernel/include/logger.hrl").
-include_lib("rabbitmq_prelaunch/include/logging.hrl").

-export([setup/1]).

setup(Context) ->
    ?LOG_DEBUG(""),
    ?LOG_DEBUG("== Logging =="),
    ok = set_ERL_CRASH_DUMP_envvar(Context),
    ok = configure_logger(Context),
    ok.

set_ERL_CRASH_DUMP_envvar(#{log_base_dir := LogBaseDir}) ->
    case os:getenv("ERL_CRASH_DUMP") of
        false ->
            ErlCrashDump = filename:join(LogBaseDir, "erl_crash.dump"),
            ?LOG_DEBUG(
              "Setting $ERL_CRASH_DUMP environment variable to \"~ts\"",
              [ErlCrashDump]),
            os:putenv("ERL_CRASH_DUMP", ErlCrashDump),
            ok;
        ErlCrashDump ->
            ?LOG_DEBUG(
              "$ERL_CRASH_DUMP environment variable already set to \"~ts\"",
              [ErlCrashDump]),
            ok
    end.

configure_logger(#{main_log_file := MainLog,
                   upgrade_log_file := UpgradeLog}) ->
    {MainHandler,
     UpgradeHandler} = case MainLog of
                           "-" ->
                               %% Log to STDOUT.
                               ?LOG_DEBUG("Logging to stdout"),
                               {tty,
                                tty};
                           _ ->
                               ?LOG_DEBUG("Logging to:"),
                               [?LOG_DEBUG("  - ~ts", [Log])
                                || Log <- [MainLog, UpgradeLog]],
                               %% Log to file.
                               {MainLog,
                                UpgradeLog}
                       end,
    ok = configure_main_log(MainHandler),
    ok = configure_upgrade_log(UpgradeHandler),
    ok.

configure_main_log(tty) ->
    %% Already the default.
    ok;
configure_main_log(Filename) ->
    Config0 = main_handler_config(),
    Config = Config0#{config => #{file => Filename}},
    ok = logger:add_handler(main_log_file, logger_std_h, Config),

    Filter = {fun logger_filters:domain/2,
              {stop, sub, ?LOGGER_DOMAIN_UPGRADE}},
    ok = logger:add_handler_filter(main_log_file, no_upgrade, Filter),
    ok = logger:remove_handler(default).

configure_upgrade_log(tty) ->
    %% Already the default.
    ok;
configure_upgrade_log(Filename) ->
    Config0 = upgrade_handler_config(),
    Config = Config0#{config => #{file => Filename}},
    ok = logger:add_handler(upgrade_log_file, logger_std_h, Config),

    Filter = {fun logger_filters:domain/2,
              {log, sub, ?LOGGER_DOMAIN_UPGRADE}},
    ok = logger:add_handler_filter(upgrade_log_file, no_upgrade, Filter).

main_handler_config() ->
    rabbit_prelaunch_early_logging:main_handler_config().

upgrade_handler_config() ->
    Config = rabbit_prelaunch_early_logging:main_handler_config(),
    Config#{filter_default => stop}.
