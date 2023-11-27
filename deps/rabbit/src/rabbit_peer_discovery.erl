%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2023 Broadcom. All Rights Reserved. The term “Broadcom” refers to Broadcom Inc. and/or its subsidiaries.  All rights reserved.
%%

-module(rabbit_peer_discovery).

-include_lib("kernel/include/logger.hrl").
-include_lib("stdlib/include/assert.hrl").

-include_lib("rabbit_common/include/logging.hrl").

%%
%% API
%%

-export([maybe_init/0,
         sync_desired_cluster/0,
         maybe_register/0,
         maybe_unregister/0,
         discover_cluster_nodes/0]).
-export([backend/0,
         node_type/0,
         normalize/1,
         append_node_prefix/1,
         node_prefix/0]).
-export([do_query_node_props/1]).

-ifdef(TEST).
-export([sort_nodes_and_props/1,
         join_selected_node/3]).
-endif.

-type backend() :: atom().
-type node_and_props() :: {node(), [node()], non_neg_integer()}.

-define(DEFAULT_BACKEND,   rabbit_peer_discovery_classic_config).

%% what node type is used by default for this node when joining
%% a new cluster as a virgin node
-define(DEFAULT_NODE_TYPE, disc).

%% default node prefix to attach to discovered hostnames
-define(DEFAULT_PREFIX, "rabbit").

%% default discovery retries and interval.
-define(DEFAULT_DISCOVERY_RETRY_COUNT, 30).
-define(DEFAULT_DISCOVERY_RETRY_INTERVAL_MS, 1000).

-define(NODENAME_PART_SEPARATOR, "@").

-define(PT_PEER_DISC_BACKEND, {?MODULE, backend}).

-compile({no_auto_import, [register/1, unregister/1]}).

-spec backend() -> backend().

backend() ->
    case application:get_env(rabbit, cluster_formation) of
        {ok, Proplist} ->
            Backend = proplists:get_value(
                        peer_discovery_backend, Proplist, ?DEFAULT_BACKEND),
            ?assert(is_atom(Backend)),
            Backend;
        undefined ->
            ?DEFAULT_BACKEND
    end.

-spec node_type() -> rabbit_types:node_type().

node_type() ->
    case application:get_env(rabbit, cluster_formation) of
        {ok, Proplist} ->
            proplists:get_value(node_type, Proplist, ?DEFAULT_NODE_TYPE);
        undefined ->
            ?DEFAULT_NODE_TYPE
    end.

-spec lock_acquisition_failure_mode() -> ignore | fail.

lock_acquisition_failure_mode() ->
    case application:get_env(rabbit, cluster_formation) of
        {ok, Proplist} ->
            proplists:get_value(lock_acquisition_failure_mode, Proplist, fail);
        undefined      ->
            fail
  end.

-spec maybe_init() -> ok.
%% @doc Initializes the peer discovery subsystem.

maybe_init() ->
    Backend = backend(),
    ?LOG_DEBUG(
       "Peer discovery: configured backend: ~tp",
       [Backend],
       #{domain => ?RMQLOG_DOMAIN_PEER_DISC}),

    %% We cache the configured backend as well. This is used by
    %% `sync_desired_cluster/0' and `maybe_unregister/0', to ensure that the
    %% same backend is used to create/sync the cluster and (un)register the
    %% node, even if the configuration changed in between.
    persistent_term:put(?PT_PEER_DISC_BACKEND, Backend),

    _ = code:ensure_loaded(Backend),
    case erlang:function_exported(Backend, init, 0) of
        true  ->
            ?LOG_DEBUG(
               "Peer discovery: backend supports initialisation",
               #{domain => ?RMQLOG_DOMAIN_PEER_DISC}),
            case Backend:init() of
                ok ->
                    ?LOG_DEBUG(
                       "Peer discovery: backend initialisation succeeded",
                       #{domain => ?RMQLOG_DOMAIN_PEER_DISC}),
                    ok;
                {error, _Reason} = Error ->
                    ?LOG_WARNING(
                       "Peer discovery: backend initialisation failed: ~tp.",
                       [Error],
                       #{domain => ?RMQLOG_DOMAIN_PEER_DISC}),
                    ok
            end;
        false ->
            ?LOG_DEBUG(
               "Peer discovery: backend does not support initialisation",
               #{domain => ?RMQLOG_DOMAIN_PEER_DISC}),
            ok
    end.

-spec sync_desired_cluster() -> ok.
%% @doc Creates or synchronizes the cluster membership of this node based on a
%% peer discovery backend.
%%
%% If the peer discovery backend finds nodes that this node should cluster
%% with, this function calls {@link rabbit_db_cluster:join/2} to join one of
%% these nodes.
%%
%% This function always returns `ok', regardless if this node joined a cluster
%% or it should boot as a standalone node.
%%
%% Currently, it only expands the cluster. It won't take care of kicking
%% members that are not listed by the backend.

sync_desired_cluster() ->
    Backend = persistent_term:get(?PT_PEER_DISC_BACKEND),

    %% We handle retries at the top level: steps are followed sequentially and
    %% if one of them fails, we retry the whole process.
    {Retries, RetryDelay} = discovery_retries(),

    sync_desired_cluster(Backend, Retries, RetryDelay).

-spec sync_desired_cluster(Backend, RetriesLeft, RetryDelay) -> ok when
      Backend :: backend(),
      RetriesLeft :: non_neg_integer(),
      RetryDelay :: non_neg_integer().
%% @private

sync_desired_cluster(Backend, RetriesLeft, RetryDelay) ->
    %% The peer discovery process follows the following steps:
    %%   1. It uses the configured backend to query the nodes that should form
    %%      a cluster. It takes care of checking the validity of the returned
    %%      values: the list of nodes is made of atoms and the node type is
    %%      valid.
    %%   2. It queries some properties for each node in the list. This is used
    %%      to filter out unreachable nodes and to sort the final list. The
    %%      sorting is important because it determines which node it will try
    %%      to join.
    %%   3. It joins the selected node using a regular `join_cluster'. This
    %%      step is protected by a lock if the backend supports this
    %%      mechanism.
    case discover_cluster_nodes(Backend) of
        {ok, {[ThisNode], _NodeType}} when ThisNode =:= node() ->
            ?LOG_DEBUG(
               "Peer discovery: no nodes to cluster with according to "
               "backend; proceeding as a standalone node",
               #{domain => ?RMQLOG_DOMAIN_PEER_DISC}),
            ok;
        {ok, {DiscoveredNodes, NodeType}} ->
            NodesAndProps = query_node_props(DiscoveredNodes),
            case can_use_discovered_nodes(DiscoveredNodes, NodesAndProps) of
                true ->
                    SelectedNode = select_node_to_join(NodesAndProps),
                    Ret = join_selected_node(Backend, SelectedNode, NodeType),
                    case Ret of
                        ok ->
                            %% TODO: Check if there are multiple "concurrent"
                            %% clusters in `NodesAndProps' instead of one, or
                            %% standalone ready nodes that joined no one.
                            %%
                            %% TODO: After some delay, re-evaluate peer
                            %% discovery, in case there are again multiple
                            %% clusters or standalone ready nodes.
                            %%
                            %% TODO: Remove members which are not in the list
                            %% returned by the backend.
                            ok;
                        {error, _Reason} ->
                            retry_sync_desired_cluster(
                              Backend, RetriesLeft, RetryDelay)
                    end;
                false ->
                    retry_sync_desired_cluster(
                      Backend, RetriesLeft, RetryDelay)
            end;
        {error, _Reason} ->
            retry_sync_desired_cluster(Backend, RetriesLeft, RetryDelay)
    end.

-spec retry_sync_desired_cluster(Backend, RetriesLeft, RetryDelay) -> ok when
      Backend :: backend(),
      RetriesLeft :: non_neg_integer(),
      RetryDelay :: non_neg_integer().
%% @private

retry_sync_desired_cluster(Backend, RetriesLeft, RetryDelay)
  when RetriesLeft > 0 ->
    RetriesLeft1 = RetriesLeft - 1,
    ?LOG_DEBUG(
       "Peer discovery: retrying to create/sync cluster in ~b ms "
       "(~b attempts left)",
       [RetryDelay, RetriesLeft1],
       #{domain => ?RMQLOG_DOMAIN_PEER_DISC}),
    timer:sleep(RetryDelay),
    sync_desired_cluster(Backend, RetriesLeft1, RetryDelay);
retry_sync_desired_cluster(_Backend, 0, _RetryDelay) ->
    ?LOG_ERROR(
       "Peer discovery: could not discover and join another node; "
       "proceeding as a standalone node",
       #{domain => ?RMQLOG_DOMAIN_PEER_DISC}),
    ok.

-spec discover_cluster_nodes() -> {ok, Discovery} when
      Discovery :: {DiscoveredNodes, NodeType},
      DiscoveredNodes :: [node()],
      NodeType :: rabbit_types:node_type().
%% @doc Queries the peer discovery backend to discover nodes.
%%
%% This is used by the CLI.

discover_cluster_nodes() ->
    Backend = persistent_term:get(?PT_PEER_DISC_BACKEND, backend()),
    discover_cluster_nodes(Backend).

-spec discover_cluster_nodes(Backend) -> Ret when
      Backend :: backend(),
      Discovery :: {DiscoveredNodes, NodeType},
      DiscoveredNodes :: [node()],
      NodeType :: rabbit_types:node_type(),
      Ret :: {ok, Discovery} | {error, Reason},
      Reason :: any().
%% @private

discover_cluster_nodes(Backend) ->
    %% The returned list of nodes and the node type are only sanity-checked by
    %% this function. In other words, the list contains atoms and the node
    %% type is valid. Nodes availability and inter-node compatibility are
    %% taken care of later.
    Ret = Backend:list_nodes(),
    ?LOG_DEBUG(
       "Peer discovery: backend returned the following configuration:~n"
       "  ~tp",
       [Ret],
       #{domain => ?RMQLOG_DOMAIN_PEER_DISC}),
    case normalize(Ret) of
        {ok, {DiscoveredNodes, NodeType} = Discovery} ->
            check_discovered_nodes_list_validity(DiscoveredNodes, NodeType),
            {ok, Discovery};
        {error, _} = Error ->
            ?LOG_ERROR(
               "Peer discovery: failed to query the list of nodes from the "
               "backend: ~0tp",
               [Error],
               #{domain => ?RMQLOG_DOMAIN_PEER_DISC}),
            Error
    end.

-spec check_discovered_nodes_list_validity(DiscoveredNodes, NodeType) ->
    Ret when
      DiscoveredNodes :: [node()],
      NodeType :: rabbit_types:node_type(),
      Ret :: ok.
%% @private

check_discovered_nodes_list_validity(DiscoveredNodes, NodeType)
  when is_list(DiscoveredNodes) andalso
       NodeType =:= disc orelse NodeType =:= disk orelse NodeType =:= ram ->
    BadNodenames = lists:filter(
                     fun(Nodename) -> not is_atom(Nodename) end,
                     DiscoveredNodes),
    case BadNodenames of
        [] -> ok;
        _  -> e({invalid_cluster_node_names, BadNodenames})
    end;
check_discovered_nodes_list_validity(DiscoveredNodes, BadNodeType)
  when is_list(DiscoveredNodes) ->
    e({invalid_cluster_node_type, BadNodeType}).

-spec query_node_props(Nodes) -> NodesAndProps when
      Nodes :: [node()],
      NodesAndProps :: [node_and_props()].
%% @doc Queries properties for each node in `Nodes' and sorts the list using
%% these properties.
%%
%% The following properties are queried:
%% <ul>
%% <li>the cluster membership of the node, i.e. the list of nodes it is
%% clustered with, including itself</li>
%% <li>the node's Erlang VM start time</li>
%% </ul>
%%
%% If a node can't be queried because it is unavailable, it is excluded from
%% the returned list.
%%
%% These properties are then used to sort the list of nodes according to the
%% following criterias:
%% <ol>
%% <li>Nodes are sorted by cluster size, from the bigger to the smaller.</li>
%% <li>For nodes with the same cluster size, nodes are sorted by start time,
%% from the oldest node to the youngest.</li>
%% <li>For nodes with the same cluster size and start time, nodes are sorted by
%% names, alphabetically.</li>
%%
%% The goal is that every nodes returned by the backend will select the same
%% node to join (the first in the list). The cluster size criteria is here to
%% make sure the node joins the cluster that is being expanded instead of
%% another standalone node. The start time is used because it's a better
%% criteria to sort nodes in a deterministic way than their name in case nodes
%% start in an arbitrary number and the first node in alphabetical order
%% becomes available later.
%%
%% An example of what we want to avoid is e.g. node 4 joining node 1, then node
%% 1 joining node 2. Indeed, now that peer discovery uses {@link
%% rabbit_db_cluster:join/2} instead of its own code path, there is the risk
%% that node 1 kicks node 4 out of the cluster by joining node 2 because
%% `join_cluster' includes a reset.
%%
%% @private

query_node_props(Nodes) when Nodes =/= [] ->
    {Prefix, _Suffix} = rabbit_nodes_common:parts(node()),
    PeerName = lists:flatten(
                 io_lib:format("~s-peerdisc-~s", [Prefix, os:getpid()])),
    %% We go through a temporary hidden node to query all other discovered
    %% peers properties, instead of querying them directly.
    %%
    %% The reason is that we don't want that Erlang automatically connect all
    %% nodes together as a side effect (to form the full mesh network by
    %% default). If we let Erlang do that, we may interfere with the Feature
    %% flags controller which is globally registered when it performs an
    %% operation. If all nodes become connected, it's possible two or more
    %% globally registered controllers end up connected before they are ready
    %% to be clustered, and thus in the same "namespace". `global' will kill
    %% all but one of them.
    %%
    %% By using a temporary intermediate hidden node, we ask Erlang not to
    %% connect everyone automatically.
    VMArgs0 = ["-hidden"],
    VMArgs1 = case rabbit_prelaunch:get_context() of
                  #{erlang_cookie := ErlangCookie,
                    var_origins := #{erlang_cookie := environment}} ->
                      ["-setcookie", atom_to_list(ErlangCookie) | VMArgs0];
                  _ ->
                      VMArgs0
              end,
    case peer:start(#{name => PeerName, args => VMArgs1}) of
        {ok, Pid, Peer} ->
            ?LOG_DEBUG(
               "Peer discovery: use temporary hidden node '~ts' to query "
               "discovered peers properties",
               [Peer],
               #{domain => ?RMQLOG_DOMAIN_PEER_DISC}),
            Ret = erpc:call(Peer, ?MODULE, do_query_node_props, [Nodes]),
            peer:stop(Pid),
            Ret;
        {error, _} = Error ->
            ?LOG_ERROR(
               "Peer discovery: failed to start temporary hidden node to "
               "query discovered peers' properties",
               #{domain => ?RMQLOG_DOMAIN_PEER_DISC}),
            throw(Error)
    end;
query_node_props([]) ->
    [].

do_query_node_props(Nodes) when Nodes =/= [] ->
    %% Make sure all log messages are forwarded from this temporary hidden
    %% node to the upstream node, regardless of their level.
    _ = logger:set_primary_config(level, debug),
    %% TODO: Replace with `rabbit_nodes:list_members/0' when the oldest
    %% supported version has it.
    MembersPerNode = erpc:multicall(Nodes, rabbit_nodes, all, []),
    query_node_props1(Nodes, MembersPerNode, []);
do_query_node_props([]) ->
    [].

query_node_props1(
  [Node | Nodes], [{ok, Members} | MembersPerNode], NodesAndProps) ->
    NodeAndProps = {Node, Members},
    NodesAndProps1 = [NodeAndProps | NodesAndProps],
    query_node_props1(Nodes, MembersPerNode, NodesAndProps1);
query_node_props1(
  [Node | Nodes], [{error, _} = Error | MembersPerNode], NodesAndProps) ->
    %% We consider that an error means the remote node is unreachable or not
    %% ready. Therefore, we exclude it from the list of discovered nodes as we
    %% won't be able to join it anyway.
    ?LOG_DEBUG(
       "Peer discovery: failed to query cluster members of node '~ts': ~0tp~n"
       "Peer discovery: node '~ts' excluded from the discovered nodes",
       [Node, Error, Node],
       #{domain => ?RMQLOG_DOMAIN_PEER_DISC}),
    query_node_props1(Nodes, MembersPerNode, NodesAndProps);
query_node_props1([], [], NodesAndProps) ->
    NodesAndProps1 = lists:reverse(NodesAndProps),
    query_node_props2(NodesAndProps1, []).

query_node_props2([{Node, Members} | Rest], NodesAndProps) ->
    try
        StartTime = get_node_start_time(Node, microsecond),
        NodeAndProps = {Node, Members, StartTime},
        NodesAndProps1 = [NodeAndProps | NodesAndProps],
        query_node_props2(Rest, NodesAndProps1)
    catch
        _:Error:_ ->
            %% If one of the erpc calls we use to get the start time fails,
            %% there is something wrong with the remote node because it
            %% doesn't depend on RabbitMQ. We exclude it from the discovered
            %% nodes.
            ?LOG_DEBUG(
               "Peer discovery: failed to query start time of node '~ts': "
               "~0tp~n"
               "Peer discovery: node '~ts' excluded from the discovered nodes",
               [Node, Error, Node],
               #{domain => ?RMQLOG_DOMAIN_PEER_DISC}),
            query_node_props2(Rest, NodesAndProps)
    end;
query_node_props2([], NodesAndProps) ->
    NodesAndProps1 = lists:reverse(NodesAndProps),
    sort_nodes_and_props(NodesAndProps1).

-spec get_node_start_time(Node, Unit) -> StartTime when
      Node :: node(),
      Unit :: erlang:time_unit(),
      StartTime :: non_neg_integer().
%% @doc Returns the start time of the given `Node' in `Unit'.
%%
%% The start time is an arbitrary point in time (in the past or the future),
%% expressed native time unit. It is a monotonic time that is specific to that
%% node. It can't be compared as is with other nodes' start time. To convert it
%% to a system time so that we can compare it, we must add the node's time
%% offset.
%%
%% Both the start time and the time offset are expressed in native time unit.
%% Again, this can't be compared to other nodes' native time unit values. We
%% must convert it to a common time unit first.
%%
%% See the documentation of {@link erlang:time_offset/0} at
%% https://www.erlang.org/doc/man/erlang#time_offset-0 to get the full
%% explanation of the computation.
%%
%% @private

get_node_start_time(Node, Unit) ->
    NativeStartTime = erpc:call(Node, erlang, system_info, [start_time]),
    TimeOffset = erpc:call(Node, erlang, time_offset, []),
    SystemStartTime = NativeStartTime + TimeOffset,
    StartTime = erpc:call(
                  Node, erlang, convert_time_unit,
                  [SystemStartTime, native, Unit]),
    StartTime.

-spec sort_nodes_and_props(NodesAndProps) -> SortedNodesAndProps when
      NodesAndProps :: [node_and_props()],
      SortedNodesAndProps :: [node_and_props()].
%% @doc Sorts the list of nodes according to their properties.
%%
%% See {@link query_node_props/1} for an explanation of the criterias used to
%% sort the list.
%%
%% @see query_node_props/1.
%%
%% @private

sort_nodes_and_props(NodesAndProps) ->
    NodesAndProps1 = lists:sort(
                       fun(
                         {NodeA, MembersA, StartTimeA},
                         {NodeB, MembersB, StartTimeB}) ->
                               length(MembersA) > length(MembersB) orelse
                               (length(MembersA) =:= length(MembersB) andalso
                                StartTimeA < StartTimeB) orelse
                               (length(MembersA) =:= length(MembersB) andalso
                                StartTimeA =:= StartTimeB andalso
                                NodeA =< NodeB)
                       end, NodesAndProps),
    ?LOG_DEBUG(
       lists:flatten(
         ["Peer discovery: sorted list of nodes and their properties "
          "considered to create/sync the cluster:"] ++
         ["~n  - ~0tp" || _ <- NodesAndProps1]),
       NodesAndProps1,
       #{domain => ?RMQLOG_DOMAIN_PEER_DISC}),
    NodesAndProps1.

-spec can_use_discovered_nodes(DiscoveredNodes, NodesAndProps) -> CanUse when
      DiscoveredNodes :: [node()],
      NodesAndProps :: [node_and_props()],
      CanUse :: boolean().
%% @doc Indicates if the list of discovered nodes is good enough to proceed
%% with peer discovery.
%%
%% It is possible that we queried the backend early enough that it doesn't yet
%% know about the nodes that should form a cluster. To reduce the chance of a
%% list of nodes which makes little sense, we checks two criterias:
%% <ul>
%% <li>We want that this node is part of the list.</li>
%% <li>If we have a cluster size hint and the expected size is greater than 1,
%% we want the list to have at least two nodes. The cluster size hint is
%% computed from the configured target cluster size hint and the length of the
%% nodes list returned by the backend. This function picks the maximum of the
%% two. This is useful for backends such as the classic config one where the
%% returned list is static (i.e. it can be used as the cluster size hint).</li>
%% </ul>
%%
%% @private

can_use_discovered_nodes(DiscoveredNodes, NodesAndProps)
  when NodesAndProps =/= [] ->
    Nodes = [Node || {Node, _Members, _StartTime} <- NodesAndProps],

    ThisNode = node(),
    ThisNodeIsIncluded = lists:member(ThisNode, Nodes),
    case ThisNodeIsIncluded of
        true ->
            ok;
        false ->
            ?LOG_DEBUG(
               "Peer discovery: not satisfyied with discovered peers: the "
               "list does not contain this node",
               #{domain => ?RMQLOG_DOMAIN_PEER_DISC})
    end,

    %% We consider that the list of nodes returned by the backend can be a
    %% cluster size hint too. That's why we pick the maximum between the
    %% configured one and the list length.
    ClusterSizeHint = erlang:max(
                        rabbit_nodes:target_cluster_size_hint(),
                        length(DiscoveredNodes)),
    HasEnoughNodes = ClusterSizeHint =< 1 orelse length(Nodes) >= 2,
    case HasEnoughNodes of
        true ->
            ok;
        false ->
            ?LOG_DEBUG(
               "Peer discovery: not satisfyied with discovered peers: the "
               "list should contain at least two nodes with a configured "
               "cluster size hint of ~b nodes",
               [ClusterSizeHint],
               #{domain => ?RMQLOG_DOMAIN_PEER_DISC})
    end,

    ThisNodeIsIncluded andalso HasEnoughNodes;
can_use_discovered_nodes(_DiscoveredNodes, []) ->
    ?LOG_DEBUG(
       "Peer discovery: discovered no peer nodes to cluster with. "
       "Some discovery backends can filter nodes out based on a "
       "readiness criteria. "
       "Enabling debug logging might help troubleshoot.",
       #{domain => ?RMQLOG_DOMAIN_PEER_DISC}),
    false.

-spec select_node_to_join(NodesAndProps) -> SelectedNode when
      NodesAndProps :: [node_and_props()],
      SelectedNode :: node().
%% @doc Selects the node to join among the sorted list of nodes.
%%
%% The selection is simple: we take the first entry. It corresponds to the
%% oldest node we could reach, clustered with the greatest number of nodes.
%%
%% @private

select_node_to_join([{Node, _Members, _StartTime} | _]) ->
    ?LOG_INFO(
       "Peer discovery: node '~ts' selected for auto-clustering",
       [Node],
       #{domain => ?RMQLOG_DOMAIN_PEER_DISC}),
    Node.

-spec join_selected_node(Backend, Node, NodeType) -> Ret when
      Backend :: backend(),
      Node :: node(),
      NodeType :: rabbit_types:node_type(),
      Ret :: ok | {error, Reason},
      Reason :: any().
%% @doc Joins the selected node.
%%
%% This function relies on {@link rabbit_db_cluster:join/2}. It acquires a lock
%% before proceeding with the join if the backend provides such a mechanism.
%%
%% If the selected node is this node, this is a no-op and no lock is acquired.
%%
%% @private

join_selected_node(_Backend, ThisNode, _NodeType) when ThisNode =:= node() ->
    ?LOG_DEBUG(
       "Peer discovery: the selected node is this node; proceed with boot",
       #{domain => ?RMQLOG_DOMAIN_PEER_DISC}),
    ok;
join_selected_node(Backend, SelectedNode, NodeType) ->
    ?LOG_DEBUG(
       "Peer discovery: trying to acquire lock",
       #{domain => ?RMQLOG_DOMAIN_PEER_DISC}),
    LockResult = lock(Backend, SelectedNode),
    ?LOG_DEBUG(
       "Peer discovery: rabbit_peer_discovery:lock/0 returned ~0tp",
       [LockResult],
       #{domain => ?RMQLOG_DOMAIN_PEER_DISC}),
    case LockResult of
        not_supported ->
            ?LOG_DEBUG(
               "Peer discovery: no lock acquired",
               #{domain => ?RMQLOG_DOMAIN_PEER_DISC}),
            join_selected_node_locked(SelectedNode, NodeType);
        {ok, Data} ->
            ?LOG_DEBUG(
               "Peer discovery: lock acquired",
               #{domain => ?RMQLOG_DOMAIN_PEER_DISC}),
            try
                join_selected_node_locked(SelectedNode, NodeType)
            after
                ?LOG_DEBUG(
                   "Peer discovery: lock released",
                   #{domain => ?RMQLOG_DOMAIN_PEER_DISC}),
                unlock(Backend, Data)
            end;
        {error, _Reason} = Error ->
            ?LOG_WARNING(
               "Peer discovery: failed to acquire a lock: ~0tp",
               [Error],
               #{domain => ?RMQLOG_DOMAIN_PEER_DISC}),
            case lock_acquisition_failure_mode() of
                ignore -> join_selected_node_locked(SelectedNode, NodeType);
                fail   -> Error
            end
    end.

-spec join_selected_node_locked(Node, NodeType) -> Ret when
      Node :: node(),
      NodeType :: rabbit_types:node_type(),
      Ret :: ok | {error, Reason},
      Reason :: any().

join_selected_node_locked(Node, NodeType) ->
    %% We used to synchronize feature flags here before we updated the cluster
    %% membership. We don't do it anymore because the `join_cluster' code
    %% resets the joining node and copies the feature flags states from the
    %% cluster.
    try
        Ret = rabbit_db_cluster:join(Node, NodeType),
        ?assertNotEqual({ok, already_member}, Ret),
        case Ret of
            ok ->
                ?LOG_INFO(
                   "Peer discovery: this node (~ts) successfully joined "
                   "node '~ts' cluster",
                   [node(), Node],
                   #{domain => ?RMQLOG_DOMAIN_PEER_DISC});
            Error1 ->
                ?LOG_WARNING(
                   "Peer discovery: could not auto-cluster with node '~ts': "
                   "~0tp",
                   [Node, Error1],
                   #{domain => ?RMQLOG_DOMAIN_PEER_DISC})
        end,
        Ret
    catch
        throw:Error2 ->
            ?LOG_WARNING(
               "Peer discovery: could not auto-cluster with node '~ts': ~0tp",
               [Node, Error2],
               #{domain => ?RMQLOG_DOMAIN_PEER_DISC}),
            Error2
    end.

-spec e(any()) -> no_return().

e(Tag) -> throw({error, {Tag, error_description(Tag)}}).

error_description({invalid_cluster_node_names, BadNames}) ->
    "In the 'cluster_nodes' configuration key, the following node names "
        "are invalid: " ++ lists:flatten(io_lib:format("~tp", [BadNames]));
error_description({invalid_cluster_node_type, BadType}) ->
    "In the 'cluster_nodes' configuration key, the node type is invalid "
        "(expected 'disc' or 'ram'): " ++
        lists:flatten(io_lib:format("~tp", [BadType])).

-spec maybe_register() -> ok.

maybe_register() ->
    Backend = persistent_term:get(?PT_PEER_DISC_BACKEND, backend()),
    case Backend:supports_registration() of
        true ->
            ?LOG_DEBUG(
               "Peer discovery: registering this node",
               #{domain => ?RMQLOG_DOMAIN_PEER_DISC}),
            register(Backend),
            _ = Backend:post_registration(),
            ok;
        false ->
            ?LOG_DEBUG(
               "Peer discovery: registration unsupported, skipping register",
               #{domain => ?RMQLOG_DOMAIN_PEER_DISC}),
            ok
    end.

-spec maybe_unregister() -> ok.

maybe_unregister() ->
    Backend = persistent_term:get(?PT_PEER_DISC_BACKEND),
    case Backend:supports_registration() of
        true ->
            ?LOG_DEBUG(
               "Peer discovery: unregistering this node",
               #{domain => ?RMQLOG_DOMAIN_PEER_DISC}),
            unregister(Backend);
        false ->
            ?LOG_DEBUG(
               "Peer discovery: registration unsupported, skipping unregister",
               #{domain => ?RMQLOG_DOMAIN_PEER_DISC}),
            ok
    end.

-spec discovery_retries() -> {Retries, RetryDelay} when
      Retries :: non_neg_integer(),
      RetryDelay :: non_neg_integer().

discovery_retries() ->
    case application:get_env(rabbit, cluster_formation) of
        {ok, Proplist} ->
            Retries  = proplists:get_value(discovery_retry_limit,    Proplist, ?DEFAULT_DISCOVERY_RETRY_COUNT),
            Interval = proplists:get_value(discovery_retry_interval, Proplist, ?DEFAULT_DISCOVERY_RETRY_INTERVAL_MS),
            {Retries, Interval};
        undefined ->
            {?DEFAULT_DISCOVERY_RETRY_COUNT, ?DEFAULT_DISCOVERY_RETRY_INTERVAL_MS}
    end.

-spec register(Backend) -> ok when
      Backend :: backend().

register(Backend) ->
    ?LOG_INFO(
       "Peer discovery: will register with peer discovery backend ~ts",
       [Backend],
       #{domain => ?RMQLOG_DOMAIN_PEER_DISC}),
    case Backend:register() of
        ok ->
            ok;
        {error, _Reason} = Error ->
            ?LOG_ERROR(
               "Peer discovery: failed to register with peer discovery "
               "backend ~ts: ~tp",
               [Backend, Error],
               #{domain => ?RMQLOG_DOMAIN_PEER_DISC}),
            ok
    end.


-spec unregister(Backend) -> ok when
      Backend :: backend().

unregister(Backend) ->
  ?LOG_INFO(
     "Peer discovery: will unregister with peer discovery backend ~ts",
     [Backend],
     #{domain => ?RMQLOG_DOMAIN_PEER_DISC}),
  case Backend:unregister() of
    ok ->
          ok;
    {error, _Reason} = Error ->
      ?LOG_ERROR(
         "Peer discovery: failed to unregister with peer discovery "
         "backend ~ts: ~tp",
        [Backend, Error],
        #{domain => ?RMQLOG_DOMAIN_PEER_DISC}),
      ok
  end.

-spec lock(Backend, SelectedNode) -> Ret when
      Backend :: backend(),
      SelectedNode :: node(),
      Ret :: {ok, Data} | not_supported | {error, Reason},
      Data :: any(),
      Reason :: string().

lock(Backend, SelectedNode) ->
    ?LOG_INFO(
       "Peer discovery: will try to lock with peer discovery backend ~ts",
       [Backend],
       #{domain => ?RMQLOG_DOMAIN_PEER_DISC}),
    %% We want to acquire a lock for two nodes: this one and the selected
    %% node. This protects against concurrent cluster joins.
    %%
    %% Some backends used to use the entire list of discovered nodes and used
    %% `global' as the lock implementation. This was a problem because a side
    %% effect was that all discovered Erlang nodes were connected to each
    %% other. This led to conflicts in the global process name registry and
    %% thus processes killed randomly. This was the case with the feature
    %% flags controller for instance.
    %%
    %% Peer discovery shouldn't connect to all discovered nodes before it is
    %% ready to actually join another node. And it should only connect to that
    %% specific node, not all of them.
    ThisNode = node(),
    NodesToLock = [ThisNode, SelectedNode],
    case Backend:lock(NodesToLock) of
        {error, Reason} = Error ->
            ?LOG_ERROR(
               "Peer discovery: failed to lock with peer discovery "
               "backend ~ts: ~0tp",
               [Backend, Reason],
               #{domain => ?RMQLOG_DOMAIN_PEER_DISC}),
            Error;
        Any ->
            Any
    end.

-spec unlock(Backend, Data) -> Ret when
      Backend :: backend(),
      Data :: any(),
      Ret :: ok | {error, Reason},
      Reason :: string().

unlock(Backend, Data) ->
    ?LOG_INFO(
       "Peer discovery: will try to unlock with peer discovery "
       "backend ~ts",
       [Backend],
       #{domain => ?RMQLOG_DOMAIN_PEER_DISC}),
    case Backend:unlock(Data) of
        {error, Reason} = Error ->
            ?LOG_ERROR(
               "Peer discovery: failed to unlock with peer discovery "
               "backend ~ts: ~0tp, lock data: ~0tp",
               [Backend, Reason, Data],
               #{domain => ?RMQLOG_DOMAIN_PEER_DISC}),
            Error;
        Any ->
            Any
    end.

%%
%% Implementation
%%

-spec normalize(Nodes :: [node()] |
                {Nodes :: [node()],
                 NodeType :: rabbit_types:node_type()} |
                {ok, Nodes :: [node()]} |
                {ok, {Nodes :: [node()],
                      NodeType :: rabbit_types:node_type()}} |
                {error, Reason :: string()}) ->
    {ok, {Nodes :: [node()], NodeType :: rabbit_types:node_type()}} |
    {error, Reason :: string()}.

normalize(Nodes) when is_list(Nodes) ->
  {ok, {Nodes, disc}};
normalize({Nodes, NodeType}) when is_list(Nodes) andalso is_atom(NodeType) ->
  {ok, {Nodes, NodeType}};
normalize({ok, Nodes}) when is_list(Nodes) ->
  {ok, {Nodes, disc}};
normalize({ok, {Nodes, NodeType}}) when is_list(Nodes) andalso is_atom(NodeType) ->
  {ok, {Nodes, NodeType}};
normalize({error, Reason}) ->
  {error, Reason}.

-spec node_prefix() -> string().

node_prefix() ->
    case string:tokens(atom_to_list(node()), ?NODENAME_PART_SEPARATOR) of
        [Prefix, _] -> Prefix;
        [_]         -> ?DEFAULT_PREFIX
    end.

-spec append_node_prefix(Value :: binary() | string()) -> string().

append_node_prefix(Value) when is_binary(Value) orelse is_list(Value) ->
    Val = rabbit_data_coercion:to_list(Value),
    Hostname = case string:tokens(Val, ?NODENAME_PART_SEPARATOR) of
                   [_ExistingPrefix, HN] -> HN;
                   [HN]                  -> HN
               end,
    string:join([node_prefix(), Hostname], ?NODENAME_PART_SEPARATOR).
