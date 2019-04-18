%% Some wrapper for rabbitmq/ra
%%

-module(raft_console).

-export([create/1, status/1, status/0]).

-export([read/0, inc/0, inc/1]).

create(ErlangNodeStrings) when is_list(hd(ErlangNodeStrings)) ->
    create([list_to_atom(lists:delete($', X)) || X <- ErlangNodeStrings]);

create(ErlangNodes) ->
    ServerIds = [{counter_server, N} || N <- ErlangNodes],

    {ok, PayloadSize} = application:get_env(ra, payload_size),
    StateMachineConfig = #{
            payload_size => PayloadSize
        },
    StateMachine = {module, data_counter, StateMachineConfig},

    {ok, ServersStarted, []} = ra:start_cluster(counter_cluster, StateMachine, ServerIds),
    io:format("Raft cluster started with nodes:~n~p~n", [ServersStarted]),
    ok.


%% Assumes that this node is included in the ErlangNodes list given in create...

status([]) ->
    Status = status(),
    io:format("Raft cluster status:~n~p~n", [Status]),
    ok.

status() -> ra:members({counter_server, node()}).

read() ->
    {ok, Result, _Leader} = ra:process_command({counter_server, node()}, {read}),
    {ok, Result}.

inc() ->
    {ok, _Reply, _Leader} = ra:process_command({counter_server, node()}, {inc}),
    ok.

inc(Data) ->
    {ok, _Reply, _Leader} = ra:process_command({counter_server, node()}, {inc, Data}),
    ok.

