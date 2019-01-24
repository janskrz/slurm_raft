%% Some wrapper for rabbitmq/ra
%%

-module(raft_console).

-export([create/1, status/1, status/0]).

-export([read/0, inc/0]).

create(ErlangNodeStrings) when is_list(hd(ErlangNodeStrings)) ->
    create([list_to_atom(lists:delete($', X)) || X <- ErlangNodeStrings]);

create(ErlangNodes) ->
    ServerIds = [{counter_server, N} || N <- ErlangNodes],

    {ok, ServersStarted, []} = ra:start_cluster(counter_cluster,
                                                {simple, fun erlang:'+'/2, 0},
                                                ServerIds),
    io:format("Raft cluster started with nodes:~n~p~n", [ServersStarted]),
    ok.


%% Assumes that this node is included in the ErlangNodes list given in create...

status([]) ->
    Status = status(),
    io:format("Raft cluster status:~n~p~n", [Status]),
    ok.

status() -> ra:members({counter_server, node()}).

read() ->
    {ok, Result, _Leader} = ra:consistent_query({counter_server, node()}, fun (S) -> S end),
    {ok, Result}.

inc() ->
    {ok, _Reply, _Leader} = ra:process_command({counter_server, node()}, 1),
    ok.
