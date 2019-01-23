%%-------------------------------------------------------------------
%% @doc slurm_raft top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(slurm_raft_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    DataRoot = os:getenv("CONSENSUS_DATA_ROOT", "./data"),
    NodeDataDir = filename:join(DataRoot, atom_to_list(node())),

    ok = application:set_env(ra, data_dir, NodeDataDir),

    RaftChildSup = {ra_sup,
                    {ra_sup, start_link, []},
                    permanent, infinity, supervisor, [ra_sup]},

    {ok, { {one_for_all, 0, 1}, [RaftChildSup]} }.

%%====================================================================
%% Internal functions
%%====================================================================
