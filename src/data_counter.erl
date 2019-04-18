-module(data_counter).
-behaviour(ra_machine).

-export([init/1, apply/3]).

%% Implements a simple counter with binary data attached to benchmark behaviour
%% with increasing data/command size

-type counter_inc_command() :: {inc} | {read} | {inc, Data :: binary()}.

init(Config) ->
    PayloadSize = maps:get(payload_size, Config),
    Payload = crypto:strong_rand_bytes(PayloadSize),
    io:format("Creating data counter with data payload size of ~p bytes.~n", [PayloadSize]),
    {0, Payload}.

apply(_Meta, {read}, State) ->
    {Counter, _Data} = State,
    {State, Counter, []};

apply(_Meta, {inc}, State) ->
    {Counter, Data} = State,
    NewState = {Counter + 1, Data},
    {NewState, ok, []};

apply(_Meta, {inc, NewData}, State) ->
    {Counter, _Data} = State,
    NewState = {Counter + 1, NewData},
    {NewState, ok, []}.
