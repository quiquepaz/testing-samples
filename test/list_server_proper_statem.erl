-module(list_server_proper_statem).

-behaviour(proper_statem).

-include_lib("proper/include/proper.hrl").

%% proper_statem behaviour callbacks
-export([initial_state/0, precondition/2, command/1, postcondition/3, next_state/3]).

-export([add/1, remove_ok/1, remove_error/1]).

%% Properties
-export([prop_list_server/0]).

-record(state, {
    mylist
}).

%%%===================================================================
%%% STATEM CALLBACKS
%%%===================================================================

%% Initialize the state
initial_state() ->
    #state{mylist=[]}.

command(#state{mylist=MyList}) ->
    N=nat(),
    oneof(
        [{call, ?MODULE, add, [N]}] ++
        [{call, ?MODULE, remove_ok, [N]} || lists:member(N, MyList)] ++
        [{call, ?MODULE, remove_error, [N]} || not lists:member(N, MyList)]
    ).

%% Next state transformation, S is the current state
next_state(S, _V, {call, ?MODULE, _, [_N]}) ->
    S#state{mylist=list_server:get_list()}.

precondition(_S, _) ->
    true.

%%

postcondition(_S, {call, ?MODULE, add, [N]}, ok) ->
    MyList=list_server:get_list(),
    lists:member(N, MyList);

postcondition(_S, {call, ?MODULE, remove_ok, [_N]}, ok) ->
    true;

postcondition(_S, {call, ?MODULE, remove_error, [N]}, {error, not_found}) ->
    MyList=list_server:get_list(),
    not lists:member(N, MyList);

postcondition(_S, Cmd, Res) ->
    MyList=list_server:get_list(),
    io:format("----> Cmd=~p, Res=~p, MyList=~p~n", [Cmd, Res, MyList]),
    false.

%% PROPERTY
%% Generates a list of commands from the callbacks in ?MODULE.
%% When fail it shows an error trace for debugging.
prop_list_server() ->
   ?FORALL(Cmds, commands(?MODULE),
        begin
            {H, S, Res} = run_commands(?MODULE, Cmds),
            list_server:clean(),
            ?WHENFAIL(format_trace(Cmds, H, S, Res), Res == ok)
        end).

%% Internal functions

add(N) ->
    list_server:add(N).

remove_ok(N) ->
    list_server:remove(N).

remove_error(N) ->
    list_server:remove(N).


format_trace(Cmds, History, FinalState, Result) ->
    io:format(
      format_steps(proper_statem:zip(Cmds, History))++
      lists:flatten(io_lib:format(
        "~n============~nFinal State:~p~nRes:~p~n",[FinalState, Result]))).

format_steps(L) ->
    lists:concat(
      ["======~nTrace:~n======~n" |
       [format_step(Cmd, State, Result) || {Cmd, {State, Result}} <- L]
      ]).

format_step({set, _Var, {call, M, F, A}}, State, Result) ->
    lists:flatten(io_lib:format(
            "~p~n~n~p:~p(~w) ->~n~p~n------------~n", [State, M, F, A, Result])).

