-module(list_server_proper_statem).

-behaviour(proper_statem).

-include_lib("proper/include/proper.hrl").

%% proper_statem behaviour callbacks
-export([initial_state/0, precondition/2, command/1, postcondition/3, next_state/3]).

%% Properties
-export([prop_list_server/0]).

-record(state, {
    monitored_length
}).

%%%===================================================================
%%% STATEM CALLBACKS
%%%===================================================================

%% Initialize the state
initial_state() ->
    #state{monitored_length=0}.

command(_S) ->
    N=nat(),
    oneof([{call, list_server, add, [N]}, {call, list_server, remove, [N]}]).

%% Next state transformation, S is the current state
next_state(S, _V, _) ->
    S.
%next_state(#state{monitored_length=L}=S, V, {call, list_server, add, [_N]}) ->
%    S#state{monitored_length=L+1};
%next_state(#state{monitored_length=L}=S, V, {call, list_server, remove, [_N]}) ->
%    case V of
%        ok -> S#state{monitored_length=L-1};
%        {error, not_found} -> S
%    end.

precondition(_S, _) ->
    true.

postcondition(_S, {call, list_server, add, [N]}, _Res) ->
    MyList=list_server:get_list(),
    lists:member(N, MyList);

postcondition(_S, {call, list_server, remove, [_N]}, ok) ->
    true;

postcondition(_S, {call, list_server, remove, [N]}, {error, not_found}) ->
    MyList=list_server:get_list(),
    not lists:member(N, MyList).

%% PROPERTY
%% Generates a list of commands from the callbacks in ?MODULE.
%% When fail it shows an error trace for debugging.
prop_list_server() ->
   ?FORALL(Cmds, commands(?MODULE),
        begin
            {_H, _S, Res} = run_commands(?MODULE, Cmds),
            Res == ok
        end).


