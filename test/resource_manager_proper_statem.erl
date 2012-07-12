-module(resource_manager_proper_statem).

-behaviour(proper_statem).

-include_lib("proper/include/proper.hrl").

-define(PRIORITIES, [high, medium, low]).

%% Aux
-export([launch_and_register_fail/3, launch_and_register/3, launch_and_register_cancel/3, unregister/1, unregister_not_registered/1]).

%% proper_statem behaviour callbacks
-export([initial_state/0, precondition/2, command/1, postcondition/3, next_state/3]).
%% Properties
-export([prop_resource_manager/0]).

%%%===================================================================
%%% STATE
%%%===================================================================
-record(info, {priority, cancelcb}).
-record(state,{registered, next_id, priorities}).

%%%===================================================================
%%% GENERATORS
%%%=================================================================== 
opcb() ->
  fun() -> ok end.

opcbfail() ->
    fun() ->
        {error, no_resource_available}
    end.

cancelcb() ->
  fun() -> ok end.

cancelcbfail() ->
    fun() -> throw(no_resource_available) end.

id() ->
  integer().

%%%===================================================================
%%% STATEM CALLBACKS
%%%===================================================================

%% Initialize the state
initial_state() ->
    erlang:put(num, 0),
    #state{registered=dict:new(), next_id = 0, priorities = ?PRIORITIES}.

%% Command generator, S is the state
command(#state{registered=Reg}) ->
    Id = id(),
    Priority = oneof(?PRIORITIES),
    ToFix =  dict:to_list(resource_manager:get_candidates_to_fix(Priority, Reg, ?PRIORITIES)),
    Keys = dict:fetch_keys(Reg),
    oneof(
      [{call, ?MODULE, launch_and_register,
        [Priority, opcb(), cancelcb()]}] ++

      [{call, ?MODULE, launch_and_register_cancel,
        [Priority, opcbfail(), cancelcb()]} || ToFix=:=[]] ++ % opcb() will fail

      [{call, ?MODULE, launch_and_register_fail,
        [Priority, opcbfail(), cancelcbfail()]} || ToFix/=[]] ++ % all cb's will fail,

      [{call, ?MODULE, unregister, [oneof(Keys)]} || Keys /=[]] ++
      [{call, ?MODULE, unregister_not_registered, [Id]} || not lists:member(Id, Keys)] % unregister always will work
     ).

%% Next state transformation, S is the current state
next_state(S, V, {call, ?MODULE, launch_and_register, [Priority,  _Opcb, CancelCb]}) ->
    % Callback can be executed
    NewInfo = #info{priority=Priority, cancelcb=CancelCb},
    S#state{registered=dict:store(V, NewInfo, S#state.registered)};

next_state(S, _V, {call, ?MODULE, launch_and_register_fail, [_Priority,  _Opcb, _CancelCb]}) ->
    % Callback cannot be executed and cancellation fails
    S;

next_state(S, V, {call, ?MODULE, launch_and_register_cancel, [Priority, _Opcb, CancelCb]}) when is_integer(V)->
    % Callback fails, then we cancel, then callback succeeds
    {Id, _Info} = resource_manager:who_fix(Priority, S),
    NewState = S#state{registered=dict:erase(Id, S#state.registered)},
    NewInfo = #info{priority=Priority, cancelcb=CancelCb},
    NewState#state{registered=dict:store(V, NewInfo, NewState#state.registered)};

next_state(S, _V, {call, ?MODULE, launch_and_register_cancel, [_Priority, _Opcb, _CancelCb]}) ->
    S;

next_state(S, _V, {call, ?MODULE, unregister, [Id]}) ->
    S#state{registered=dict:erase(Id, S#state.registered)};

next_state(S, _V, {call, ?MODULE, unregister_not_registered, [_Id]}) ->
    S.

%% Precondition, checked before command is added to the command sequence
%% XXX For negative testing do NOT use the preconditions.
precondition(_S, _) ->
    true.

%% Postcondition, checked after command has been evaluated
%% OBS: S is the state before next_state(S,_,<command>)
postcondition(_S, {call, ?MODULE, launch_and_register, [_Priority, _Opcb, _CancelCb]}, Res)
        when is_integer(Res) ->
    true;

postcondition(_S, {call, ?MODULE, launch_and_register_fail, [_Priority, _Opcb,  _CancelCb]}, _Res) ->
    {error, no_resource_available};

postcondition(_S, {call, ?MODULE, launch_and_register_cancel, [_Priority, _Opcb, _CancelCb]}, Res) ->
    (is_integer(Res)) or (Res=={error, no_resource_available});

postcondition(_S, {call, ?MODULE, unregister, [_Id]}, Res) ->
    Res==ok;

postcondition(_S, _Call, _Res) ->
    true.

%%%===================================================================
%%% PROPERTIES
%%%===================================================================

%% Generates a list of commands from the callbacks in ?MODULE.
%% When fail it shows an error trace for debugging.
prop_resource_manager() ->
   ?FORALL(Cmds, commands(?MODULE),
        begin
            {H, S, Res} = run_commands(?MODULE, Cmds),
            %resource_manager:stop(),
            ?WHENFAIL(format_trace(Cmds, H, S, Res), Res == ok)
        end).

%%%===================================================================
%%% AUXILIARY
%%%===================================================================

launch_and_register_cancel(Priority, OpCb, CancelCb) ->
    resource_manager:launch_and_register(Priority, OpCb, CancelCb).

launch_and_register_fail(Priority, OpCb, CancelCb) ->
    resource_manager:launch_and_register(Priority, OpCb, CancelCb).

launch_and_register(Priority, OpCb, CancelCb) ->
    resource_manager:launch_and_register(Priority, OpCb, CancelCb).

unregister(Id) ->
    resource_manager:unregister(Id).

unregister_not_registered(Id) ->
    resource_manager:unregister(Id).

%% DEBUGGING

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

is_even(N) ->
    (N rem 2) == 0.
