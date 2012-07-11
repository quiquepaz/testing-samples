-module(resource_manager).

-behaviour(gen_server).

%% API
-export([start_link/1, launch_and_register/3, unregister/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% exported just for testing
-export([who_fix/2, get_candidates_to_fix/3]).

-define(SERVER, ?MODULE).

-record(info, {priority, cancelcb}).
-record(state, {registered = dict:new(), % The info for each Id.
                next_id=0,               % The next Id to use
                priorities
               }).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Priorities) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Priorities], []).

stop() ->
    gen_server:call(?SERVER, stop).

%%--------------------------------------------------------------------
%% @doc  Tries to launch an operation that uses the resource.
%%
%%       If it works, then returns one identifier for this operation.
%%       So, when the operation finish, and release the resource, it will
%%       be unregistered, in the resource_manager, using that identifier.
%%
%%       If can not do that, tries to cancel the operation with less priority.
%%       Then tries again launch the operation.
%%
%%       If the cancelation fails or the operation still can not be possible,
%%       returns no_resource_available.
%%
%% OpCb = fun(() -> ok). 
%%   It is the fuction that actually executes the operation that uses the resource.
%%   It can throw 'no_resource_available'.
%%
%% AdaptCb = fun((Freq::integer(), Prog::integer()) -> ok).
%%   It is the function that adapts the operation associated using the frequency
%%   and program indicated.
%%   It can throws any error.
%%
%% CancelCb = fun(() -> ok).
%%   It is the function that cancels the operation associated.
%%   It can throw any error.
%%
%% @spec launch_and_register(Priority::atom(), OpCb::fun(), CancelCb::fun()) ->
%%          Id::integer() | {error, no_resource_available} | {error, wrong_priority}
%% @end
%%--------------------------------------------------------------------
launch_and_register(Priority, OpCb, CancelCb) ->
    gen_server:call(?SERVER, {launch_and_register, Priority, OpCb, CancelCb}, infinity).

%%--------------------------------------------------------------------
%% @doc  Deletes registration information stored for the indicated 'Id'
%% @spec unregister(Id::integer()) -> ok
%% @end
%%--------------------------------------------------------------------
unregister(Id) ->
    gen_server:call(?SERVER, {unregister, Id}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Priorities]) ->
    {ok, #state{priorities = Priorities, registered=dict:new(), next_id=0}}.

handle_call({launch_and_register, Priority, OpCb, CancelCb}, _From, State) ->
    {Reply, NewState} = launch_intern(Priority, OpCb, CancelCb, State),
    io:format("Assigned id: ~p~n",[State#state.next_id]),
    {reply, Reply, NewState};

handle_call({unregister, Id}, _From, State) ->
    io:format("Unregistering the id: ~p~n",[Id]),
    NewState = unregister_intern(Id, State),
    {reply, ok, NewState};

handle_call(stop, _From, State) ->
    {stop, normal, State};

handle_call(_, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc  Tries to launch an operation that uses the resource.
%%
%%       If it works, then returns one identifier for this operation.
%%       So, when the operation finish, and release the resource,
%%       unregister it in the resource_manager using that identifier.
%%
%%       If it can not start, tries to cancel the operation
%%       with less priority. Then tries to launch the operation again.
%%
%%       If the cancelation fails returns no_resource_available.
%%
%% @spec launch_intern(Priority::atom(), OpCb::fun(), CancelCb::fun(), State::state()) ->
%%          Id::integer() | {error, no_resource_available} | {error, wrong_priority}
%% @end
%%--------------------------------------------------------------------
launch_intern(Priority,OpCb,CancelCb,State) ->
    case check_priority(Priority,State#state.priorities) of
        {error, wrong_priority} ->
            {{error, wrong_priority}, State};
        ok ->
            do_launch_intern(Priority,OpCb,CancelCb,State)
    end.

do_launch_intern(Priority,OpCb,CancelCb,State) ->
    case launch_callback(OpCb,[]) of
        {error, no_resource_available} ->
            case who_fix(Priority, State) of
                {error, no_resource_available} ->
                    % Nothing can be cancelled
                    {{error, no_resource_available}, State};
                {Id, #info{cancelcb=CancelCb}} ->
                    case cancel(Id, CancelCb, State) of
                        {error, no_resource_available} ->
                            % Failed to cancel, nothing else we can do
                            {error, no_resource_available};
                        #state{}=UpdatedState ->
                            % Succesfully cancelled, so retry the launch
                            case launch_callback(OpCb,[]) of
                                {error, no_resource_available} ->
                                    % Nothing else to do
                                    {{error, no_resource_available}, UpdatedState};
                                ok ->
                                    % After cancelling we succeeded!
                                    NewState=register_intern(Priority,CancelCb,UpdatedState),
                                    {NewState#state.next_id, State}
                            end
                    end
            end;
        ok ->
            % No problem at all
            NewState=register_intern(Priority,CancelCb,State),
            {NewState#state.next_id, State}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc  Tries to cancel an operation that is using a resource.
%% @spec cancel(CancelCb::fun(), Id::integer(), State) -> UpdatedState | {error, no_resource_available}
%% @end
%%--------------------------------------------------------------------
cancel(CancelCb, Id, State) ->
    try
        launch_callback(CancelCb, []),
        %io:format("Operation cancelled~n",[])
        unregister_intern(Id, State)
    catch
        _:_ ->
            {error, no_resource_available}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc  Returns the info of the operation to fix.
%% @spec who_fix(Priority::atom(), State::state()) -> {Id::integer(), Info::info()} | {error, no_resource_available}
%% @end
%%--------------------------------------------------------------------
who_fix(Priority, State) ->
    Reg = State#state.registered,
    PList = State#state.priorities,
    OnlyLower = get_candidates_to_fix(Priority, Reg, PList),
    Candidates = dict:to_list(OnlyLower),
    case Candidates of
      [] ->
            %io:format("Nothing could be adapted or cancelled",[]),
            {error, no_resource_available};
      [_H|_T] ->
            get_least(PList,Candidates,least)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc  Returns a dictionary only with the elements that could be
%%       candidates to be cancelled, that is, those which priority
%%       is lesser that the indicated.
%% @spec get_candidates_to_fix(Priority::atom(), Reg::dict(),
%%                             PList::[atom()]) -> dict()
%% @end
%%--------------------------------------------------------------------
get_candidates_to_fix(Priority, Reg, PList) ->
    dict:filter(
      fun (_Id,Info) ->
              P = Info#info.priority, 
              higher_priority(Priority,P,PList)
      end,Reg).

%%--------------------------------------------------------------------
%% @private
%% @doc  Obtains the information for the operation with the least priority.
%% @spec get_least(PList::[atom()],
%%                 OpList::[{integer(), info()}],
%%                 Res:: least |
%%                       {Key::integer(), Info::info()}) -> Res
%% @end
%%--------------------------------------------------------------------
get_least(_PList, [], Least) ->
    Least;
get_least(PList, [{Key, Info} | T], least) ->
    get_least(PList, T, {Key, Info});
get_least(PList, [{Key, Info} | T], {LeastKey, LeastInfo}) ->
    P = Info#info.priority,
    LeastP = LeastInfo#info.priority,
    case higher_priority(LeastP, P, PList) of
        true ->
            get_least(PList, T, {Key, Info});
        false ->
            get_least(PList, T, {LeastKey, LeastInfo})
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc  Registers an operation that is using the resource.
%% @spec register_intern(Priority::atom(),
%%                       CancelCb::fun(),
%%                       State::state()) -> Newstate::state()
%% @end
%%--------------------------------------------------------------------
register_intern(Priority, CancelCb, State) ->
    State#state{registered=dict:store(
                             State#state.next_id,
                             #info{priority=Priority,
                                   cancelcb=CancelCb},
                             State#state.registered),
                 next_id = State#state.next_id +1}.

%%--------------------------------------------------------------------
%% @private
%% @doc  Deletes the registration info for the indicated 'Id'.
%% @spec unregister_intern(Id::integer(), State::state()) -> NewState::state()
%% @end
%%--------------------------------------------------------------------
unregister_intern(Id, State) ->
     State#state{registered=dict:erase(Id, State#state.registered)}.

%%--------------------------------------------------------------------
%% @private
%% @doc Checks wether a given priority is member of a given priority list. This
%% function should be called before using any other function which works with
%% priorities.
%% @spec check_priority(atom(), [atom()]) -> ok | {error, wrong_priority}
%% @end
%%--------------------------------------------------------------------
check_priority(P, Priorities) ->
  case lists:member(P, Priorities) of
    true -> ok;
    false -> {error, wrong_priority}
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc Returns wether the first given priority is higher than the second.
%% Ordering criteria is natural order on the given priorities list. If
%% priorities list is empty, this method returns false.
%% @spec higher_priority(atom(), atom(), [atom()]) -> boolean()
%% @end
%%--------------------------------------------------------------------
higher_priority(_P1, _P2, []) ->
  false;
higher_priority(P1, P1, _Priorities) ->
  false;
higher_priority(P1, _P2, [P1 | _Priorities]) ->
  true;
higher_priority(_P1, P2, [P2 | _Priorities]) ->
  false;
higher_priority(P1, P2, [_P | Priorities]) ->
  higher_priority(P1, P2, Priorities).

%%--------------------------------------------------------------------
%% @private
%% @doc Launches a  callback.
%% @spec launch_callback(Cb::fun(), Arg::[term()]) -> ok
%% @end
%%--------------------------------------------------------------------
launch_callback(F, Arg) ->
    erlang:apply(F, Arg).

