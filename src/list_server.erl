-module(list_server).

-behaviour(gen_server).

-record(state, {
    mylist
}).

%% API
-export([start_link/0, add/1, remove/1, get_list/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec start_link() -> {ok,pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec add(integer()) -> ok.
add(N) ->
    gen_server:call(?MODULE, {add, N}).

-spec remove(integer()) -> ok | {error, not_found}.
remove(N) ->
    gen_server:call(?MODULE, {remove, N}).

-spec get_list() -> [integer()].
get_list() ->
    gen_server:call(?MODULE, get_list).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init([]) ->
    InitialState = #state{mylist=[]},
    {ok, InitialState}.

%handle_call({add, 5}, _From, #state{mylist=MyList} = State) ->
%%    throw(bug),
%    {reply, ok, State};
handle_call({add, N}, _From, #state{mylist=MyList} = State) ->
    {reply, ok, State#state{mylist=[N|MyList]}};

handle_call({remove, N}, _From, #state{mylist=MyList} = State) ->
    {Res, NewState} = case lists:member(N, MyList) of
        true -> {ok, State#state{mylist=MyList--[N]}};
        false -> {{error, not_found}, State}
    end,
    {reply, Res, NewState};

handle_call(get_list, _From, #state{mylist=MyList} = State) ->
    {reply, MyList, State};

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

%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------

