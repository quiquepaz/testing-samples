-module(ts_lists).

-ifdef(TESTING).
    -export([load_lists_from_file/1]).
-endif.

-export([remove_duplicates_from_lists_file/1, remove_duplicates/1]).

%%--------------------------------------------------------------------
%% @doc
%% Takes a list of numbers as input and returns that list without duplicates in it
-spec remove_duplicates([integer()]) -> [integer()].
%% @end
%%--------------------------------------------------------------------
remove_duplicates([]) ->
    [];
remove_duplicates(L) when is_list(L) ->
    lists:usort(L).

%remove_duplicates([]) ->
%    [];
%remove_duplicates(L) ->
%    N=random:uniform(length(L)),
%    [H|_]=L,
%    Res=lists:usort(L),
%    case N>10 of
%        true -> [H | Res];
%        false -> Res
%    end.

%%--------------------------------------------------------------------
%% @doc
%% Parses the input lists from a file in 'consultable' format and applies remove_duplicates to everyone of them.
-spec remove_duplicates_from_lists_file(string()) -> {ok, [list(integer())]} | {error, term()}.
%% @end
%%--------------------------------------------------------------------
remove_duplicates_from_lists_file(File) ->
    case load_lists_from_file(File) of
        {error, Reason} ->
            {error, Reason};
        {ok, Lists} ->
            {ok, [remove_duplicates(L) || L <- Lists]}
    end.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

-spec load_lists_from_file(string()) -> {ok, [list(integer())]} | {error, term()}.
load_lists_from_file(File) ->
    case file:consult(File) of
        {error, Reason} ->
            {error, Reason};
        {ok, Terms} ->
            {ok, [L || L <- Terms, is_list(L)]}
    end.


