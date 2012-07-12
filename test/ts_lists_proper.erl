-module(ts_lists_proper).

-include_lib("proper/include/proper.hrl").

-compile(export_all).

prop_remove_duplicates_keeps_all_elements() ->
    numtests(1000, ?FORALL(L, ne_custom_list(),
        begin
            Processed=ts_lists:remove_duplicates(L),
            ?WHENFAIL(
                io:format("L=~p, Processed=~p~n", [L, Processed]),
                lists:all(fun (N) -> lists:member(N, Processed) end, L)
            )
        end
    )).

prop_remove_duplicates_doesnt_create_elements() ->
    numtests(1000, ?FORALL(L, ne_custom_list(),
        begin
            Processed=ts_lists:remove_duplicates(L),
            ?WHENFAIL(
                io:format("L=~p, Processed=~p~n", [L, Processed]),
                lists:all(fun (N) -> lists:member(N, L) end, Processed)
            )
        end
    )).

prop_remove_duplicates_contains_no_duplicates() ->
    numtests(1000, ?FORALL(Original, ne_custom_list(),
        begin
                L=ts_lists:remove_duplicates(Original),
                Seq=lists:seq(1, length(L)),
                ?WHENFAIL(io:format("Original=~p, Processed=~p~n", [Original, L]),
                    lists:all(fun (N) ->
                                Elem=lists:nth(N, L),
                                %io:format("L=~p, Elem=~p~n", [L, Elem]),
                                {_Before, [Elem|After]} = lists:splitwith(fun (X) -> X/=Elem end, L),
                                not lists:member(Elem, After)
                        end, Seq)
                )
        end
    )).

%% Generators

ne_custom_list() ->
    ?SUCHTHAT(L, custom_list(), L/=[]).

custom_list() ->
    list(integer()).
