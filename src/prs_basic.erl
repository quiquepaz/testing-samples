-module(prs_basic).

-include_lib("proper/include/proper.hrl").

-compile(export_all).

remove_duplicates([]) ->
    [];
remove_duplicates([_|T]) ->
    T.

%remove_duplicates(L) ->
%    N=random:uniform(length(L)),
%    [H|_]=L,
%    Res=lists:usort(L),
%    case N>10 of
%        true -> [H | Res];
%        false -> Res
%    end.

%remove_duplicates(L) ->
%    sets:to_list(sets:from_list(L)).

%% Tests

prop_remove_duplicates_keeps_all_elements() ->
    ?FORALL({L, N}, ?SUCHTHAT({L2, M}, {custom_list(), nat()}, (M>0) andalso (M=<length(L2))),
        begin
            Processed=remove_duplicates(L),
            ?WHENFAIL(
                io:format("L=~p, N=~p, Processed=~p~n", [L, N, Processed]),
                (L==[]) orelse (lists:member(lists:nth(N, L), Processed))
            )
        end
    ).

prop_remove_duplicates_contains_no_duplicates() ->
    numtests(1000, ?FORALL(Original, ne_custom_list(),
        begin
                L=remove_duplicates(Original),
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
    list(nat()).
