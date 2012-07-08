-module(ts_lists_eunit).

-include_lib("eunit/include/eunit.hrl").

-define(VALID_TEST_FILE, "/tmp/ts_lists_eunit.sample").
-define(INVALID_TEST_FILE, "/tmp/a_file_that_does_not_exist").

simple_cases_test_() ->
    [
        ?_assertEqual([], ts_lists:remove_duplicates([])),
        % Bad testcase (dependent on the implementation!!)
        ?_assertEqual([1, 2, 3, 4, 5], ts_lists:remove_duplicates([5, 1, 2, 2, 3, 4, 4]))
    ].

application_starts_test_() ->
    [
        ?_assertEqual(ok, start_app(testing_samples))
    ].

lists_from_file_test_() ->
    {setup,
        fun() ->
                ok=filelib:ensure_dir(?VALID_TEST_FILE),
                ok=file:write_file(?VALID_TEST_FILE, "[1, 2, -2, -2, -1].\n[-2, 2, 2, 1].\n"),
                file:delete(?INVALID_TEST_FILE),
                {?VALID_TEST_FILE, ?INVALID_TEST_FILE}
        end,
        fun ({F, F2}) ->
                ok=file:delete(F)
        end,
        fun ({F, F2}) ->
            [
                ?_assertEqual({ok, [[1, 2, -2, -2, -1], [-2, 2, 2, 1]]}, ts_lists:load_lists_from_file(F)),
                ?_assertEqual({error, enoent}, ts_lists:load_lists_from_file(F2)),
                ?_assertEqual({error, enoent}, ts_lists:remove_duplicates_from_lists_file(F2)),
                ?_test(
                    begin
                            {ok, [L1, L2]} = ts_lists:remove_duplicates_from_lists_file(F),
                            ?assertEqual(4, length(L1)),
                            ?assertEqual(3, length(L2))
                    end
                )
            ]
        end
    }.

%% Internal functions
start_app(App) ->
    case application:start(App) of
        ok ->
            application:stop(App),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

