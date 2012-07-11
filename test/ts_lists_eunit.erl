-module(ts_lists_eunit).

-include_lib("eunit/include/eunit.hrl").

-define(VALID_TEST_FILE, "/tmp/ts_lists_eunit.sample").
-define(INVALID_TEST_FILE, "/tmp/a_file_that_does_not_exist").

simple_cases_test_() ->
    [
        ?_assertEqual([], ts_lists:remove_duplicates([])),
        % Bad testcase (dependent / forcing on the implementation!!)
        ?_assertEqual([1, 2, 3, 4, 5], ts_lists:remove_duplicates([1, 2, 2, 3, 4, 4, 5]))
    ].

application_starts_test_() ->
    [
        ?_assertEqual(ok, start_app(testing_samples))
    ].

load_lists_from_file_test_() ->
    {setup,
        fun() -> setup_files() end,
        fun ({ValidFile, _InvalidFile}) -> ok=file:delete(ValidFile) end,
        fun ({ValidFile, InvalidFile}) ->
            [
                ?_assertEqual({ok, [[1, 2, -2, -2, -1], [-2, 2, 2, 1]]}, ts_lists:load_lists_from_file(ValidFile)),
                ?_assertEqual({error, enoent}, ts_lists:load_lists_from_file(InvalidFile))
            ]
        end
    }.

remove_duplicates_from_lists_file_test_() ->
    {setup,
        fun() -> setup_files() end,
        fun ({ValidFile, _InvalidFile}) -> ok=file:delete(ValidFile) end,
        fun ({ValidFile, InvalidFile}) ->
            [
                ?_assertEqual({error, enoent}, ts_lists:remove_duplicates_from_lists_file(InvalidFile)),
                ?_assertMatch({ok, [L1, L2]} when is_list(L1) and is_list(L2) , ts_lists:remove_duplicates_from_lists_file(ValidFile))
            ]
        end
    }.

%% Internal functions

setup_files() ->
    ok=filelib:ensure_dir(?VALID_TEST_FILE),
    ok=file:write_file(?VALID_TEST_FILE, "[1, 2, -2, -2, -1].\n[-2, 2, 2, 1].\n"),
    file:delete(?INVALID_TEST_FILE),
    {?VALID_TEST_FILE, ?INVALID_TEST_FILE}.


start_app(App) ->
    case application:start(App) of
        ok ->
            application:stop(App),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

