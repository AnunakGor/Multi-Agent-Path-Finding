-module(test).
-include_lib("eunit/include/eunit.hrl").

vertex_conflict_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     {generator, fun test_cases/0}}.

setup() ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    bot_simulation:init_db(),
    ok.

cleanup(_) ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    ok.

test_cases() ->
    [
        ?_test(test_simple_case()),
        ?_test(test_crossing_case()),
        ?_test(test_three_bots_case())
    ].

    test_simple_case() ->
        {timeout, 10, fun() ->
            Bots = [
                {1, {1,1}, {1,3}},
                {2, {2,1}, {2,3}}
            ],
            Paths = cbs:search(Bots),
            ?assertNotMatch({error, no_solution}, Paths),
            check_vertex_conflicts(Paths)
        end}.
    
    test_crossing_case() ->
        {timeout, 10, fun() ->
            Bots = [
                {1, {1,2}, {3,2}},
                {2, {3,2}, {1,2}}
            ],
            Paths = cbs:search(Bots),
            ?assertNotMatch({error, no_solution}, Paths),
            check_vertex_conflicts(Paths)
        end}.
    
    test_three_bots_case() ->
        {timeout, 20, fun() ->
            Bots = [
                {1, {1,1}, {1,3}},
                {2, {2,1}, {3,1}},
                {3, {3,3}, {2,3}}
            ],
            Paths = cbs:search(Bots),
            ?assertNotMatch({error, no_solution}, Paths),
            check_vertex_conflicts(Paths)
        end}.

check_vertex_conflicts(Paths) ->
    MaxTime = case Paths of
        {error, _} -> 0;
        _ -> lists:max([length(Path) || {_BotId, Path} <- Paths])
    end,
    lists:foreach(
        fun(Time) ->
            Positions = [get_position_helper(Path, Time) || {_BotId, Path} <- Paths],
            UniquePositions = lists:usort(Positions),
            ?assertEqual(
                length(Positions),
                length(UniquePositions),
                io_lib:format("Vertex conflict at time ~p: Positions ~p", [Time, Positions])
            )
        end,
        lists:seq(0, MaxTime - 1)
    ).

get_position_helper(Path, Time) ->
    if
        Time < length(Path) -> lists:nth(Time + 1, Path);
        true -> lists:last(Path)
    end.