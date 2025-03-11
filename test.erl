-module(test).
-include_lib("eunit/include/eunit.hrl").

%% Type Definitions

-type coordinate() :: {integer(), integer()}.
-type path() :: [coordinate()].
-type bot_path() :: {integer(), path()}.
-type test_case() :: any().

%% @doc EUnit test suite for vertex conflicts with setup/cleanup and generator.
-spec vertex_conflict_test_() -> any().
vertex_conflict_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     {generator, fun test_cases/0}}.

%% @doc Setup: Stop mnesia, delete any schema, and initialize the DB.
-spec setup() -> ok.
setup() ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    bot_simulation:init_db(),
    ok.

%% @doc Cleanup: Stop mnesia and delete schema.
-spec cleanup(_Arg :: any()) -> ok.
cleanup(_) ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    ok.

%% @doc Generates a list of test cases.
-spec test_cases() -> [test_case()].
test_cases() ->
    [
        ?_test(test_simple_case()),
        ?_test(test_crossing_case()),
        ?_test(test_three_bots_case()),
        ?_test(test_edge_conflict_case()),
        ?_test(test_boundary_bots_case()),
        ?_test(test_many_bots_case()),
        ?_test(test_trivial_case()),
        ?_test(test_unsolvable_case())
    ].

%% @doc Tests two bots with non-conflicting vertex paths.
-spec test_simple_case() -> any().
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

%% @doc Tests two bots with crossing trajectories.
-spec test_crossing_case() -> any().
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

%% @doc Tests three bots with potential vertex conflicts.
-spec test_three_bots_case() -> any().
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

%% @doc Tests two bots with an edge conflict scenario (swapping positions).
-spec test_edge_conflict_case() -> any().
test_edge_conflict_case() ->
    {timeout, 10, fun() ->
        Bots = [
            {1, {2,2}, {2,3}},
            {2, {2,3}, {2,2}}
        ],
        Paths = cbs:search(Bots),
        ?assertNotMatch({error, no_solution}, Paths),
        check_vertex_conflicts(Paths),
        check_edge_conflicts(Paths)
    end}.

%% @doc Tests bots placed on grid boundaries.
-spec test_boundary_bots_case() -> any().
test_boundary_bots_case() ->
    {timeout, 10, fun() ->
        Bots = [
            {1, {1,1}, {1,10}},
            {2, {1,10}, {10,10}},
            {3, {10,10}, {10,1}},
            {4, {10,1}, {1,1}}
        ],
        Paths = cbs:search(Bots),
        ?assertNotMatch({error, no_solution}, Paths),
        check_vertex_conflicts(Paths),
        check_edge_conflicts(Paths)
    end}.

%% @doc Tests a more complex scenario with five bots.
-spec test_many_bots_case() -> any().
test_many_bots_case() ->
    {timeout, 20, fun() ->
        Bots = [
            {1, {1,1}, {10,10}},
            {2, {10,10}, {1,1}},
            {3, {1,10}, {10,1}},
            {4, {10,1}, {1,10}},
            {5, {5,1}, {5,10}}
        ],
        Paths = cbs:search(Bots),
        ?assertNotMatch({error, no_solution}, Paths),
        check_vertex_conflicts(Paths),
        check_edge_conflicts(Paths)
    end}.

%% @doc Tests the trivial case where a bot's start equals its goal.
-spec test_trivial_case() -> any().
test_trivial_case() ->
    {timeout, 5, fun() ->
        Bots = [
            {1, {5,5}, {5,5}}
        ],
        case cbs:search(Bots) of
            [{1, Path}] -> ?assertEqual([{5,5}], Path);
            _ -> ?assert(false)
        end
    end}.

%% @doc Tests an unsolvable case where two bots share identical start and goal.
-spec test_unsolvable_case() -> any().
test_unsolvable_case() ->
    {timeout, 10, fun() ->
        Bots = [
            {1, {1,1}, {1,3}},
            {2, {1,1}, {1,3}}
        ],
        Paths = cbs:search(Bots),
        ?assertMatch({error, no_solution}, Paths)
    end}.

%% @doc Checks that at each time step there are no vertex conflicts.
-spec check_vertex_conflicts(Paths :: [bot_path()]) -> ok.
check_vertex_conflicts(Paths) ->
    MaxTime = case Paths of
        {error, _} -> 0;
        _ -> lists:max([length(Path) || {_BotId, Path} <- Paths])
    end,
    lists:foreach(
        fun(Time) ->
            Positions = [get_position_helper(Path, Time) || {_BotId, Path} <- Paths],
            UniquePositions = lists:usort(Positions),
            ?assertEqual(length(Positions), length(UniquePositions),
                io_lib:format("Vertex conflict at time ~p: Positions ~p", [Time, Positions]))
        end,
        lists:seq(0, MaxTime - 1)
    ).

%% @doc Checks that at each time step there are no edge conflicts (bots swapping positions).
-spec check_edge_conflicts(Paths :: [bot_path()]) -> ok.
check_edge_conflicts(Paths) ->
    MaxTime = case Paths of
        {error, _} -> 0;
        _ -> lists:max([length(Path) || {_BotId, Path} <- Paths])
    end,
    lists:foreach(
        fun(Time) ->
            lists:foreach(
                fun({BotId1, Path1}) ->
                    lists:foreach(
                        fun({BotId2, Path2}) ->
                            if BotId1 < BotId2 ->
                                    Pos1_T  = get_position_helper(Path1, Time),
                                    Pos1_T1 = get_position_helper(Path1, Time+1),
                                    Pos2_T  = get_position_helper(Path2, Time),
                                    Pos2_T1 = get_position_helper(Path2, Time+1),
                                    ?assertNotEqual({Pos1_T, Pos1_T1}, {Pos2_T1, Pos2_T});
                               true ->
                                    ok
                            end
                        end, Paths)
                end, Paths)
        end,
        lists:seq(0, MaxTime - 2)
    ).

%% @doc Helper: Retrieves the bot's position at a given time.
-spec get_position_helper(Path :: path(), Time :: non_neg_integer()) -> coordinate().
get_position_helper(Path, Time) ->
    if
        Time < length(Path) -> lists:nth(Time + 1, Path);
        true -> lists:last(Path)
    end.
