-module(pathfinding_test).
-export([run_tests/0]).
% -compile(export_all).

%% Entry point to run all tests
run_tests() ->
    io:format("Starting pathfinding tests~n~n"),
    test_same_start_goal(),
    test_swap_conflict(),
    test_vertex_conflict(),
    test_multiple_bots(),
    test_boundary_bots(),
    test_extreme_edge(),
    io:format("Pathfinding tests completed~n").

%% Test 1: Single bot with identical start and goal.
test_same_start_goal() ->
    io:format("Test 1: Bot with same start and goal~n"),
    %% This bot will have a trivial path (staying in place).
    bot_simulation:start_manual([{ {5,5}, {5,5} }]),
    io:format("Test 1 completed~n~n").

%% Test 2: Two bots that try to swap positions.
test_swap_conflict() ->
    io:format("Test 2: Swap conflict between two bots~n"),
    %% Bot 1: from {1,1} to {10,10}
    %% Bot 2: from {10,10} to {1,1}
    bot_simulation:start_manual([{ {1,1}, {10,10} }, { {10,10}, {1,1} }]),
    io:format("Test 2 completed~n~n").

%% Test 3: Two bots that potentially conflict at a vertex.
test_vertex_conflict() ->
    io:format("Test 3: Vertex conflict between two bots~n"),
    %% Both bots aim for the same destination cell at roughly the same time.
    bot_simulation:start_manual([{ {1,1}, {5,5} }, { {1,2}, {5,5} }]),
    io:format("Test 3 completed~n~n").

%% Test 4: Multiple bots with a mix of trajectories.
test_multiple_bots() ->
    io:format("Test 4: Multiple bots with potential conflicts~n"),
    %% A set of five bots with diverse paths.
    Bots = [ { {1,1}, {2,10} },
             { {2,10}, {1,1} },
             { {1,10}, {10,1} },
             { {5,5}, {6,6} },
             { {2,2}, {9,9} } ],
    bot_simulation:start_manual(Bots),
    io:format("Test 4 completed~n~n").

%% Test 5: Bots placed along grid boundaries.
test_boundary_bots() ->
    io:format("Test 5: Bots along grid boundaries~n"),
    %% Bots that start or end at the corners/edges of the grid.
    Bots = [ { {1,1}, {1,10} },
             { {1,10}, {10,10} },
             { {10,10}, {10,1} },
             { {10,1}, {1,1} } ],
    bot_simulation:start_manual(Bots),
    io:format("Test 5 completed~n~n").
%% Test 6: Extreme edge case with heavy symmetric conflicts.
test_extreme_edge() ->
    io:format("Test 6: Extreme edge case with heavy symmetric conflicts~n"),
    %% This test creates multiple symmetric conflicts:
    %% - Bots 1 & 2 traverse diagonally in opposite directions.
    %% - Bots 3 & 4 traverse the other diagonal.
    %% - Bot 5 crosses vertically through the center.
    bot_simulation:start_manual([
        {{1,1}, {10,10}},
        {{10,10}, {1,1}},
        {{1,10}, {10,1}},
        {{10,1}, {1,10}},
        {{5,1}, {5,10}}
    ]),
    io:format("Test 6 completed~n~n").
