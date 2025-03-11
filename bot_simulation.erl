-module(bot_simulation).
-export([start/0, start_manual/1, init_db/0]).

%% Record Definitions
-record(bot, {id, start, goal, path}).
-record(bot_path, {bot_id, path}).

%% Type Definitions

-type coordinate() :: {integer(), integer()}.
-type path() :: [coordinate()].
-type time() :: non_neg_integer().
-type bot() :: #bot{}.
-type bot_path() :: {integer(), path()}.
-type manual_positions() :: [{coordinate(), coordinate()}].

%% @doc Starts simulation with randomly generated bots.
-spec start() -> ok.
start() ->
    Seed = {erlang:monotonic_time(), erlang:unique_integer(), erlang:system_time()},
    rand:seed(exs1024, Seed),
    init_db(),
    Bots = generate_bots(5),
    io:format("Generated Bots: ~p~n", [Bots]),
    BotTuples = [{Bot#bot.id, Bot#bot.start, Bot#bot.goal} || Bot <- Bots],
    case cbs:search(BotTuples) of
        {error, no_solution} ->
            io:format("No solution found for given bots.~n");
        Paths ->
            io:format("Computed Paths: ~p~n", [Paths]),
            store_paths(Paths),
            simulate(Paths)
    end.

%% @doc Starts simulation with manually provided bot positions.
-spec start_manual(Positions :: manual_positions()) -> ok | {error, no_valid_bots}.
start_manual(PositionList) ->
    init_db(),
    Bots = manual_generate_bots(PositionList),
    case Bots of
        [] ->
            io:format("No valid bots provided.~n"),
            {error, no_valid_bots};
        _ ->
            io:format("Generated Bots: ~p~n", [Bots]),
            BotTuples = [{Bot#bot.id, Bot#bot.start, Bot#bot.goal} || Bot <- Bots],
            case cbs:search(BotTuples) of
                {error, no_solution} ->
                    io:format("No solution found for given bots.~n");
                Paths ->
                    io:format("Computed Paths: ~p~n", [Paths]),
                    store_paths(Paths),
                    simulate(Paths)
            end
    end.

%% @doc Initializes the Mnesia database (bot_paths table) and grid.
-spec init_db() -> ok.
init_db() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(bot_paths, [{attributes, record_info(fields, bot_path)},
                                    {disc_copies, [node()]}]),
    grid_manager:init_grid(),
    ok.

%% @doc Stores computed bot paths in the Mnesia database.
-spec store_paths(Paths :: [bot_path()]) -> ok.
store_paths(Paths) ->
    Fun = fun() ->
                  lists:foreach(fun({BotId, Path}) ->
                                      mnesia:write(#bot_path{bot_id = BotId, path = Path})
                                end, Paths)
          end,
    mnesia:transaction(Fun).

%% @doc Generates N random bots with unique start and goal positions.
-spec generate_bots(N :: integer()) -> [bot()].
generate_bots(N) ->
    generate_bots(N, []).

generate_bots(0, Acc) ->
    Acc;
generate_bots(N, Acc) ->
    Start = {random_between(1,10), random_between(1,10)},
    Goal = {random_between(1,10), random_between(1,10)},
    case {Start =:= Goal, lists:any(fun(B) -> (B#bot.start =:= Start) orelse (B#bot.goal =:= Goal) end, Acc)} of
        {true, _} -> generate_bots(N, Acc);
        {_, true} -> generate_bots(N, Acc);
        {false, false} ->
            Bot = #bot{id = N, start = Start, goal = Goal, path = []},
            generate_bots(N-1, [Bot|Acc])
    end.

%% @doc Generates a random integer between Low and High (inclusive).
-spec random_between(Low :: integer(), High :: integer()) -> integer().
random_between(Low, High) ->
    rand:uniform(High - Low + 1) + Low - 1.

%% @doc Simulates bot movements using computed paths.
-spec simulate(Paths :: [bot_path()]) -> ok.
simulate(Paths) ->
    TotalTime = lists:max([length(Path) || {_Bot, Path} <- Paths]),
    io:format("Simulation starting. Total time steps: ~p~n", [TotalTime]),
    simulate_steps(Paths, 0, TotalTime).

%% @doc Simulates bot movements step-by-step given Paths, current Time, and TotalTime.
-spec simulate_steps(Paths :: [bot_path()], Time :: time(), TotalTime :: time()) -> ok.
simulate_steps(Paths, Time, TotalTime) when Time >= TotalTime ->
    io:format("Simulation complete. Total time: ~p steps.~n", [TotalTime]),
    print_summary(Paths, TotalTime);
simulate_steps(Paths, Time, TotalTime) ->
    io:format("Time step ~p:~n", [Time]),
    lists:foreach(fun({Bot, Path}) ->
                          Pos = get_position(Path, Time),
                          io:format(" Bot ~p at position ~p~n", [Bot, Pos])
                  end, Paths),
    %% timer:sleep(500),
    simulate_steps(Paths, Time+1, TotalTime).

%% @doc Retrieves the bot's position at the given Time from its Path.
-spec get_position(Path :: path(), Time :: time()) -> coordinate().
get_position(Path, Time) ->
    if
        Time < length(Path) -> lists:nth(Time+1, Path);
        true -> lists:last(Path)
    end.

%% @doc Prints a summary of final bot paths with time intervals.
-spec print_summary(Paths :: [bot_path()], TotalTime :: time()) -> ok.
print_summary(Paths, TotalTime) ->
    io:format("Final Bot Paths (with time intervals):~n"),
    lists:foreach(fun({Bot, Path}) ->
                          Intervals = compress_path(Path),
                          io:format("  Bot ~p: ~p~n", [Bot, Intervals])
                  end, Paths),
    io:format("Overall time taken for all bots to reach destinations: ~p steps.~n", [TotalTime]).

%% @doc Generates bot records from manually provided positions.
-spec manual_generate_bots(Positions :: manual_positions()) -> [bot()].
manual_generate_bots(PositionList) ->
    manual_generate_bots(PositionList, 1, []).

manual_generate_bots([], _Id, Acc) ->
    lists:reverse(Acc);
manual_generate_bots([{Start, Goal} | Rest], Id, Acc) ->
    case {is_valid_position(Start), is_valid_position(Goal)} of
         {true, true} ->
             case lists:any(fun(B) -> (B#bot.start =:= Start) orelse (B#bot.goal =:= Goal) end, Acc) of
                 true ->
                     io:format("Error: Duplicate start or goal for bot ~p: Start ~p, Goal ~p~n", [Id, Start, Goal]),
                     manual_generate_bots(Rest, Id+1, Acc);
                 false ->
                     Bot = #bot{id = Id, start = Start, goal = Goal, path = []},
                     io:format("Bot ~p generated with start ~p and goal ~p~n", [Id, Start, Goal]),
                     manual_generate_bots(Rest, Id+1, [Bot | Acc])
             end;
         _ ->
             io:format("Error: Invalid position for bot ~p: Start ~p, Goal ~p~n", [Id, Start, Goal]),
             manual_generate_bots(Rest, Id+1, Acc)
    end.

%% @doc Checks if a given coordinate is within grid bounds (1 to 10).
-spec is_valid_position(Pos :: coordinate()) -> boolean().
is_valid_position({X, Y}) when is_integer(X), is_integer(Y) ->
    X >= 1 andalso X =< 10 andalso Y >= 1 andalso Y =< 10;
is_valid_position(_) ->
    false.

%% @doc Compresses a bot's path into intervals of the form {Cell, StartTime, EndTime}.
-spec compress_path(Path :: path()) -> [{coordinate(), integer(), integer()}].
compress_path([]) ->
    [];
compress_path([H|T]) ->
    compress_path([H|T], 0).

%% @doc Helper: Compresses consecutive positions into intervals where the bot remains at the same coordinate.
-spec compress_path(Path :: path(), StartTime :: integer()) -> [{coordinate(), integer(), integer()}].
compress_path([], _StartTime) ->
    [];
compress_path([H|T], StartTime) ->
    compress_interval(H, StartTime, T, StartTime, []).

%% @doc Compresses consecutive positions into intervals where the bot remains at the same coordinate.
-spec compress_interval(CurrentCell :: coordinate(), CurrentStart :: integer(), Rest :: path(), CurrentTime :: integer(), Acc :: [{coordinate(), integer(), integer()}]) -> [{coordinate(), integer(), integer()}].
compress_interval(CurrentCell, CurrentStart, [], CurrentTime, Acc) ->
    lists:reverse([{CurrentCell, CurrentStart, CurrentTime} | Acc]);
compress_interval(CurrentCell, CurrentStart, [H|T], CurrentTime, Acc) ->
    NextTime = CurrentTime + 1,
    if
        H =:= CurrentCell ->
            compress_interval(CurrentCell, CurrentStart, T, NextTime, Acc);
        true ->
            compress_interval(H, NextTime, T, NextTime, [{CurrentCell, CurrentStart, CurrentTime} | Acc])
    end.
