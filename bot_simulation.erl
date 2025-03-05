-module(bot_simulation).
-export([start/0, start_manual/1]).

-record(bot, {id, start, goal, path}).
-record(bot_path, {bot_id, path}).

%% @doc Starts the bot simulation with randomly generated bots.
%% It initializes the database, generates bots, and finds paths using CBS.
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

%% @doc Starts the simulation with manually provided bot positions.
-spec start_manual([{ {integer(), integer()}, {integer(), integer()} }]) -> ok | {error, no_valid_bots}.
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

%% @doc Initializes the Mnesia database schema and starts the database.
-spec init_db() -> ok.
init_db() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(bot_paths,
        [{attributes, record_info(fields, bot_path)},
         {disc_copies, [node()]}]).

%% @doc Stores bot paths in the Mnesia database.
-spec store_paths([{integer(), [{integer(), integer()}]}]) -> ok.
store_paths(Paths) ->
    Fun = fun() ->
                  lists:foreach(
                      fun({BotId, Path}) ->
                          mnesia:write(#bot_path{bot_id = BotId, path = Path})
                      end,
                      Paths)
          end,
    mnesia:transaction(Fun).

%% @doc Generates `N` random bots with unique start and goal positions.
-spec generate_bots(integer()) -> [#bot{}].
generate_bots(N) ->
    generate_bots(N, []).

generate_bots(0, Acc) ->
    Acc;
generate_bots(N, Acc) ->
    Start = {random_between(1,10), random_between(1,10)},
    Goal = {random_between(1,10), random_between(1,10)},
    case {Start =:= Goal,
          lists:any(fun(B) ->
                        (B#bot.start =:= Start) orelse (B#bot.goal =:= Goal)
                    end, Acc)} of
        %   lists:any(fun(B) ->
        %                 (B#bot.start =:= Start) orelse (B#bot.goal =:= Start) orelse
        %                 (B#bot.start =:= Goal) orelse (B#bot.goal =:= Goal)
        %             end, Acc)} of
        {true, _} ->
            generate_bots(N, Acc);
        {_, true} ->
            generate_bots(N, Acc);
        {false, false} ->
            Bot = #bot{id = N, start = Start, goal = Goal, path = []},
            generate_bots(N-1, [Bot|Acc])
    end.

%% @doc Generates a random integer between `Low` and `High` (inclusive).
-spec random_between(integer(), integer()) -> integer().
random_between(Low, High) ->
    rand:uniform(High - Low + 1) + Low - 1.

%% @doc Simulates bot movements over time.
-spec simulate([{integer(), [{integer(), integer()}]}]) -> ok.
simulate(Paths) ->
    TotalTime = lists:max([length(Path) || {_Bot, Path} <- Paths]),
    io:format("Simulation starting. Total time steps: ~p~n", [TotalTime]),
    simulate_steps(Paths, 0, TotalTime).

%% @doc Simulates step-by-step movements of bots.
-spec simulate_steps([{integer(), [{integer(), integer()}]}], integer(), integer()) -> ok.
simulate_steps(Paths, Time, TotalTime) when Time >= TotalTime ->
    io:format("Simulation complete. Total time: ~p steps.~n", [TotalTime]),
    print_summary(Paths, TotalTime);
simulate_steps(Paths, Time, TotalTime) ->
    io:format("Time step ~p:~n", [Time]),
    lists:foreach(fun({Bot, Path}) ->
                          Pos = get_position(Path, Time),
                          io:format(" Bot ~p at position ~p~n", [Bot, Pos])
                  end, Paths),
    % timer:sleep(500),  
    simulate_steps(Paths, Time+1, TotalTime).

%% @doc Retrieves the bot's position at a given time step.
-spec get_position([{integer(), integer()}], integer()) -> {integer(), integer()}.
get_position(Path, Time) ->
    if
        Time < length(Path) -> lists:nth(Time+1, Path);
        true -> lists:last(Path)
    end.

%% @doc Prints the final summary of bot movements.
-spec print_summary([{integer(), [{integer(), integer()}]}], integer()) -> ok.
print_summary(Paths, TotalTime) ->
    io:format("Final Bot Paths (with time intervals):~n"),
    lists:foreach(fun({Bot, Path}) ->
                          Intervals = compress_path(Path),
                          io:format("  Bot ~p: ~p~n", [Bot, Intervals])
                  end, Paths),
    io:format("Overall time taken for all bots to reach destinations: ~p steps.~n", [TotalTime]).

%% @doc Generates bot records from manually provided positions.
-spec manual_generate_bots([{ {integer(), integer()}, {integer(), integer()} }]) -> [#bot{}].
manual_generate_bots(PositionList) ->
    manual_generate_bots(PositionList, 1, []).

manual_generate_bots([], _Id, Acc) ->
    lists:reverse(Acc);
manual_generate_bots([{Start, Goal} | Rest], Id, Acc) ->
    case {is_valid_position(Start), is_valid_position(Goal)} of
         {true, true} ->
             case lists:any(fun(B) ->
                             (B#bot.start =:= Start) orelse (B#bot.goal =:= Goal)
                           end, Acc) of
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

%% @doc Checks if a given position `{X,Y}` is within grid bounds.
-spec is_valid_position({integer(), integer()}) -> boolean().
is_valid_position({X, Y}) when is_integer(X), is_integer(Y) ->
    X >= 1 andalso X =< 10 andalso Y >= 1 andalso Y =< 10;
is_valid_position(_) ->
    false.


%% @doc Compresses a bot's path into a list of intervals `{Cell, StartTime, EndTime}`.
-spec compress_path([{integer(), integer()}]) -> [{ {integer(), integer()}, integer(), integer() }].
compress_path(Path) ->
    compress_path(Path, 0).

compress_path([H|T], StartTime) ->
    compress_interval(H, StartTime, T, StartTime, []).

%% @doc Compresses a sequence of positions into intervals where the bot stays at the same position.
-spec compress_interval({integer(), integer()}, integer(), [{integer(), integer()}], integer(), [{ {integer(), integer()}, integer(), integer() }]) ->
    [{ {integer(), integer()}, integer(), integer() }].
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
