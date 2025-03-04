-module(bot_simulation).
-export([start/0, start_manual/1]).

-record(bot, {id, start, goal, path}).
-record(bot_path, {bot_id, path}).

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

start_manual(BotTuples) ->
    %% BotTuples should be a list of tuples of Form {BotId, Start, Goal}
    init_db(),
    io:format("Using manual bot coordinates: ~p~n", [BotTuples]),
    case cbs:search(BotTuples) of
        {error, no_solution} ->
            io:format("No solution found for the provided bots.~n");
        Paths ->
            io:format("Computed Paths: ~p~n", [Paths]),
            store_paths(Paths),
            simulate(Paths)
    end.

init_db() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(bot_paths,
        [{attributes, record_info(fields, bot_path)},
         {disc_copies, [node()]}]).

store_paths(Paths) ->
    Fun = fun() ->
                  lists:foreach(
                      fun({BotId, Path}) ->
                          mnesia:write(#bot_path{bot_id = BotId, path = Path})
                      end,
                      Paths)
          end,
    mnesia:transaction(Fun).

generate_bots(N) ->
    generate_bots(N, []).

generate_bots(0, Acc) ->
    Acc;
generate_bots(N, Acc) ->
    Start = {random_between(1,10), random_between(1,10)},
    Goal = {random_between(1,10), random_between(1,10)},
    case {Start =:= Goal,
          lists:any(fun(B) ->
                        (B#bot.start =:= Start) orelse (B#bot.goal =:= Start) orelse
                        (B#bot.start =:= Goal) orelse (B#bot.goal =:= Goal)
                    end, Acc)} of
        {true, _} ->
            generate_bots(N, Acc);
        {_, true} ->
            generate_bots(N, Acc);
        {false, false} ->
            Bot = #bot{id = N, start = Start, goal = Goal, path = []},
            generate_bots(N-1, [Bot|Acc])
    end.

random_between(Low, High) ->
    rand:uniform(High - Low + 1) + Low - 1.

simulate(Paths) ->
    TotalTime = lists:max([length(Path) || {_Bot, Path} <- Paths]),
    io:format("Simulation starting. Total time steps: ~p~n", [TotalTime]),
    simulate_steps(Paths, 0, TotalTime).

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

get_position(Path, Time) ->
    if
        Time < length(Path) -> lists:nth(Time+1, Path);
        true -> lists:last(Path)
    end.

print_summary(Paths, TotalTime) ->
    io:format("Final Bot Paths:~n"),
    lists:foreach(fun({Bot, Path}) ->
                          io:format("  Bot ~p: ~p~n", [Bot, Path])
                  end, Paths),
    io:format("Overall time taken for all bots to reach destinations: ~p steps.~n", [TotalTime]).
