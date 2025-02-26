-module(main).

-export([main/0]).


-record(bot, {id, start, goal, path = []}).
-record(reservation, {time, pos, bot_id}).

main() ->
    init_mnesia(),
    
    %% Starting positions and Ending positions
    Bots = [
        #bot{id = 1, start = {2,1}, goal = {0,2}},
        #bot{id = 2, start = {2,2}, goal = {1,2}},
        #bot{id = 3, start = {1,0}, goal = {2,0}},
        #bot{id = 4, start = {1,1}, goal = {0,0}}
        % #bot{id = 5, start = {2,1}, goal = {2,2}},
    ],
    % Bots = [
    %     #bot{id = 1, start = {2,1}, goal = {2,2}},
    %     #bot{id = 2, start = {0,0}, goal = {2,0}}
    % ],
    io:format("Starting bots: ~p~n", [Bots]),

    Solution = solve_cbs(Bots),
    io:format("Computed solution paths: ~p~n", [Solution]),

    simulate(Solution),
    ok.

%%Initialization of mnesia db
init_mnesia() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    case mnesia:create_table(reservation, [
            {attributes, record_info(fields,reservation)},
            {disc_copies, [node()]}
         ]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, _}} -> ok
    end.

%% cbs 
solve_cbs(Bots) ->
    RootConstraints = [],
    RootSolution = [ {Bot#bot.id, 
                        case a_star(Bot, RootConstraints) of 
                            {error, no_path} -> []; 
                            Path -> Path 
                        end} 
                      || Bot <- Bots],
    RootCost = makespan(RootSolution),
    RootNode = {RootConstraints, RootSolution, RootCost},
    % io:format("DEBUG: Root node: Constraints=~p, Solution=~p, Cost=~p~n", [RootConstraints, RootSolution, RootCost]),
    cbs([RootNode], Bots).

%% checking for conflicts in the paths.
cbs([], _Bots) -> 
    io:format("DEBUG: No solution found~n"), [];
cbs(Nodes, Bots) -> 
    SortedNodes = lists:keysort(3, Nodes),
    {Constraints, Solution, Cost} = hd(SortedNodes),
    % io:format("DEBUG: Processing node with Cost=~p, Constraints=~p~n", [Cost, Constraints]),
    case detect_conflict(Solution) of
        none -> 
            % io:format("DEBUG: No conflict detected. Solution found!~n", []),
            Solution;
        Conflict -> 
            % io:format("DEBUG: Conflict detected: ~p~n", [Conflict]),
            NewNodes = branch({Constraints, Solution, makespan(Solution)}, Conflict, Bots),
            % io:format("DEBUG: Branching resulted in ~p new nodes~n", [length(NewNodes)]),
            RemainingNodes = tl(SortedNodes),
            cbs(NewNodes ++ RemainingNodes, Bots)
    end.

detect_conflict(Solution) -> 
    T_max = makespan(Solution),
    detect_conflict_at_time(Solution, 0, T_max).

detect_conflict_at_time(_Solution, T, T_max) when T > T_max -> 
    none;
detect_conflict_at_time(Solution, T, T_max) -> 
    Positions = [{BotId, get_pos(Path, T)} || {BotId, Path} <- Solution],
    case find_vertex_conflict(Positions) of
         {Bot1, Bot2, Pos} -> 
            %  io:format("DEBUG: Vertex conflict at time ~p between Bot ~p and Bot ~p at Pos ~p~n", [T, Bot1, Bot2, Pos]),
             {vertex, Bot1, Bot2, T, Pos};
         none -> 
             case find_edge_conflict(Solution, T) of
                  {Bot1, Bot2, From1, To1, From2, To2} -> 
                    %   io:format("DEBUG: Edge conflict at time ~p between Bot ~p and Bot ~p (from ~p->~p and ~p->~p)~n", 
                                % [T, Bot1, Bot2, From1, To1, From2, To2]),
                      {edge, Bot1, Bot2, T, From1, To1, From2, To2};
                  none -> 
                      detect_conflict_at_time(Solution, T+1,T_max)
             end
    end.

find_vertex_conflict(Positions) -> 
    find_vertex_conflict_loop(Positions).

find_vertex_conflict_loop([]) -> 
    none;
find_vertex_conflict_loop([{BotId, Pos} | Rest]) -> 
    case lists:filter(fun({_OtherBotId, OtherPos}) -> OtherPos =:= Pos end, Rest) of
         [] -> find_vertex_conflict_loop(Rest);
         [{OtherBotId, _} | _] -> {BotId, OtherBotId, Pos}
    end.

find_edge_conflict(Solution, T) -> 
    Moves = [{BotId, get_pos(Path, T), get_pos(Path, T+1)} || {BotId, Path} <- Solution],
    find_edge_conflict_loop(Moves).

find_edge_conflict_loop([]) -> 
    none;
find_edge_conflict_loop([{BotId, From, To} | Rest]) -> 
    case lists:foldl(fun({OtherBotId, OtherFrom, OtherTo}, Acc) -> 
         case Acc of
             none -> 
                 if From =:= OtherTo andalso To =:= OtherFrom -> 
                         {BotId, OtherBotId, From, To,OtherFrom, OtherTo};
                    true -> 
                         none
                 end;
             Conflict -> 
                 Conflict
         end
    end, none, Rest) of
         none -> find_edge_conflict_loop(Rest);
         Conflict -> Conflict
    end.

get_pos(Path, T) -> 
    if T < length(Path) -> 
           lists:nth(T+1, Path);
       true -> 
           lists:last(Path)
    end.

makespan(Solution) -> 
    lists:max([length(Path) || {_BotId, Path} <- Solution]).

%% branching (adding Constraints)
branch(Node, {vertex, Bot1, Bot2, T, Pos}, Bots) -> 
    % io:format("DEBUG: Branching on vertex conflict at time ~p for Bots ~p and ~p at Pos ~p~n", [T, Bot1, Bot2, Pos]),
    Nodes1 =branch_for_bot(Node, Bot1, {vertex, T, Pos}, Bots),
    Nodes2 = branch_for_bot(Node, Bot2, {vertex, T, Pos}, Bots),
    Nodes1 ++ Nodes2;
branch(Node, {edge, Bot1, Bot2, T, From1, To1, From2, To2}, Bots) -> 
    % io:format("DEBUG: Branching on edge conflict at time ~p for Bots ~p and ~p~n", [T, Bot1, Bot2]),
    Nodes1 = branch_for_bot(Node, Bot1, {edge, T, From1, To1}, Bots),
    Nodes2 = branch_for_bot(Node, Bot2, {edge, T, From2, To2}, Bots),
    Nodes1 ++ Nodes2.

branch_for_bot({Constraints, Solution, _Cost}, BotId, {vertex, T, Pos}, Bots) -> 
    NewConstraint = {BotId, vertex, T, Pos},
    % io:format("DEBUG: Adding vertex constraint for Bot ~p: time ~p, pos ~p~n", [BotId, T, Pos]),
    NewConstraints = Constraints ++ [NewConstraint],
    Bot = find_bot(BotId, Bots),
    case a_star(Bot, NewConstraints) of
         {error, no_path} -> 
            %  io:format("DEBUG:No path found for Bot ~p with new constraint ~p~n", [BotId, NewConstraint]),
             [];
         NewPath -> 
            %  io:format("DEBUG: New path for Bot ~p: ~p~n", [BotId, NewPath]),
             NewSolution = update_solution(Solution, BotId, NewPath),
             NewCost = makespan(NewSolution),
             [{NewConstraints, NewSolution, NewCost}]
    end;
branch_for_bot({Constraints, Solution, _Cost}, BotId, {edge, T, From, To}, Bots) -> 
    NewConstraint = {BotId, edge, T, From, To},
    % io:format("DEBUG: Adding edge constraint for Bot ~p: time ~p, from ~p to ~p~n", [BotId, T, From, To]),
    NewConstraints = Constraints ++ [NewConstraint],
    Bot = find_bot(BotId, Bots),
    case a_star(Bot, NewConstraints) of
         {error, no_path} -> 
            %  io:format("DEBUG: No path found for Bot ~p with new constraint ~p~n", [BotId, NewConstraint]),
             [];
         NewPath -> 
            %  io:format("DEBUG: New path for Bot ~p: ~p~n", [BotId, NewPath]),
             NewSolution = update_solution(Solution, BotId, NewPath),
             NewCost = makespan(NewSolution),
             [{NewConstraints, NewSolution, NewCost}]
    end.

find_bot(BotId, Bots) -> 
    case lists:filter(fun(B) -> B#bot.id =:= BotId end, Bots) of
         [Bot|_] -> Bot;
         [] -> undefined
    end.

update_solution([], _BotId, _NewPath) -> 
    [];
update_solution([{BotId, _} | Rest], BotId, NewPath) -> 
    [{BotId, NewPath} | Rest];
update_solution([H | Rest], BotId, NewPath) -> 
    [H | update_solution(Rest, BotId, NewPath)].

%% A* Search
a_star(Bot, Constraints) -> 
    Start = Bot#bot.start,
    Goal = Bot#bot.goal,
    % io:format("DEBUG: Starting A* for Bot ~p from ~p to ~p with constraints ~p~n",
    %           [Bot#bot.id, Start, Goal, Constraints]),
    OpenList = [{Start, 0, manhattan(Start, Goal), [Start]}],
    Closed = [],
    search(OpenList, Closed, Bot, Constraints, Goal).

search([], _Closed, _Bot, _Constraints, _Goal) -> 
    {error, no_path};
search(OpenList, Closed, Bot, Constraints, Goal) -> 
    SortedOpen = lists:keysort(3, OpenList),
    {Current, G, F, Path} = hd(SortedOpen),
    % io:format("DEBUG: A* expanding node: ~p, G=~p, F=~p, Path=~p~n",
            %   [Current, G, F, lists:reverse(Path)]),
    RestOpen = tl(SortedOpen),
    if
        Current =:= Goal -> 
            % io:format("DEBUG: A* reached goal for Bot ~p. Final path: ~p~n",
                    %   [Bot#bot.id, lists:reverse(Path)]),
            lists:reverse(Path);
        true -> 
            NextNodes = expand(Current, G, Path, Bot, Constraints, Goal),
            NewOpen = insert_all(RestOpen, NextNodes),
            search(NewOpen, [Current | Closed], Bot, Constraints, Goal)
    end.

expand(Current, G, Path, Bot, Constraints, Goal) -> 
    NextTime = G + 1,
    Neighbors = get_neighbors(Current),
    ValidNeighbors = [ N || N <- Neighbors, valid_move(Bot#bot.id, Current, N, NextTime, Constraints) ],
    % io:format("DEBUG: Expanding from ~p. Valid neighbors: ~p~n", [Current, ValidNeighbors]),
    [ {N, NextTime, NextTime + manhattan(N, Goal), [N|Path]} || N <- ValidNeighbors ].

get_neighbors({X, Y}) -> 
    Moves = [{X, Y}, {X+1, Y}, {X-1, Y}, {X, Y+1}, {X, Y-1}],
    [ P || P <- Moves, valid_position(P) ].
% currently this is working for smaller grids only
valid_position({X, Y}) when X >= 0, X < 3, Y >= 0, Y < 3 -> 
    true;
valid_position(_) -> 
    false.

valid_move(BotId, From, To, Time, Constraints) -> 
    Valid = not lists:any(fun(Constraint) -> 
        case Constraint of
            {BotId, vertex, Time, Pos} when Pos =:= To -> true;
            {BotId, edge, Time, From1, To1} when From1 =:= From, To1 =:= To -> true;
            _ -> false
        end
    end, Constraints),
    if 
      Valid -> true;
      true -> 
        % io:format("DEBUG: Move for Bot ~p from ~p to ~p at time ~p violates constraint ~p~n", 
        %           [BotId, From, To, Time, Constraints]),
        false
    end.

insert_all(Open, Nodes) -> 
    lists:foldl(fun(Node, Acc) -> insert_sorted(Acc, Node) end, Open, Nodes).

insert_sorted([], Node) -> 
    [Node];
insert_sorted([H = {_Pos, _G, F1, _Path} | T], Node = {_Pos2, _G2, F2, _Path2}) -> 
    if
        F2 =< F1 -> 
            [Node, H | T];
        true -> 
            [H | insert_sorted(T, Node)]
    end.

manhattan({X1, Y1}, {X2, Y2}) -> 
    abs(X1 - X2) + abs(Y1 - Y2).

%% simulation and reservation Storage in Mnesia
simulate(Solution) -> 
    T_max = makespan(Solution),
    io:format("Simulating bot movements up to time ~p~n", [T_max]),
    lists:foreach(fun(T) -> 
        io:format("Time ~p:~n", [T]),
        lists:foreach(fun({BotId, Path}) -> 
            Pos = get_pos(Path, T),
            store_reservation(T, Pos, BotId),
            io:format("Bot ~p at position ~p~n", [BotId, Pos])
        end, Solution)
    end, lists:seq(0, T_max)).


store_reservation(Time, Pos, BotId) ->
    Fun = fun() ->
              mnesia:write(#reservation{time = Time, pos = Pos, bot_id = BotId})
          end,
    mnesia:transaction(Fun).