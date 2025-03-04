
-module(aster).
-export([find_path/3]).

find_path(Start, Goal, Constraints) ->
    Open = [{heuristic(Start, Goal), 0, Start, [Start]}],
    Closed = sets:new(),
    search(Open, Closed, Goal, Constraints).

search([], _Closed, _Goal, _Constraints) ->
    {error, no_path};
search(Open, Closed, Goal, Constraints) ->
    {_, G, Current, Path} = hd(Open),
    RestOpen = tl(Open),
    if
        Current =:= Goal ->
            lists:reverse(Path);  
        true ->
            Time = length(Path) - 1,
            Neighbors = neighbors(Current),
            ValidNeighbors = [
                {Neighbor, G + 1} ||
                    Neighbor <- Neighbors,
                    is_valid_move(Current, Neighbor, Time, Constraints)
            ],
            NewNodes = [
                {heuristic(Neighbor, Goal) + NewG, NewG, Neighbor, [Neighbor | Path]}
                || {Neighbor, NewG} <- ValidNeighbors,
                   not(sets:is_element({Neighbor, Time+1}, Closed))
            ],
            UpdatedOpen = insert_nodes(RestOpen, NewNodes),
            NewClosed = sets:add_element({Current, Time}, Closed),
            search(UpdatedOpen, NewClosed, Goal, Constraints)
    end.

%% manhattan distance 
heuristic({X1, Y1}, {X2, Y2}) ->
    abs(X1 - X2) + abs(Y1 - Y2).

%% neighbors: up, down, left, right, and wait (stay in place)
neighbors({X, Y}) ->
    Potential = [{X, Y}, {X+1, Y}, {X-1, Y}, {X, Y+1}, {X, Y-1}],
    [ {NX, NY} || {NX, NY} <- Potential, NX >= 1, NX =< 10, NY >= 1, NY =< 10].

is_valid_move(Current, Next, Time, Constraints) ->
    VertexOk = not lists:any(fun({T, Pos}) -> T =:= Time+1 andalso Pos =:= Next end, Constraints),
    EdgeOk = not lists:any(fun({T, {From, To}}) ->
                                T =:= Time+1 andalso From =:= Current andalso To =:= Next
                            end, Constraints),
    VertexOk andalso EdgeOk.

insert_nodes(Open, NewNodes) ->
    AllNodes = Open ++ NewNodes,
    lists:sort(fun({F1,_,_,_}, {F2,_,_,_}) -> F1 =< F2 end, AllNodes).
