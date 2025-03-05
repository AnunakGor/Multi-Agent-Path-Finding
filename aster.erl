-module(aster).
-export([find_path/3]).

find_path(Start, Goal, Constraints) ->
    InitialF = heuristic(Start, Goal),
    Node = {InitialF, 0, Start, [Start]},
    UniqueKey = erlang:unique_integer([monotonic, positive]),
    Open = gb_trees:insert({InitialF, UniqueKey}, Node, gb_trees:empty()),
    Closed = sets:new(),
    search(Open, Closed, Goal, Constraints).

search(Open, Closed, Goal, Constraints) ->
    case gb_trees:is_empty(Open) of
        true ->
            {error, no_path};
        false ->
            {_, { _F, G, Current, Path}, OpenRest} = gb_trees:take_smallest(Open),
            if
                Current =:= Goal ->
                    lists:reverse(Path);
                true ->
                    Time = length(Path) - 1,
                    Neighbors = neighbors(Current),
                    ValidNodes = [ {heuristic(Neighbor, Goal) + (G+1), G+1, Neighbor, [Neighbor | Path]}
                                   || Neighbor <- Neighbors,
                                      is_valid_move(Current, Neighbor, Time, Constraints),
                                      not(sets:is_element({Neighbor, Time+1}, Closed))],
                    OpenNew = insert_nodes(OpenRest, ValidNodes),
                    NewClosed = sets:add_element({Current, Time}, Closed),
                    search(OpenNew, NewClosed, Goal, Constraints)
            end
    end.

insert_nodes(Tree, []) ->
    Tree;
insert_nodes(Tree, [Node | Rest]) ->
    {F, _, _, _} = Node,
    NewKey = {F, erlang:unique_integer([monotonic, positive])},
    Tree1 = gb_trees:insert(NewKey, Node, Tree),
    insert_nodes(Tree1, Rest).

%% manhattan distance heuristic
heuristic({X1, Y1}, {X2, Y2}) ->
    abs(X1 - X2) + abs(Y1 - Y2).

%% neighbors: up, down, left, right, and wait (stay in place)
neighbors({X, Y}) ->
    Potential = [{X, Y}, {X+1, Y}, {X-1, Y}, {X, Y+1}, {X, Y-1}],
    [ {NX, NY} || {NX, NY} <- Potential, NX >= 1, NX =< 10, NY >= 1, NY =< 10].

is_valid_move(Current, Next, Time, Constraints) ->
    VertexOk = not lists:any(fun({T, Pos}) ->
                                  T =:= Time+1 andalso Pos =:= Next
                              end, Constraints),
    EdgeOk = not lists:any(fun({T, {From, To}}) ->
                                T =:= Time+1 andalso From =:= Current andalso To =:= Next
                            end, Constraints),
    VertexOk andalso EdgeOk.

