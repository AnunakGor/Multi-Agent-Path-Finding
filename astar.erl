-module(astar).
-export([find_path/3]).

%% Type Definitions

-type coordinate() :: {integer(), integer()}.
-type path() :: [coordinate()].
-type time() :: non_neg_integer().
-type vertex_constraint() :: {time(), coordinate()}.
-type edge_constraint() :: {time(), {coordinate(), coordinate()}}.
-type constraint() :: vertex_constraint() | edge_constraint().
-type constraints() :: [constraint()].
-type a_node() :: {F :: integer(), G :: integer(), coordinate(), path()}.


%% @doc Finds a path from Start to Goal considering Constraints (list of vertex or edge constraints).
-spec find_path(Start :: coordinate(), Goal :: coordinate(), Constraints :: constraints()) -> path() | {error, no_path}.
find_path(Start, Goal, Constraints) ->
    InitialF = heuristic(Start, Goal),
    Node = {InitialF, 0, Start, [Start]},
    UniqueKey = erlang:unique_integer([monotonic, positive]),
    Open = gb_trees:insert({InitialF, UniqueKey}, Node, gb_trees:empty()),
    Closed = sets:new(),
    search(Open, Closed, Goal, Constraints).

%% @doc Performs the A* search; Open is a gb_trees tree of nodes, Closed is a set of visited {coordinate, time} tuples.
-spec search(Open :: gb_trees:tree(), Closed :: sets:set(), Goal :: coordinate(), Constraints :: constraints()) -> path() | {error, no_path}.
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
                    Neighbors = grid_manager:get_neighbors(Current),
                    ValidNodes = [ {heuristic(Neighbor, Goal) + (G+1), G+1, Neighbor, [Neighbor | Path]}
                                   || Neighbor <- Neighbors,
                                      is_valid_move(Current, Neighbor, Time, Constraints),
                                      not(sets:is_element({Neighbor, Time+1}, Closed)) ],
                    OpenNew = insert_nodes(OpenRest, ValidNodes),
                    NewClosed = sets:add_element({Current, Time}, Closed),
                    search(OpenNew, NewClosed, Goal, Constraints)
            end
    end.

%% @doc Inserts a list of A* nodes into the Open set (gb_trees tree).
-spec insert_nodes(Tree :: gb_trees:tree(), Nodes :: [a_node()]) -> gb_trees:tree().
insert_nodes(Tree, []) ->
    Tree;
insert_nodes(Tree, [Node | Rest]) ->
    {F, _, _, _} = Node,
    NewKey = {F, erlang:unique_integer([monotonic, positive])},
    Tree1 = gb_trees:insert(NewKey, Node, Tree),
    insert_nodes(Tree1, Rest).

%% @doc Computes the Manhattan distance between Pos1 and Pos2.
-spec heuristic(Pos1 :: coordinate(), Pos2 :: coordinate()) -> integer().
heuristic({X1, Y1}, {X2, Y2}) ->
    abs(X1 - X2) + abs(Y1 - Y2).

%% @doc Checks if moving from Current to Next at time Time+1 is valid based on Constraints.
-spec is_valid_move(Current :: coordinate(), Next :: coordinate(), Time :: time(), Constraints :: constraints()) -> boolean().
is_valid_move(Current, Next, Time, Constraints) ->
    VertexOk = not lists:any(fun({T, Pos}) -> T =:= Time+1 andalso Pos =:= Next end, Constraints),
    EdgeOk = not lists:any(fun({T, {From, To}}) -> T =:= Time+1 andalso From =:= Current andalso To =:= Next end, Constraints),
    VertexOk andalso EdgeOk.
