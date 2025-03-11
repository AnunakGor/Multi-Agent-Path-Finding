-module(cbs).
-export([search/1]).

-record(node, {constraints = [], paths = [], cost = 0}).

%% Type Definitions

-type coordinate() :: {integer(), integer()}.
-type path() :: [coordinate()].
-type time() :: non_neg_integer().
-type bot() :: {integer(), coordinate(), coordinate()}.
-type bot_path() :: {integer(), path()}.
-type cbs_constraint() :: {time(), coordinate()} | {time(), {coordinate(), coordinate()}}.
-type cbs_constraints() :: [cbs_constraint()].
-type conflict() :: {conflict, integer(), integer(), time(), coordinate() | none, {coordinate(), coordinate()} | none}.
-type cbs_node() :: #node{constraints :: cbs_constraints(), paths :: [bot_path()], cost :: integer()}.


%% @doc Initiates the CBS algorithm for multiple bots; Bots is a list of {BotId, Start, Goal} tuples.
-spec search(Bots :: [bot()]) -> {error, no_solution} | [bot_path()].
search(Bots) ->
    InitialConstraints = [],
    InitialPaths = [{BotId, aster:find_path(Start, Goal, InitialConstraints)}
                     || {BotId, Start, Goal} <- Bots],
    Root = #node{constraints = InitialConstraints, paths = InitialPaths, cost = compute_cost(InitialPaths)},
    CBSOpen = [Root],
    cbs_open_search(CBSOpen, Bots).

%% @doc Main CBS loop; processes nodes to resolve conflicts; Bots is a list of {BotId, Start, Goal} tuples.
-spec cbs_open_search(Open :: [cbs_node()], Bots :: [bot()]) -> {error, no_solution} | [bot_path()].
cbs_open_search([], _Bots) ->
    {error, no_solution};
cbs_open_search([Node|Rest], Bots) ->
    case validate_paths(Node#node.paths) of
        ok ->
            Node#node.paths;
        {conflict, BotA, BotB, Time, Pos, EdgeConflict} ->
            NewConstraintsA = Node#node.constraints ++ make_constraint(BotA, Time, Pos, EdgeConflict),
            NewConstraintsB = Node#node.constraints ++ make_constraint(BotB, Time, Pos, EdgeConflict),
            NodeA = replan(Node, BotA, NewConstraintsA, Bots),
            NodeB = replan(Node, BotB, NewConstraintsB, Bots),
            NewNodes = lists:filter(fun(X) -> X =/= {error, no_path} end, [NodeA, NodeB]),
            NewOpen = insert_cbs_nodes(Rest ++ NewNodes),
            cbs_open_search(NewOpen, Bots)
    end.

%% @doc Validates paths for conflicts; returns ok if no conflict, else a conflict tuple.
-spec validate_paths(Paths :: [bot_path()]) -> ok | conflict().
validate_paths(Paths) ->
    case find_conflict(Paths, 0) of
        none -> ok;
        Conflict -> Conflict
    end.

%% @doc Finds conflicts (vertex or edge) between paths; returns none if no conflict, else a conflict tuple.
-spec find_conflict(Paths :: [bot_path()], Time :: time()) -> none | conflict().
find_conflict(Paths, Time) ->
    MaxTime = lists:max([length(Path) || {_Bot, Path} <- Paths]),
    find_conflict(Paths, Time, MaxTime).

find_conflict(_Paths, Time, MaxTime) when Time >= MaxTime ->
    none;
find_conflict(Paths, Time, MaxTime) ->
    Positions = [{Bot, get_position(Path, Time)} || {Bot, Path} <- Paths],
    case find_vertex_conflict(Positions) of
        {BotA, BotB, Pos} ->
            {conflict, BotA, BotB, Time, Pos, none};
        none ->
            case find_edge_conflict(Paths, Time) of
                {BotA, BotB, From, To} ->
                    {conflict, BotA, BotB, Time+1, To, {From, To}};
                none ->
                    find_conflict(Paths, Time+1, MaxTime)
            end
    end.

%% @doc Retrieves a bot's position at the given Time; Path is a list of coordinates.
-spec get_position(Path :: path(), Time :: time()) -> coordinate().
get_position(Path, Time) ->
    if
        Time < length(Path) -> lists:nth(Time+1, Path);
        true -> lists:last(Path)
    end.

%% @doc Checks for vertex conflicts (same position at the same time) among bot positions.
-spec find_vertex_conflict(Positions :: [{integer(), coordinate()}]) -> none | {integer(), integer(), coordinate()}.
find_vertex_conflict([]) -> none;
find_vertex_conflict([{Bot, Pos} | Rest]) ->
    case lists:filter(fun({OtherBot, OtherPos}) -> OtherPos =:= Pos andalso OtherBot =/= Bot end, Rest) of
        [] -> find_vertex_conflict(Rest);
        [{OtherBot, _} | _] -> {Bot, OtherBot, Pos}
    end.

%% @doc Finds edge conflicts where two bots swap positions; Paths is a list of {BotId, Path} tuples.
-spec find_edge_conflict(Paths :: [bot_path()], Time :: time()) -> none | {integer(), integer(), coordinate(), coordinate()}.
find_edge_conflict(Paths, Time) ->
    Pairs = [{A, B} || {A, _} <- Paths, {B, _} <- Paths, A < B],
    find_edge_conflict(Pairs, Paths, Time).

find_edge_conflict([], _Paths, _Time) ->
    none;
find_edge_conflict([{BotA, BotB} | Rest], Paths, Time) ->
    PathA = get_path(BotA, Paths),
    PathB = get_path(BotB, Paths),
    PosA_T = get_position(PathA, Time),
    PosA_T1 = get_position(PathA, Time+1),
    PosB_T = get_position(PathB, Time),
    PosB_T1 = get_position(PathB, Time+1),
    if
        PosA_T =:= PosB_T1 andalso PosB_T =:= PosA_T1 ->
            {BotA, BotB, PosA_T, PosA_T1};
        true ->
            find_edge_conflict(Rest, Paths, Time)
    end.

%% @doc Retrieves the path of a bot with BotId from the list of bot paths.
-spec get_path(BotId :: integer(), Paths :: [bot_path()]) -> path().
get_path(Bot, Paths) ->
    case lists:keyfind(Bot, 1, Paths) of
        false -> [];
        {Bot, Path} -> Path
    end.

%% @doc Generates constraints based on detected conflicts; EdgeConflict is either a tuple {From, To} or none.
-spec make_constraint(Bot :: integer(), Time :: time(), Pos :: coordinate(), EdgeConflict :: {coordinate(), coordinate()} | none) -> [cbs_constraint()].
make_constraint(_Bot, Time, _Pos, {From, To}) ->
    [{Time, {From, To}}];
make_constraint(_Bot, Time, Pos, none) ->
    [{Time, Pos}].

%% @doc Replans the path for a bot after a conflict using NewConstraints; Bots is a list of {BotId, Start, Goal} tuples.
-spec replan(Node :: cbs_node(), Bot :: integer(), NewConstraints :: [cbs_constraint()], Bots :: [bot()]) -> cbs_node() | {error, no_path}.
replan(Node, Bot, NewConstraints, Bots) ->
    case lists:keyfind(Bot, 1, Bots) of
        false ->
            {error, bot_not_found};
        {Bot, Start, Goal} ->
            NewPath = astar:find_path(Start, Goal, NewConstraints),
            case NewPath of
                {error, no_path} ->
                    {error, no_path};
                _ ->
                    NewPaths = update_path(Node#node.paths, Bot, NewPath),
                    #node{constraints = NewConstraints, paths = NewPaths, cost = compute_cost(NewPaths)}
            end
    end.

%% @doc Updates the path for a specific bot in the list of bot paths.
-spec update_path(Paths :: [bot_path()], Bot :: integer(), NewPath :: path()) -> [bot_path()].
update_path(Paths, Bot, NewPath) ->
    [ if Bot =:= B -> {B, NewPath}; true -> {B, P} end || {B, P} <- Paths].

%% @doc Computes the cost of a CBS node based on the maximum path length among all bot paths.
-spec compute_cost(Paths :: [bot_path()]) -> integer().
compute_cost(Paths) ->
    lists:max([length(Path) || {_Bot, Path} <- Paths]).

%% @doc Inserts new CBS nodes into the open list while sorting by cost in ascending order.
-spec insert_cbs_nodes(Nodes :: [cbs_node()]) -> [cbs_node()].
insert_cbs_nodes(Nodes) ->
    lists:sort(fun(N1, N2) -> N1#node.cost =< N2#node.cost end, Nodes).
