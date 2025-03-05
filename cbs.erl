-module(cbs).
-export([search/1]).

-record(node, {constraints = [], paths = [], cost = 0}).

%% @doc Initiates the CBS (Conflict-Based Search) algorithm for multiple bots.
%%It finds collision-free paths for all bots.
-spec search([{integer(), {integer(), integer()}, {integer(), integer()}}]) ->
    {error, no_solution} | [{integer(), [{integer(), integer()}]}].
search(Bots) ->
    InitialConstraints = [],
    InitialPaths = [{BotId, aster:find_path(Start, Goal, InitialConstraints)}
                     || {BotId, Start, Goal} <- Bots],
    Root = #node{constraints = InitialConstraints,
                  paths = InitialPaths,
                  cost = compute_cost(InitialPaths)},
    CBSOpen = [Root],
    cbs_open_search(CBSOpen, Bots).

%% @doc Main search loop for CBS algorithm.
%% Processes nodes and resolves conflicts iteratively.
-spec cbs_open_search([#node{}], [{integer(), {integer(), integer()}, {integer(), integer()}}]) ->
    {error, no_solution} | [{integer(), [{integer(), integer()}]}].
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

%% @doc Validates paths and checks for conflicts.
-spec validate_paths([{integer(), [{integer(), integer()}]}]) ->
    ok | {conflict, integer(), integer(), integer(), {integer(), integer()} | none, {integer(), integer()} | none}.
validate_paths( Paths) ->
    case find_conflict(Paths, 0) of
        none -> ok;
        Conflict -> Conflict
    end.

%% @doc Finds conflicts (vertex or edge) between paths.
-spec find_conflict([{integer(), [{integer(), integer()}]}], integer()) ->
    none | {conflict, integer(), integer(), integer(), {integer(), integer()} | none, {integer(), integer()} | none}.
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

%% @doc Retrieves a bot's position at a given time step.
-spec get_position([{integer(), integer()}], integer()) -> {integer(), integer()}.
get_position(Path, Time) ->
    if
        Time < length(Path) -> lists:nth(Time+1, Path);
        true -> lists:last(Path)
    end.

%% @doc Checks for vertex conflicts (same position at the same time).
-spec find_vertex_conflict([{integer(), {integer(), integer()}}]) ->
    none | {integer(), integer(), {integer(), integer()}}.
find_vertex_conflict([]) -> none;
find_vertex_conflict([{Bot, Pos} | Rest]) ->
    case lists:filter(fun({OtherBot, OtherPos}) ->
                          OtherPos =:= Pos andalso OtherBot =/= Bot
                      end, Rest) of
        [] -> find_vertex_conflict(Rest);
        [{OtherBot, _} | _] -> {Bot, OtherBot, Pos}
    end.

%% @doc Finds edge conflicts where two bots swap positions at the same time.
%% An edge conflict occurs if `BotA` moves from `PosA_T` to `PosA_T1`
%% while `BotB` moves from `PosB_T` to `PosB_T1` and they cross each other.
-spec find_edge_conflict([{integer(), list({integer(), integer()})}], integer()) ->
    none | {integer(), integer(), {integer(), integer()}, {integer(), integer()}}.
find_edge_conflict(Paths, Time) ->
    Pairs = [{A, B} || {A, _} <- Paths, {B, _} <- Paths, A < B],
    find_edge_conflict(Pairs,Paths, Time).

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

%% @doc Retrieves the path of a bot given a list of bot paths.
-spec get_path(integer(), [{integer(), list({integer(), integer()})}]) -> list({integer(), integer()}).
get_path(Bot, Paths) ->
    case lists:keyfind(Bot, 1, Paths) of
        false -> [];
        {Bot, Path} -> Path
    end.

%% @doc Generates constraints based on detected conflicts.
-spec make_constraint(integer(), integer(), {integer(), integer()}, {integer(), integer()} | none) ->
[{integer(), {integer(), integer()} | {integer(), integer(), integer(), integer()}}].
make_constraint(_Bot, Time, _Pos, {From, To}) ->
    [{Time, {From, To}}];
make_constraint(_Bot, Time, Pos, none) ->
    [{Time, Pos}].

%% @doc Replans the path for a bot after a conflict.
-spec replan(#node{}, integer(), [{integer(), {integer(), integer()}}], [{integer(), {integer(), integer()}, {integer(), integer()}}]) ->
    #node{} | {error, no_path}.
replan(Node, Bot, NewConstraints, Bots) ->
    case lists:keyfind(Bot, 1, Bots) of
        false -> {error, bot_not_found};
        {Bot, Start, Goal} ->
            NewPath = aster:find_path(Start, Goal, NewConstraints),
            case NewPath of
                {error, no_path} ->
                    {error, no_path};
                _ ->
                    NewPaths = update_path(Node#node.paths, Bot, NewPath),
                    #node{constraints = NewConstraints,
                          paths = NewPaths,
                          cost = compute_cost(NewPaths)}
            end
    end.

%% @doc Updates the path of a specific bot in the list of bot paths.
%%      If the bot is found in the list, its path is replaced with `NewPath`.
-spec update_path([{integer(), list({integer(), integer()})}], integer(), list({integer(), integer()})) ->
    [{integer(), list({integer(), integer()})}].
update_path(Paths, Bot, NewPath) ->
    [ if Bot =:= B -> {B, NewPath}; true -> {B, P} end || {B, P} <- Paths].

%% @doc Computes the cost of a CBS node based on path lengths.
-spec compute_cost([{integer(), [{integer(), integer()}]}]) -> integer().
compute_cost(Paths) ->
    lists:max([length(Path) || {_Bot, Path} <- Paths]).

%% @doc Inserts new CBS nodes into the open list while maintaining order.
-spec insert_cbs_nodes([#node{}]) -> [#node{}].
insert_cbs_nodes(Nodes) ->
    lists:sort(fun(N1, N2) -> N1#node.cost =< N2#node.cost end, Nodes).
