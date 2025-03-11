-module(grid_manager).
-export([init_grid/0, get_neighbors/1, is_valid_cell/1]).

-record(grid_cell, {position, neighbors}).

%% Type Definitions

-type coordinate() :: {integer(), integer()}.
-type grid_dim() :: pos_integer().

%% @doc Initializes the grid in the Mnesia database and populates it for a 10x10 grid.
-spec init_grid() -> ok.
init_grid() ->
    mnesia:create_table(grid_cell,
                        [{attributes, record_info(fields, grid_cell)},
                         {disc_copies, [node()]}]),
    populate_grid(10, 10).

%% @doc Populates the grid with cell information for given Width and Height.
-spec populate_grid(Width :: grid_dim(), Height :: grid_dim()) -> ok | {error, term()}.
populate_grid(Width, Height) ->
    Transaction = fun() ->
            [begin
                Pos = {X, Y},
                Neighbors = calculate_neighbors(X, Y, Width, Height),
                mnesia:write(#grid_cell{position = Pos, neighbors = Neighbors})
             end || X <- lists:seq(1, Width), Y <- lists:seq(1, Height)]
          end,
    case mnesia:transaction(Transaction) of
        {atomic, _Result} -> ok;
        {aborted, Reason} ->
            io:format("Grid population failed: ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc Calculates valid neighboring positions for a cell at (X, Y) within grid dimensions Width and Height.
-spec calculate_neighbors(X :: integer(), Y :: integer(), Width :: grid_dim(), Height :: grid_dim()) -> [coordinate()].
calculate_neighbors(X, Y, Width, Height) ->
    Potential = [{X, Y}, {X+1, Y}, {X-1, Y}, {X, Y+1}, {X, Y-1}],
    [{NX, NY} || {NX, NY} <- Potential, NX >= 1, NX =< Width, NY >= 1, NY =< Height].

%% @doc Gets the list of neighbor coordinates for the given cell Position from the database.
-spec get_neighbors(Position :: coordinate()) -> [coordinate()].
get_neighbors(Position) ->
    case mnesia:dirty_read(grid_cell, Position) of
        [#grid_cell{neighbors = Neighbors}] -> Neighbors;
        [] -> []
    end.

%% @doc Checks if a given cell coordinate is within the valid 10x10 grid boundaries.
-spec is_valid_cell(Pos :: coordinate()) -> boolean().
is_valid_cell({X, Y}) ->
    X >= 1 andalso X =< 10 andalso Y >= 1 andalso Y =< 10.
