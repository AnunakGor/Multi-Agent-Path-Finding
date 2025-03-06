-module(grid_manager).
-export([init_grid/0, get_neighbors/1, is_valid_cell/1]).

% Define the record
-record(grid_cell, {position, neighbors}).

%% @doc Initializes the grid in the Mnesia database
-spec init_grid() -> ok.
init_grid() ->
    % Make sure table name matches record name
    mnesia:create_table(grid_cell,
                       [{attributes, record_info(fields, grid_cell)},
                        {disc_copies, [node()]}]),
    populate_grid(10, 10).

%% @doc Populates the grid with cell information
-spec populate_grid(integer(), integer()) -> ok.
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

%% @doc Calculates valid neighboring positions for a cell
-spec calculate_neighbors(integer(), integer(), integer(), integer()) -> 
    list({integer(), integer()}).
calculate_neighbors(X, Y, Width, Height) ->
    Potential = [{X, Y}, {X+1, Y}, {X-1, Y}, {X, Y+1}, {X, Y-1}],
    [{NX, NY} || {NX, NY} <- Potential, 
                 NX >= 1, NX =< Width, 
                 NY >= 1, NY =< Height].

%% @doc Gets neighbors of a cell from the database
-spec get_neighbors({integer(), integer()}) -> list({integer(), integer()}).
get_neighbors(Position) ->
    Fun = fun() ->
            case mnesia:read(grid_cell, Position, read) of
                [#grid_cell{neighbors = Neighbors}] -> Neighbors;
                [] -> [] 
            end
          end,
    {atomic, Result} = mnesia:transaction(Fun),
    Result.

%% @doc Validates if a cell position exists in the grid
-spec is_valid_cell({integer(), integer()}) -> boolean().
is_valid_cell({X, Y}) ->
    X >= 1 andalso X =< 10 andalso Y >= 1 andalso Y =< 10.