# Multi-Agent Pathfinding (MAPF) on a 10x10 Grid

This project implements a solution for the Multi-Agent Pathfinding (MAPF) problem using Conflict-Based Search (CBS) combined with the A* algorithm. The system is built in Erlang and utilizes Mnesia for persistent storage of grid information and bot paths.

## Introduction

The MAPF problem involves finding paths for multiple bots on a grid such that no two bots occupy the same cell at the same time. In our implementation, the grid is fixed at a 10x10 size, and several constraints ensure safe navigation:

- **Grid and Data Storage:**  
  The grid is populated and managed via Mnesia. Each grid cell is stored with its neighboring positions, and the computed paths for each bot are saved in the Mnesia database.

- **Bot Movement Constraints:**  
  - **Vertex Constraints:** No two bots can be in the same cell at the same time.  
  - **Edge Constraints:** Bots cannot swap or cross each other’s positions simultaneously.

- **Conflict Handling:**  
  Conflict-Based Search (CBS) detects conflicts between bot paths and introduces constraints iteratively to replan individual routes until a conflict-free set of paths is achieved.

## Idea of the Solution Algorithm / Logic Used

### A* Pathfinding (Module: `aster.erl`)
- **Purpose:**  
  Computes an optimal path from a start to a goal position using A* with a Manhattan distance heuristic.
- **Key Features:**  
  - **Optimized search:** Utilizes Erlang’s `gb_trees` for efficient node management, significantly optimizing search time.
  - **Constraint Checking:**  
    Before moving to a neighbor, the algorithm verifies that the move does not violate any vertex or edge constraints.

### Conflict-Based Search (Module: `cbs.erl`)
- **Purpose:**  
  Extends single-agent A* to multiple agents by detecting and resolving conflicts.
- **Process:**  
  - **Initial Paths:**  
    Each bot computes a path using A* without any constraints.
  - **Conflict Detection:**  
    The algorithm iterates through the paths to detect vertex or edge conflicts.
  - **Constraint Generation:**  
    When a conflict is found, CBS generates new constraints (either vertex or edge based) for the involved bots.
  - **Replanning:**  
    Affected bots replan their paths using the updated constraints. The new paths are then checked again for conflicts.
  - **Optimization:**  
    Sorting and maintaining nodes by cost (using `gb_trees`) ensures that lower-cost (more optimal) solutions are explored first.

## Flow of the Program

The project is structured into several Erlang modules, each handling distinct parts of the MAPF solution:

### Initialization and Simulation (Module: `bot_simulation.erl`)
- **Database and Grid Setup:**  
  - The `init_db/0` function initializes the Mnesia database and creates tables for bot paths and grid cells.
  - The grid is populated with cell positions and their respective neighbors via `grid_manager:init_grid/0`.
- **Bot Generation:**  
  - **Random Generation:** The `start/0` function generates a set of bots with unique start and goal positions.
  - **Manual Setup:** The `start_manual/1` function accepts a list of bot positions, validating and initializing bots based on user-provided coordinates.
- **Path Calculation:**  
  The module delegates path computation to the CBS module. It collects individual bot paths and stores them in the Mnesia database.
- **Simulation:**  
  Bots’ movements are simulated step-by-step, printing each bot’s position at every time step until all bots reach their destinations.

### Conflict Resolution and Pathfinding (Module: `cbs.erl`)
- **Conflict-Based Search Loop:**  
  The CBS algorithm repeatedly checks for conflicts in the computed paths. When a conflict is detected, it:
  - Generates new constraints for the conflicting bots.
  - Replans paths for these bots with the updated constraints.
  - Re-inserts the updated nodes into the search queue, ordered by cost.
- **Termination:**  
  The search terminates when a set of paths is found with no conflicts, or reports failure if no solution exists.

### Testing (Module: `pathfinding_test.erl`)
This module provides several test cases to validate the implementation:
- **Test 1: Same Start and Goal**  
  Checks the trivial case where a bot’s start and goal positions are identical.
- **Test 2: Swap Conflict**  
  Two bots swap positions; tests the edge conflict resolution.
- **Test 3: Vertex Conflict**  
  Two bots attempt to occupy the same cell concurrently.
- **Test 4: Multiple Bots**  
  Validates the solution with five bots having diverse paths.
- **Test 5: Boundary Bots**  
  Bots are placed along grid boundaries to ensure edge cells are correctly handled.
- **Test 6: Difficult Edge Cases**  
  Simulates heavy symmetric conflicts with multiple bots to test the robustness of the CBS algorithm.

---
## Commands

Commands to compile the erl files:
 ```bash 
  c(aster).
  c(cbs).
  c(bot_simulation).
  c(grid_manager).
  c(pathfinding_test).
```
This compiles the erl files and generates the beam files.

### Running with Random Bot Initialization
To start the simulation with randomly generated bot positions, run:
```bash
bot_simulation:start().
```
### Running with Manual Bot Positions
To run the simulation with manually provided bot positions (using a test case with 5 bots):
```bash
bot_simulation:start_manual([
    {{1,1}, {10,10}},
    {{10,10}, {1,1}},
    {{1,10}, {10,1}},
    {{10,1}, {1,10}},
    {{5,1}, {5,10}}
```
### Running tests
To execute all the tests defined in `pathfinding_test.erl`, run:
```bash
pathfinding_test:run_tests().
