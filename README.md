# Rubik's Cube Solver

This is a Rubik's Cube solver implemented in Haskell. It uses a heuristic-based search algorithm with time constraints to find solutions for scrambled Rubik's Cubes.

## Algorithm Overview

The solver employs the following strategies:

1. **Heuristic-guided search**: The algorithm uses a heuristic function that evaluates cube states based on the number of correctly positioned and oriented cubies.

2. **Time-constrained search**: The search is limited by a specified time constraint to ensure the solver doesn't run indefinitely.

3. **Depth-limited search**: The search is performed up to a certain depth, which is different for the general case and the G1 subgroup.

4. **G1 subgroup awareness**: The solver recognizes when the cube reaches the G1 subgroup (all cubies correctly oriented, and U-D slice edges in the correct slice) and switches to a more focused search strategy with a reduced move set.

5. **Move pruning**: Certain moves are pruned to reduce the search space, such as avoiding consecutive moves on the same face or opposite faces.

6. **Random moves for escaping local optima**: When the search fails to find an improving move, the solver applies a series of random moves to escape potential local optima.

The main search function (`findMoves`) explores the move space up to the specified depth limit, evaluating cube states and selecting the best moves based on the heuristic function. If an improvement is found, the search continues from the new state. If no improvement is found within the specified depth, the solver applies random moves and tries again.

This process repeats until either a solution is found (cube is solved) or the time limit is reached.

## Implementation Details

- The cube is represented using a cubie-based model, where each cubie has a position and orientation.
- Moves are represented as transformations on the cube state.
- The solver uses different move sets for the general case and the G1 subgroup to optimize the search.

## Inspiration

While not directly implementing any specific algorithm from it, this solver was inspired by concepts discussed in the paper:
[Finding Optimal Solutions to Rubik's Cube Using Pattern Databases](https://www.cs.princeton.edu/courses/archive/fall06/cos402/papers/korfrubik.pdf) by Richard E. Korf.

## Limitations

- The solver does not use pattern databases or extensive precomputation.
- It does not guarantee finding the optimal (shortest) solution.
- Performance may vary depending on the initial scramble and the chosen time limit.

## Usage

```
cabal run rubik-solver <shuffle> <time_limit> <search_depth> <search_depth_g1> <random_moves_num>
```

1. Shuffle: A string of space-separated moves to generate the initial cube state.
2. Time limit: Maximum time allowed for solving (in milliseconds).
3. Search depth: Maximum depth for the search algorithm.
4. Search depth G1: Maximum depth for the G1 phase of the search.

The program will output the solution moves and the achieved score.


### Example:
```
cabal run "R LP U D" 500000 6 8
```


