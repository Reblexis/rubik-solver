# Rubik solver

Solver for rubik's cube made in Haskell. 

Papers used for inspiration:
https://www.cs.princeton.edu/courses/archive/fall06/cos402/papers/korfrubik.pdf.


Finds a solution for rubik's cube by first getting to G1 subgroup and then searching with a smaller set of moves while using deeper search (more about the group can be found on [Wikipedia](https://en.wikipedia.org/wiki/Optimal_solutions_for_the_Rubik%27s_Cube)).

The algorithm is essentially a repeated DFS + randomness if no improving moves are found. 
It doesn't use any caching / memoization nor prewritten move sequences.

