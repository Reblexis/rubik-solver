import subprocess
import os
import ast
import random
import time

def run_rubik_solver(moves, algorithm):
    # Step 1: Build the project

    build_process = subprocess.run(['cabal', 'build'], capture_output=True, text=True)
    
    if build_process.returncode != 0:
        print("Failed to build the project")
        print("stdout:", build_process.stdout)
        print("stderr:", build_process.stderr)
        return build_process.returncode

    ghc_version = "8.8.4" 
    package_name = "rubik-solver"
    package_version = "0.1.0.0" 
    executable_name = "rubik-solver"

    executable_path = f'dist-newstyle/build/x86_64-linux/ghc-{ghc_version}/{package_name}-{package_version}/x/{executable_name}/build/{executable_name}/{executable_name}'

    args = [moves, algorithm]

    start_time = time.time()
    run_process = subprocess.run(
        [executable_path] + args,
        capture_output=True,
        text=True
    )
    duration = time.time() - start_time

    if run_process.returncode != 0:
        exception = f"Failed to run the executable, return code: {run_process.returncode}, stderr: {run_process.stderr}, stdout: {run_process.stdout}"
        raise Exception(exception)

    stdout = run_process.stdout
    solution, score = stdout.split("\n")[:2]
    solution = ast.literal_eval(solution.split(": ")[1])
    score = float(score.split(": ")[1])
    
    return solution, score, duration


def generate_shuffle(num_moves=20):
    POSSIBLE_SHUFFLE_MOVES = ["R", "L", "U", "D", "F", "B", "R'", "L'", "U'", "D'", "F'", "B'"]

    shuffle = " ".join(random.choices(POSSIBLE_SHUFFLE_MOVES, k=num_moves))

    return shuffle


def test_algorithm(algorithm, num_tests=100, num_moves=30):
    scores = []
    solution_lengths = []
    durations = []
    
    best_solution = (None, float("inf"))

    for i in range(num_tests):
        shuffle = generate_shuffle(num_moves=num_moves)
        print(f"Test {i + 1}/{num_tests}")
        solution, score, duration = run_rubik_solver(shuffle, algorithm)
        best_solution = max(best_solution, (solution, score), key=lambda x: x[1])
        print(f"solution: {solution}, score: {score}, duration: {duration}")
        scores.append(score)
        solution_lengths.append(len(solution))
        durations.append(duration)


    avg_score = sum(scores) / len(scores)
    avg_solution_length = sum(solution_lengths) / len(solution_lengths)
    avg_duration = sum(durations) / len(durations)

    print(f"Average score: {avg_score}")
    print(f"Average solution length: {avg_solution_length}")
    print(f"Average duration: {avg_duration}")
    print(f"Best solution: {best_solution[0]}, score: {best_solution[1]}")

    return avg_score, avg_solution_length, avg_duration



# Example usage

test_algorithm("baseline", num_tests=100, num_moves=30)
