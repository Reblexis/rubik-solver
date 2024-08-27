import subprocess
import os
import ast
import random
import time


def build_rubik_solver():
    build_process = subprocess.run(['cabal', 'build'], capture_output=True, text=True)

    if build_process.returncode != 0:
        print("Failed to build the project")
        print("stdout:", build_process.stdout)
        print("stderr:", build_process.stderr)
        return build_process.returncode


def run_rubik_solver(moves, time_limit=1000, search_depth=3, search_depth_g1=5, random_moves_num=3):
    # Step 1: Build the project
    
    ghc_version = "9.4.8" 
    package_name = "rubik-solver"
    package_version = "0.1.0.0" 
    executable_name = "rubik-solver"

    executable_path = f'dist-newstyle/build/x86_64-linux/ghc-{ghc_version}/{package_name}-{package_version}/x/{executable_name}/build/{executable_name}/{executable_name}'

    args = [moves, str(time_limit), str(search_depth), str(search_depth_g1), str(random_moves_num)]

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

    print(f"Stderr: {run_process.stderr}")
    
    return solution, score, duration


def generate_shuffle(num_moves=20):
    #POSSIBLE_SHUFFLE_MOVES = ["R2", "L2", "U", "D", "F2", "B2"]
    POSSIBLE_SHUFFLE_MOVES = ["R", "L", "U", "D", "F", "B", "RP", "LP", "UP", "DP", "FP", "BP"]

    shuffle = " ".join(random.choices(POSSIBLE_SHUFFLE_MOVES, k=num_moves))

    return shuffle


def test_solver(time_limit, search_depth, search_depth_g1, random_moves_num, num_tests=100, num_moves=20):
    build_rubik_solver()

    scores = []
    solution_lengths = []
    durations = []
    
    best_solution = (None, float("-inf"))

    for i in range(num_tests):
        shuffle = generate_shuffle(num_moves=num_moves)
        print(f"Test {i + 1}/{num_tests}")
        print(f"Shuffle: {shuffle}")
        solution, score, duration = run_rubik_solver(shuffle, time_limit, search_depth, search_depth_g1, random_moves_num)
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

test_solver(500000, 6, 8, 6, num_tests=100, num_moves=10)
