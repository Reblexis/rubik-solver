import subprocess
import os
import ast
import random
import time

def build_rubik_solver():
    """
    Build the Rubik's Cube solver project using cabal.
    
    Returns:
        int: The return code of the build process (0 if successful).
    """
    build_process = subprocess.run(['cabal', 'build'], capture_output=True, text=True)

    if build_process.returncode != 0:
        print("Failed to build the project")
        print("stdout:", build_process.stdout)
        print("stderr:", build_process.stderr)
    return build_process.returncode

def run_rubik_solver(moves, time_limit=1000, search_depth=3, search_depth_g1=5):
    """
    Run the Rubik's Cube solver with specified parameters.
    
    Args:
        moves (str): The sequence of moves to solve.
        time_limit (int): Time limit for the solver in milliseconds.
        search_depth (int): The search depth for the solver.
        search_depth_g1 (int): The search depth for G1 phase.
        random_moves_num (int): Number of random moves to apply.
    
    Returns:
        tuple: (solution, score, duration) where solution is a list of moves,
               score is the solver's performance metric, and duration is the execution time.
    """
    # Step 1: Build the project
    
    ghc_version = "9.4.8" 
    package_name = "rubik-solver"
    package_version = "0.1.0.0" 
    executable_name = "rubik-solver"

    project_path = os.path.abspath(os.path.join(os.path.dirname(__file__), os.pardir, os.pardir))

    executable_path = f'{project_path}/dist-newstyle/build/x86_64-linux/ghc-{ghc_version}/{package_name}-{package_version}/x/{executable_name}/build/{executable_name}/{executable_name}'

    args = [moves, str(time_limit), str(search_depth), str(search_depth_g1)]

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
    """
    Generate a random shuffle sequence for the Rubik's Cube.
    
    Args:
        num_moves (int): Number of moves in the shuffle sequence.
    
    Returns:
        str: A space-separated string of random moves.
    """
    POSSIBLE_SHUFFLE_MOVES = ["R", "L", "U", "D", "F", "B", "RP", "LP", "UP", "DP", "FP", "BP"]
    shuffle = []
    for i in range(num_moves):
        move = random.choice(POSSIBLE_SHUFFLE_MOVES)
        while len(shuffle) > 0 and shuffle[-1][0] == move[0]:
            move = random.choice(POSSIBLE_SHUFFLE_MOVES)
        shuffle.append(move)
    shuffle = " ".join(shuffle)
    return shuffle

def test_solver(time_limit, search_depth, search_depth_g1, num_tests=100, num_moves=20):
    """
    Test the Rubik's Cube solver with multiple random shuffles.
    
    Args:
        time_limit (int): Time limit for each solve attempt in milliseconds.
        search_depth (int): The search depth for the solver.
        search_depth_g1 (int): The search depth for G1 phase.
        num_tests (int): Number of test cases to run.
        num_moves (int): Number of moves in each shuffle sequence.
    
    Returns:
        tuple: (avg_score, avg_solution_length, avg_duration) representing the
               average performance metrics across all test cases.
    """
    build_rubik_solver()

    scores = []
    solution_lengths = []
    durations = []
    
    best_solution = (None, float("-inf"))

    for i in range(num_tests):
        shuffle = generate_shuffle(num_moves=num_moves)
        print(f"Test {i + 1}/{num_tests}")
        print(f"Shuffle: {shuffle}")
        solution, score, duration = run_rubik_solver(shuffle, time_limit, search_depth, search_depth_g1)
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

    return avg_score, avg_solution_length, avg_duration, scores, solution_lengths, durations

if __name__ == "__main__":
    test_solver(100, 4, 5, num_tests=100, num_moves=100)
