import subprocess
import os
import ast

def run_rubik_solver(moves, algorithm):
    # Step 1: Build the project
    build_process = subprocess.run(['cabal', 'build'], capture_output=True, text=True)
    
    if build_process.returncode != 0:
        print("Failed to build the project")
        print("stdout:", build_process.stdout)
        print("stderr:", build_process.stderr)
        return build_process.returncode

    # Replace placeholders with actual values
    ghc_version = "8.8.4" # Change this to match your GHC version
    package_name = "rubik-solver"
    package_version = "0.1.0.0" # Change this to match your package version
    executable_name = "rubik-solver"

    executable_path = f'dist-newstyle/build/x86_64-linux/ghc-{ghc_version}/{package_name}-{package_version}/x/{executable_name}/build/{executable_name}/{executable_name}'

    # Step 3: Construct the argument list
    args = [moves, algorithm]

    # Step 4: Run the executable with the arguments
    run_process = subprocess.run(
        [executable_path] + args,
        capture_output=True,
        text=True
    )

    if run_process.returncode != 0:
        exception = f"Failed to run the executable, return code: {run_process.returncode}, stderr: {run_process.stderr}, stdout: {run_process.stdout}"
        raise Exception(exception)

    # Print the output
    stdout = run_process.stdout
    solution, score = stdout.split("\n")[:2]
    print(solution.strip())
    solution = ast.literal_eval(solution.split(": ")[1])
    score = float(score.split(": ")[1])
    
    print(f"Solution: {solution}")
    print(f"Score: {score}")

    # Return the process return code

    return solution, score


SHUFFLE_MOVES = ["R", "L", "U", "D", "F", "B", "R'", "L'", "U'", "D'", "F'", "B'"]

# Example usage
return_code = run_rubik_solver("R L U B", "baseline")