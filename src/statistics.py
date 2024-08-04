import subprocess


# Path to the compiled Haskell executable
executable_path = './dist-newstyle/build/x86_64-linux/ghc-8.10.4/myproject-0.1.0.0/x/myproject/build/myproject/myproject'

# Parameters to pass
params = ['param1', 'param2']

# Run the Haskell executable with parameters
result = subprocess.run([executable_path] + params, capture_output=True, text=True)

# Output and error
print("Output:", result.stdout)
print("Error:", result.stderr)