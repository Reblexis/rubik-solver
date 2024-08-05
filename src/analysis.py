import subprocess

# Arguments to pass, with -- to separate them from cabal's own arguments
args = ['--', 'arg1', 'arg2', 'arg3']

# Use cabal run to execute the project
process = subprocess.run(['cabal', 'run'] + args, capture_output=True, text=True)


# Print the output
print('STDOUT:', process.stdout)
print('STDERR:', process.stderr)