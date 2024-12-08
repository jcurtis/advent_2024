set -ex

# Copy input to clipboard then run:
# ./run.sh DayXX
# powershell.exe -c Get-Clipboard | runhaskell $1

# ./run.sh 8 Day08
curl "https://adventofcode.com/2024/day/$1/input" --cookie "session=$AOC_SESSION" | runhaskell $2
