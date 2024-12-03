set -ex

# Copy input to clipboard then run:
# ./run.sh DayXX
powershell.exe -c Get-Clipboard | runhaskell $1
