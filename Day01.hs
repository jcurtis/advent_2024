import Data.List (sort)

testInput = "3   4\n4   3\n2   5\n1   3\n3   9\n3   3\n"

-- part 1

parse :: String -> [(Int, Int)]
parse input = map parseLine (lines input)

parseLine line = (read a, read b)
 where
  [a, b] = words line

absolute (a, b) = abs (a - b)

solve input = sum (zipWith (curry absolute) (sort a) (sort b))
 where
  (a, b) = unzip (parse input)

test = solve testInput == 11

-- echo "input" | runhaskell Day01
-- powershell.exe -c Get-Clipboard | runhaskell Day01
main = do
  input <- getContents
  print (solve input)
  print (solve' input)

-- part 2

solve' input = sum $ map (count b) a
 where
  (a, b) = unzip (parse input)

count list x = x * length (filter (== x) list)

test' = solve' testInput == 31
