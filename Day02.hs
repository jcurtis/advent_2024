import Data.List (sort)

-- part 1

solve input = length (filter id (map testSafety parsedInput))
 where
  parsedInput = parse input

parse :: String -> [[Int]]
parse input = map parseLine (lines input)

parseLine :: String -> [Int]
parseLine line = map read (words line)

testSafety :: [Int] -> Bool
testSafety report =
  ((report == sortedReport) || (report == reverse sortedReport))
    && checkDistortion report
 where
  sortedReport = sort report

checkDistortion :: [Int] -> Bool
checkDistortion [_] = True
checkDistortion report@(x : y : _) = val <= 3 && val >= 1 && checkDistortion (tail report)
 where
  val = abs (x - y)

testInput = "7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9"

test = solve testInput == 2

main = do
  input <- getContents
  print (solve input)
