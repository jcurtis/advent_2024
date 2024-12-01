import Data.List (sort)

parse input = map parseLine (lines input)
parseLine line = (read a, read b)
 where
  [a, b] = words line

absolute (a, b) = abs (a - b)

solve input = sum (zipWith (curry absolute) (sort a) (sort b))
 where
  (a, b) = unzip (parse input)

testInput = "3   4\n4   3\n2   5\n1   3\n3   9\n3   3"

main = do
  input <- getContents
  print (solve input)
