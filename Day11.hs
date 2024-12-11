testInput = "125 17"

main = do
  input <- getContents
  print (solve testInput)

-- part 1

parse :: String -> [Integer]
parse input = map read (words input)

numDigits stone = length (show stone)

splitStone :: Integer -> [Integer]
splitStone stone = [read stoneA, read stoneB]
 where
  stoneStr = show stone
  len = length stoneStr
  (stoneA, stoneB) = splitAt (len `div` 2) stoneStr

blinkStone 0 = [1]
blinkStone stone =
  if even (numDigits stone)
    then splitStone stone
    else [stone * 2024]

blinkStones :: [Integer] -> [Integer]
blinkStones = concatMap blinkStone

solve input = length $ iterate blinkStones (parse input) !! 25

test = solve testInput == 55312
