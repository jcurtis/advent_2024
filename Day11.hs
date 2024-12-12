import Data.MemoTrie (memo)

testInput = "125 17"

main = do
  input <- getContents
  print (solveFor 25 input)
  print (solveFor 75 input)

-- print (solve input)
-- print (solve' input)

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

memoBlinkStone = memo blinkStone

blinkStones :: [Integer] -> [Integer]
blinkStones = concatMap memoBlinkStone

solve input = length $ iterate blinkStones (parse input) !! 25

blinkStone' :: Integer -> Integer -> Integer
blinkStone' 0 _ = 1
blinkStone' iterations stone
  | stone == 0 = memoBlinkStone' iterationsLeft 1
  | even (numDigits stone) =
      let [a, b] = splitStone stone
       in memoBlinkStone' iterationsLeft a + memoBlinkStone' iterationsLeft b
  | otherwise = memoBlinkStone' iterationsLeft (stone * 2024)
 where
  iterationsLeft = iterations - 1

memoBlinkStone' = memo blinkStone'

solveFor blinks input = sum $ map (blinkStone' blinks) stones
 where
  stones = parse input

test = solve testInput == 55312

-- part 2

solve' input = length $ iterate blinkStones (parse input) !! 75
