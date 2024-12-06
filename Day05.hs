import Data.List.Split (splitOn)

testInput = "47|53\n97|13\n97|61\n97|47\n75|29\n61|13\n75|53\n29|13\n97|29\n53|29\n61|53\n97|53\n61|29\n47|13\n75|47\n97|75\n47|61\n75|61\n47|29\n75|13\n53|1\n\n75,47,61,53,29\n97,61,53,29,13\n75,29,13\n75,97,47,61,53\n61,13,29\n97,13,75,29,47"
(testRules, testUpdates) = parse testInput

-- part 1

parse input =
  let (rules, _ : updates) = break null inputLines
   in (map parseRule rules, map parseUpdate updates)
 where
  inputLines = lines input

parseRule :: String -> (Int, Int)
parseRule input = (read a, read b)
 where
  (a, _ : b) = break (== '|') input

parseUpdate :: String -> [Int]
parseUpdate input = map read (splitOn "," input)

verifyRule :: [Int] -> (Int, Int) -> Bool
verifyRule update (a, b) = a `notElem` update || b `notElem` pre
 where
  (pre, _) = break (== a) update

verifyUpdate :: [(Int, Int)] -> [Int] -> Bool
verifyUpdate rules update = all (verifyRule update) rules

middle xs = xs !! middleIndex
 where
  len = length xs
  middleIndex = len `div` 2

countMiddles updates = sum $ map middle updates

solve input = countMiddles (filter (verifyUpdate rules) updates)
 where
  (rules, updates) = parse input

test = solve testInput == 143

main = do
  input <- getContents
  print (solve input)
  print (solve' input)

-- part 2

fixRule :: (Int, Int) -> [Int] -> [Int]
fixRule (a, b) update
  | verifyRule update (a, b) = update
  | otherwise =
      let (pre, _ : post) = break (== a) update
       in filter (/= b) pre ++ [a, b] ++ post

fixUpdate rules update = foldr fixRule update rules

solve' input = countMiddles (map (fixUpdate rules) badUpdates)
 where
  (rules, updates) = parse input
  badUpdates = filter (not . verifyUpdate rules) updates

test' = solve' testInput == 123
