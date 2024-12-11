import Data.Char (digitToInt)
import Data.List (nub)

main = do
  input <- getContents
  print (solve input)
  print (solve' input)

testInput = "89010123\n78121874\n87430965\n96549874\n45678903\n32019012\n01329801\n10456732"
testGrid = parse testInput
testInput' = "0123\n1234\n8765\n9876"
testGrid' = parse testInput'

-- part 1

parse input = map (map digitToInt) (lines input)

pos grid (x, y) = grid !! y !! x

trailheads grid = [(x, y) | x <- [0 .. length (head grid) - 1], y <- [0 .. length grid - 1], pos grid (x, y) == 0]

bounds grid = (length (head grid), length grid)
withinBounds (boundX, boundY) (x, y) = x >= 0 && y >= 0 && x < boundX && y < boundY

directions grid (x, y) = filter (withinBounds (bounds grid)) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

nextSteps grid curPos = filter (\node -> nextNodeScore == pos grid node) (directions grid curPos)
 where
  nextNodeScore = pos grid curPos + 1

trailBranches grid curPos
  | nextNodeScore > 9 = [curPos]
  | otherwise = nub $ concatMap (trailBranches grid) (nextSteps grid curPos)
 where
  nextNodeScore = pos grid curPos + 1

solve input = length $ concatMap (trailBranches grid) (trailheads grid)
 where
  grid = parse input

test = solve testInput == 36

-- part 2

trailBranches' grid curPos
  | nextNodeScore > 9 = [curPos]
  | otherwise = concatMap (trailBranches' grid) (nextSteps grid curPos)
 where
  nextNodeScore = pos grid curPos + 1

solve' input = length $ concatMap (trailBranches' grid) (trailheads grid)
 where
  grid = parse input

test' = solve' testInput == 81
