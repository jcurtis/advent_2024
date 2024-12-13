import Data.List (nub)
import Debug.Trace (trace)

testInput1 = "AAAA\nBBCD\nBBCC\nEEEC" -- 140
testInput2 = "OOOOO\nOXOXO\nOOOOO\nOXOXO\nOOOOO" -- 772
testInput3 = "RRRRIICCFF\nRRRRIICCCF\nVVRRRCCFFF\nVVRCCCJFFF\nVVVVCJJCFE\nVVIVCCJJEE\nVVIIICJJEE\nMIIIIIJJEE\nMIIISIJEEE\nMMMISSJEEE" -- 1930

pos grid (x, y) = grid !! y !! x

bounds grid = (length (head grid), length grid)
withinBounds (boundX, boundY) (x, y) = x >= 0 && y >= 0 && x < boundX && y < boundY

directions grid (x, y) = filter (withinBounds (bounds grid)) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

countFences grid seen (x, y)
  | (x, y) `elem` seen = 0
  | null validDirections = 4
  | length validDirections == 1 = 3
  | length validDirections == 2 = 2
  | length validDirections == 3 = 1
  | length validDirections == 4 = 0
 where
  plantType = pos grid (x, y)
  validDirections =
    filter
      ( \checkNode ->
          pos grid checkNode == plantType
      )
      (directions grid (x, y))

price grid plantType = sum fences * length fences
 where
  (boundX, boundY) = bounds grid
  matchingNodes = [(x, y) | x <- [0 .. boundX - 1], y <- [0 .. boundY - 1], pos grid (x, y) == plantType]
  fences = map (countFences grid []) matchingNodes

solve input = sum $ map (price grid) plantTypes
 where
  grid = lines input
  (boundX, boundY) = bounds grid
  allNodes = [(x, y) | x <- [0 .. boundX - 1], y <- [0 .. boundY]]
  plantTypes = nub $ filter (/= '\n') input
