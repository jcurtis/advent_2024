import Control.Parallel.Strategies (parList, rdeepseq, using)
import Data.Maybe (isNothing)
import Data.Set (fromList)
import Debug.Trace (trace)

testInput = "....#.....\n.........#\n..........\n..#.......\n.......#..\n..........\n.#..^.....\n........#.\n#.........\n......#..."
testData = parse testInput

-- part 1

pos x y grid = grid !! y !! x

lens grid = (length (head grid), length grid)

findPos grid f = [(x, y) | x <- [0 .. lenX - 1], y <- [0 .. lenY - 1], f $ pos x y grid]
 where
  (lenX, lenY) = lens grid

parse input = (guard, obs, (lenX, lenY))
 where
  grid = lines input
  (lenX, lenY) = lens grid
  obs = findPos grid (== '#')
  guard = head (findPos grid (== '^'))

add (a, b) (c, d) = (a + c, b + d)

up = (0, -1)
right = (1, 0)
down = (0, 1)
left = (-1, 0)

turnRight (0, -1) = right
turnRight (1, 0) = down
turnRight (0, 1) = left
turnRight (-1, 0) = up

move guard dir obstacles visited (lenX, lenY)
  | (newPos, dir) `elem` visited = Nothing
  | newPos `elem` obstacles = move guard (turnRight dir) obstacles visited (lenX, lenY)
  | x < 0 = Just visited
  | y < 0 = Just visited
  | x >= lenX = Just visited
  | y >= lenY = Just visited
  | otherwise = move newPos dir obstacles ((newPos, dir) : visited) (lenX, lenY)
 where
  newPos = add guard dir
  (x, y) = newPos

countVisited Nothing = 0
countVisited (Just visited) = length $ fromList (map fst visited)

solve input = countVisited visited
 where
  (guard, obs, (lenX, lenY)) = parse input
  visited = move guard up obs [(guard, up)] (lenX, lenY)

test = solve testInput == 41

main = do
  input <- getContents
  print (solve input)
  print (solve' input)

-- part 2

maybeOr Nothing d = d
maybeOr (Just x) _ = x

solve' input =
  length $
    filter
      isNothing
      ( map
          ( \x ->
              trace
                ("trying pot obs " ++ show x)
                (move guard up (x : obs) [(guard, up)] (lenX, lenY))
          )
          potObs
          `using` parList rdeepseq
      )
 where
  (guard, obs, (lenX, lenY)) = parse input
  visited = move guard up obs [(guard, up)] (lenX, lenY)
  visitedNodes = map fst (maybeOr visited [])
  potObs =
    [ (x, y)
    | x <- [0 .. lenX - 1]
    , y <- [0 .. lenX - 1]
    , (x, y) `notElem` (guard : obs)
    , (x, y) `elem` visitedNodes
    ]

test' = solve' testInput == 6
