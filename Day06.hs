import Data.Set (fromList)

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

move :: (Int, Int) -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
move guard dir obstacles visited (lenX, lenY)
  | newPos `elem` obstacles = move guard (turnRight dir) obstacles visited (lenX, lenY)
  | x < 0 = visited
  | y < 0 = visited
  | x >= lenX = visited
  | y >= lenY = visited
  | otherwise = move newPos dir obstacles (newPos : visited) (lenX, lenY)
 where
  newPos = add guard dir
  (x, y) = newPos

solve input = length $ fromList (move guard up obs [guard] (lenX, lenY))
 where
  (guard, obs, (lenX, lenY)) = parse input

test = solve testInput == 41

main = do
  input <- getContents
  print (solve input)
