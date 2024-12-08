import Data.List (nub)
import Data.Map.Strict qualified as Map

testInput = "............\n........0...\n.....0......\n.......0....\n....0.......\n......A.....\n............\n............\n........A...\n.........A..\n............\n............"

-- part 1

parse input =
  ( [ (grid !! y !! x, (x, y))
    | x <- [0 .. lenX - 1]
    , y <- [0 .. lenY - 1]
    , grid !! y !! x /= '.'
    ]
  , (lenX, lenY)
  )
 where
  grid = lines input
  lenX = length (head grid)
  lenY = length grid

insert :: (Char, (Int, Int)) -> Map.Map Char [(Int, Int)] -> Map.Map Char [(Int, Int)]
insert (a, (x, y)) = Map.insertWith (++) a [(x, y)]

foldGrid :: [(Char, (Int, Int))] -> Map.Map Char [(Int, Int)]
foldGrid = foldr insert Map.empty

antinodesFromList :: [(Int, Int)] -> [(Int, Int)]
antinodesFromList nodes = nub $ concatMap (uncurry antinodes) pairs
 where
  pairs = [(x, y) | x <- nodes, y <- nodes, x < y]

-- ..........
-- ...#......
-- ..........
-- ....a.....
-- ..........
-- .....a....
-- ..........
-- ......#...
-- ..........
-- ..........
antinodes (a, b) (c, d)
  | (a + b) < (c + d) = [(a - x, b - y), (c + x, d + y)]
  | otherwise = [(a + x, b + y), (c - x, d - y)]
 where
  (x, y) = (abs (a - c), abs (b - d))

withinBounds (lenX, lenY) (x, y) = x >= 0 && y >= 0 && x < lenX && y < lenY

solve input = length $ nub $ concatMap (filter (withinBounds bounds) . antinodesFromList . snd) (Map.toList mapped)
 where
  (grid, bounds) = parse input
  mapped = foldGrid grid

test = solve testInput == 14

main = do
  input <- getContents
  print (solve input)
