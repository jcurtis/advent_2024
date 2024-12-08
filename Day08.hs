import Data.List (nub)
import Data.Map.Strict qualified as Map

testInput = "............\n........0...\n.....0......\n.......0....\n....0.......\n......A.....\n............\n............\n........A...\n.........A..\n............\n............"

-- part 1

parse input =
  ( foldGrid
      [ (grid !! y !! x, (x, y))
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
  | a == c && b == d = []
  | otherwise = [(a + x, b + y), (c - x, d - y)]
 where
  (x, y) = (a - c, b - d)

withinBounds (lenX, lenY) (x, y) = x >= 0 && y >= 0 && x < lenX && y < lenY

solve input = length $ nub $ concatMap (filter (withinBounds bounds) . antinodesFromList . snd) (Map.toList grid)
 where
  (grid, bounds) = parse input

test = solve testInput == 14

main = do
  input <- getContents
  print (solve input)
  print (solve' input)

-- part 2

solve' input = length $ nub $ concatMap (\(_, nodes) -> harmonicsFromList nodes bounds) (Map.toList grid)
 where
  (grid, bounds) = parse input

resonance operator (a, b) (x, y) = (antiA, antiB) : resonance operator (antiA, antiB) (x, y)
 where
  (antiA, antiB) = (a `operator` x, b `operator` y)

resonancePos = resonance (+)
resonanceNeg = resonance (-)

harmonics checkBounds (a, b) (c, d)
  | a == c && b == d = [(a, b)]
  | otherwise =
      takeWhile checkBounds (resonanceNeg (a, b) (x, y))
        ++ [(a, b), (c, d)]
        ++ takeWhile checkBounds (resonancePos (a, b) (x, y))
 where
  (x, y) = (a - c, b - d)

harmonicsFromList nodes bounds = concatMap (uncurry (harmonics checkBounds)) pairs
 where
  pairs = [(x, y) | x <- nodes, y <- nodes, x < y]
  checkBounds = withinBounds bounds

test' = solve' testInput == 34
