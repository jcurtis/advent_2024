import Debug.Trace (trace)

testInput = "MMMSXXMASM\nMSAMXMSMSA\nAMXSXMAAMM\nMSAMASMSMX\nXMASAMXAMM\nXXAMMXXAMA\nSMSMSASXSS\nSAXAMASAAA\nMAMMMXMMMM\nMXMXAXMASX"
testGrid = lines testInput

-- part 1

count :: [Bool] -> Int
count list = length (filter id list)

fitRight x lengthX = x < (lengthX - 3)
fitLeft x = x >= 3
fitUp y = y >= 3
fitDown y lengthY = y < (lengthY - 3)

-- checkRight grid (x, y) | trace ("checkRight " ++ show grid ++ " at " ++ show x ++ show y) False = undefined
checkRight grid (x, y) =
  fitRight x lengthX
    && ( (grid !! y !! x == 'X')
          && (grid !! y !! (x + 1) == 'M')
          && (grid !! y !! (x + 2) == 'A')
          && (grid !! y !! (x + 3) == 'S')
       )
 where
  lengthY = length grid
  lengthX = length (head grid)

checkLeft grid (x, y) =
  fitLeft x
    && ( (grid !! y !! x == 'X')
          && (grid !! y !! (x - 1) == 'M')
          && (grid !! y !! (x - 2) == 'A')
          && (grid !! y !! (x - 3) == 'S')
       )
 where
  lengthY = length grid
  lengthX = length (head grid)

checkDown grid (x, y) =
  fitDown y lengthY
    && ( (grid !! y !! x == 'X')
          && (grid !! (y + 1) !! x == 'M')
          && (grid !! (y + 2) !! x == 'A')
          && (grid !! (y + 3) !! x == 'S')
       )
 where
  lengthY = length grid
  lengthX = length (head grid)

checkUp grid (x, y) =
  fitUp y
    && ( (grid !! y !! x == 'X')
          && (grid !! (y - 1) !! x == 'M')
          && (grid !! (y - 2) !! x == 'A')
          && (grid !! (y - 3) !! x == 'S')
       )
 where
  lengthY = length grid
  lengthX = length (head grid)

checkRightUp grid (x, y) =
  (fitRight x lengthX && fitUp y)
    && ( (grid !! y !! x == 'X')
          && (grid !! (y - 1) !! (x + 1) == 'M')
          && (grid !! (y - 2) !! (x + 2) == 'A')
          && (grid !! (y - 3) !! (x + 3) == 'S')
       )
 where
  lengthY = length grid
  lengthX = length (head grid)
testRightUp = not (checkRightUp testGrid (9, 9)) && checkRightUp testGrid (5, 9)

checkRightDown grid (x, y) =
  (fitRight x lengthX && fitDown y lengthY)
    && ( (grid !! y !! x == 'X')
          && (grid !! (y + 1) !! (x + 1) == 'M')
          && (grid !! (y + 2) !! (x + 2) == 'A')
          && (grid !! (y + 3) !! (x + 3) == 'S')
       )
 where
  lengthY = length grid
  lengthX = length (head grid)
testRightDown = not (checkRightDown testGrid (0, 0)) && checkRightDown testGrid (4, 0)

checkLeftUp grid (x, y) =
  (fitLeft x && fitUp y)
    && ( (grid !! y !! x == 'X')
          && (grid !! (y - 1) !! (x - 1) == 'M')
          && (grid !! (y - 2) !! (x - 2) == 'A')
          && (grid !! (y - 3) !! (x - 3) == 'S')
       )
 where
  lengthY = length grid
  lengthX = length (head grid)

testLeftUp = not (checkLeftUp testGrid (9, 3)) && checkLeftUp testGrid (6, 5)

checkLeftDown grid (x, y) =
  (fitLeft x && fitDown y lengthY)
    && ( (grid !! y !! x == 'X')
          && (grid !! (y + 1) !! (x - 1) == 'M')
          && (grid !! (y + 2) !! (x - 2) == 'A')
          && (grid !! (y + 3) !! (x - 3) == 'S')
       )
 where
  lengthY = length grid
  lengthX = length (head grid)

testLeftDown = not (checkLeftDown testGrid (5, 0)) && checkLeftDown testGrid (9, 3)

checkMas :: [[Char]] -> (Int, Int) -> Int
checkMas [] (x, y) = 0
checkMas grid (x, y) =
  count
    [ checkRight grid (x, y)
    , checkLeft grid (x, y)
    , checkUp grid (x, y)
    , checkDown grid (x, y)
    , checkRightUp grid (x, y)
    , checkRightDown grid (x, y)
    , checkLeftUp grid (x, y)
    , checkLeftDown grid (x, y)
    ]
 where
  lengthY = length grid
  lengthX = length (head grid)

solve input =
  let lengthX = length (head grid)
      lengthY = length grid
   in sum ([checkMas grid (x, y) | x <- [0 .. (lengthX - 1)], y <- [0 .. (lengthY - 1)]])
 where
  grid = lines input

test = solve testInput == 18

main = do
  input <- getContents
  print (solve input)
