import Control.Monad (replicateM)
import Data.List (permutations)

testInput = "190: 10 19\n3267: 81 40 27\n83: 17 5\n156: 15 6\n7290: 6 8 6 15\n161011: 16 10 13\n192: 17 8 14\n21037: 9 7 18 13\n292: 11 6 16 20"
testEquations = parse testInput
testEquation = head testEquations
testValid = parseEquation "3267: 81 40 27"
testInvalid = parseEquation "7290: 6 8 6 15"

-- part 1

parseEquation :: String -> (Integer, [Integer])
parseEquation input = (read (init result), map read operands)
 where
  (result : operands) = words input

parse input = map parseEquation equations
 where
  equations = lines input

applyPerm [x] _ = x
applyPerm (x : _) [] = x
applyPerm (a : b : operands) (operator : operators) = applyPerm ((a `operator` b) : operands) operators

validEquation _ [] _ = False
validEquation result operands operators = or [applyPerm operands perm == result | perm <- perms]
 where
  perms = replicateM (length operands) operators

validEquation1 result operands = validEquation result operands [(+), (*)]

solve input = sum (map fst (filter (uncurry validEquation1) equations))
 where
  equations = parse input

test = solve testInput == 3749

main = do
  input <- getContents
  print (solve input)
  print (solve' input)

con a b = read (show a ++ show b)

validEquation2 result operands = validEquation result operands [(+), (*), con]

solve' input = sum (map fst (filter (uncurry validEquation2) equations))
 where
  equations = parse input

test' = solve' testInput == 11387
