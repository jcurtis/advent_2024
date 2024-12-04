import Data.Text (breakOn, empty, pack, unpack)
import Text.Regex.TDFA ((=~))

testInput = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

-- part 1

regex = "mul\\(([0-9]+),([0-9]+)\\)"

match input = input =~ regex :: (String, String, String, [String])

matches :: String -> [(Int, Int)]
matches "" = []
matches input
  | null subs = []
  | otherwise = (read (head subs), read (last subs)) : matches rest
 where
  (_, _, rest, subs) = match input

mult (a, b) = a * b

solve input =
  let instructions = matches input
   in sum (map mult instructions)

test = solve testInput == 161

-- part 2
testInput' = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

dont = pack "don't()"
doPack = pack "do()"

breakOnDo input = unpack b
 where
  (_, b) = breakOn doPack input

solve' input
  | b == empty = solve (unpack a)
  | otherwise = solve (unpack a) + solve' (breakOnDo b)
 where
  (a, b) = breakOn dont (pack input)

test' = solve' testInput' == 48

main = do
  if test then print "test passed" else print "test failed"
  if test' then print "test' passed" else print "test' failed"
  input <- getContents
  print (solve input)
  print (solve' input)
