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

main = do
  input <- getContents
  print (solve input)
