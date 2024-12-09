import Data.Char (digitToInt)
import Data.List.Split (chunksOf)

main = do
  input <- getContents
  print (solve input)

-- part 1

testInput = "2333133121414131402"
testDisk = expandDisk testInput
testInput' = "12345"
testDisk' = expandDisk testInput'

expandEntry _ [] = []
expandEntry id [files] = replicate files id
expandEntry id [files, free] = replicate files id ++ replicate free (-1)

mapIndex :: (Int -> a -> b) -> [a] -> [b]
mapIndex f = zipWith f [0 ..]

expandDisk input = concat $ mapIndex expandEntry chunks
 where
  chunks = chunksOf 2 (map digitToInt input)

defrag disk
  | move == (-1) = defrag (init disk)
  | null post = disk
  | otherwise = defrag (pre ++ [move] ++ tail (init post))
 where
  move = last disk
  (pre, post) = break (== (-1)) disk

checksum disk = sum (mapIndex (*) disk)

solve input = checksum (defrag (expandDisk input))

test = solve testInput == 1928
