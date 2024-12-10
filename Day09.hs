import Data.Char (digitToInt)
import Data.List.Extra (breakEnd, breakOn)
import Data.List.Split (chunksOf)

main = do
  input <- getContents
  -- print (solve input)
  print (solve' input)

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

op _ (-1) = 0
op idx id = idx * id
checksum disk = sum (mapIndex op disk)

solve input = checksum (defrag (expandDisk input))

test = solve testInput == 1928

-- part 2

defragId id disk
  | id <= 0 = disk
  | null postDisk = defragId (id - 1) disk
  | otherwise = defragId (id - 1) (preDisk ++ file ++ drop fileLen postDisk ++ emptyness ++ end)
 where
  (start, mid) = break (== id) disk
  (file, end) = breakEnd (== id) mid
  fileLen = length file
  emptyness = replicate fileLen (-1)
  (preDisk, postDisk) = breakOn emptyness start

solve' input = checksum $ defragId (last disk) disk
 where
  disk = expandDisk input

test' = solve' testInput == 2858
