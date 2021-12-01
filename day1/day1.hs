import System.IO

testDepths = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]
largerThanPrevious :: [Int] -> Int
largerThanPrevious depths =
    let compare = uncurry (>)
        results = zipWith (curry compare) (tail depths) depths
    in length (filter (==True) results)

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let readInt x = read x :: Int
        values = map readInt (lines contents)
    print (largerThanPrevious values)