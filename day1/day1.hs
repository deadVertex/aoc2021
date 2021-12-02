import System.IO

testDepths = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263] :: [Int]
largerThanPrevious :: [Int] -> Int
largerThanPrevious depths =
    let compare = uncurry (>)
        results = zipWith (curry compare) (tail depths) depths
    in length (filter (==True) results)

slidingWindowSum :: [Int] -> [Int]
slidingWindowSum [] = []
slidingWindowSum xs = sum (take 3 xs) : slidingWindowSum (tail xs)

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let readInt x = read x :: Int
        values = map readInt (lines contents)
    print (largerThanPrevious (slidingWindowSum values))