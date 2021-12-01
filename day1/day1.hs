--import System.IO

depths2 = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]
larger_than_previous :: [Integer] -> Int
larger_than_previous depths =
    let compare x = fst x > snd x
        results = map compare (zip (tail depths) depths)
    in length (filter (==True) results)

-- main = do
--    handle <- openFile "input.txt" ReadMode
--    contents <- hGetContents handle
--    let values = lines contents
--    map putStrLn values