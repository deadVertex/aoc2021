import Test.HUnit
import System.IO

convert :: [String] -> (String, Int)
convert xs = 
    (head xs, read (last xs) :: Int)

delta :: (String, Int) -> (Int, Int)
delta ("forward", a) = (a, 0)
delta ("up", a) = (0, -a)
delta ("down", a) = (0, a)

process :: [String] -> [(Int, Int)]
process xs = 
    let tokens = map words xs
        actions = map convert tokens
    in map delta actions

sumComponents :: [(Int, Int)] -> (Int, Int)
sumComponents xs =
    let a = unzip xs
    in (sum (fst a), sum (snd a))

computeAnswer :: [String] -> Int
computeAnswer xs =
    let (a, b) = sumComponents (process xs)
    in a * b

-- Application
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    print (computeAnswer (lines contents))

-- Unit tests and test input
testInput = ["forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2"]

test1 = TestCase (assertEqual "delta (forward, 5)" (5, 0) (delta ("forward", 5)))
test2 = TestCase (assertEqual "delta (down, 5)" (0, 5) (delta ("down", 5)))
test3 = TestCase (assertEqual "delta (up, 3)" (0, -3) (delta ("up", 3)))
test4 = TestCase (assertEqual "process" [(0,2)] (process ["down 2"]))
test5 = TestCase (assertEqual "sumComponents" (4, 6) (sumComponents [(1,2), (3,4)]))
test6 = TestCase (assertEqual "computeAnswer" 150 (computeAnswer testInput))


tests = TestList [TestLabel "test1" test1,
                  TestLabel "test2" test2,
                  TestLabel "test3" test3,
                  TestLabel "test4" test4,
                  TestLabel "test5" test5,
                  TestLabel "test6" test6 ]