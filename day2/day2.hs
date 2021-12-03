import Test.HUnit
import System.IO

convert :: [String] -> (String, Int)
convert xs = 
    (head xs, read (last xs) :: Int)

command :: (String, Int) -> (Int, Int, Int)
command ("forward", a) = (a, 0, 0)
command ("up", a) = (0, 0, -a)
command ("down", a) = (0, 0, a)

process :: [String] -> [(Int, Int, Int)]
process xs = 
    let tokens = map words xs
        actions = map convert tokens
    in map command actions

sumComponents :: [(Int, Int, Int)] -> (Int, Int, Int)
sumComponents xs =
    let a = [ x | (x,y,z) <- xs]
        b = [ z | (x,y,z) <- xs]
    in (sum a, sum b, 0)

computeAnswer :: [String] -> Int
computeAnswer xs =
    let (a, b, _) = sumComponents (process xs)
    in a * b

-- Application
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    print (computeAnswer (lines contents))

-- Unit tests and test input
testInput = ["forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2"]

test1 = TestCase (assertEqual "command (forward, 5)" (5, 0, 0) (command ("forward", 5)))
test2 = TestCase (assertEqual "command (down, 5)" (0, 0, 5) (command ("down", 5)))
test3 = TestCase (assertEqual "command (up, 3)" (0, 0, -3) (command ("up", 3)))
test4 = TestCase (assertEqual "process" [(0, 0, 2)] (process ["down 2"]))
test5 = TestCase (assertEqual "sumComponents" (4, 6, 0) (sumComponents [(1, 0, 2), (3, 0, 4)]))
test6 = TestCase (assertEqual "computeAnswer" 150 (computeAnswer testInput))


tests = TestList [TestLabel "test1" test1,
                  TestLabel "test2" test2,
                  TestLabel "test3" test3,
                  TestLabel "test4" test4,
                  TestLabel "test5" test5,
                  TestLabel "test6" test6 ]