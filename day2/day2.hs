import Test.HUnit
import System.IO

convert :: [String] -> (String, Int)
convert xs = 
    (head xs, read (last xs) :: Int)

data Coordinate = Coordinate { horizontal :: Int
                             , depth :: Int
                             , aim :: Int
                             } deriving (Show, Eq)

applyCommand :: (String, Int) -> Coordinate -> Coordinate
applyCommand ("forward", a) x = Coordinate (horizontal x + a) (depth x + aim x * a) (aim x)
applyCommand ("up", a) x = Coordinate (horizontal x) (depth x) (aim x - a)
applyCommand ("down", a) x = Coordinate (horizontal x) (depth x) (aim x + a)

sumCommands :: [(String, Int)] -> Coordinate -> Coordinate
sumCommands [] y = y
sumCommands xs y = sumCommands (tail xs) (applyCommand (head xs) y)

computeAnswer :: [String] -> Int
computeAnswer xs =
    let tokens = map words xs
        actions = map convert tokens
        x = sumCommands actions (Coordinate 0 0 0)
    in horizontal x * depth x

-- Application
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    print (computeAnswer (lines contents))

-- Unit tests and test input
testInput = ["forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2"]

test1 = TestCase (assertEqual "command (forward, 2)" (Coordinate 4 1 0) (applyCommand ("forward", 2) (Coordinate 2 1 0)))
test2 = TestCase (assertEqual "command (down, 3)" (Coordinate 2 1 5) (applyCommand ("down", 3) (Coordinate 2 1 2)))
test3 = TestCase (assertEqual "command (up, 1)" (Coordinate 2 1 1) (applyCommand ("up", 1) (Coordinate 2 1 2)))
test4 = TestCase (assertEqual "sumCommands" (Coordinate 5 0 0) (sumCommands [("forward", 1), ("forward", 3)] (Coordinate 1 0 0)))
test5 = TestCase (assertEqual "computeAnswer" 900 (computeAnswer testInput))

test6 = TestCase (assertEqual "command (forward, 2) with aim" (Coordinate 4 5 2) (applyCommand ("forward", 2) (Coordinate 2 1 2)))

tests = TestList [TestLabel "test1" test1,
                  TestLabel "test2" test2,
                  TestLabel "test3" test3,
                  TestLabel "test4" test4,
                  TestLabel "test5" test5,
                  TestLabel "test6" test6]