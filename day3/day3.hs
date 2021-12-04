import Test.HUnit
import System.IO

testInput = ["00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010"]

toBit :: Char -> Int
toBit '0' = 0
toBit '1' = 1

bitn :: String -> Int -> Int
bitn xs n = toBit (xs !! n)

transpose :: [String] -> [[Int]]
transpose [] = []
transpose xs = 
    let n = length (head xs)
    in [[bitn x y | x <- xs] | y <- [0..n-1]]

-- TODO: What about ties?
mostCommon :: [Int] -> Int
mostCommon xs = if sum xs > div (length xs) 2 then 1 else 0

toInt :: [Int] -> Int
toInt xs = 
    let n = length xs
        r = reverse xs
        a = [if (r !! y) == 1 then (2 ^ y) :: Int else 0 | y <- [0..n-1]]
    in sum a

calculateGamma :: [[Int]] -> Int
calculateGamma xs =
    let a = [mostCommon x | x <- xs]
    in toInt a

calculateEpsilon :: [[Int]] -> Int
calculateEpsilon xs =
    let a = [if mostCommon x == 1 then 0 else 1| x <- xs]
    in toInt a

calculatePowerConsumption :: [[Int]] -> Int
calculatePowerConsumption xs =
    let a = calculateGamma xs
        b = calculateEpsilon xs
    in a * b

-- Application
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    print (calculatePowerConsumption (transpose (lines contents)))

-- Unit Tests
test1 = TestCase (assertEqual "transpose" [[1,0,1], [0,0,1], [1,1,0]] (transpose ["101", "001", "110"]))
test2 = TestCase (assertEqual "mostCommon 1" 1 (mostCommon [1, 0, 1]))
test3 = TestCase (assertEqual "mostCommon 0" 0 (mostCommon [0, 0, 1]))
test4 = TestCase (assertEqual "toInt 101" 5 (toInt [1, 0, 1]))
test5 = TestCase (assertEqual "toInt 100" 4 (toInt [1, 0, 0]))
test6 = TestCase (assertEqual "calculateGamma" 5 (calculateGamma [[1,0,1], [0,0,1], [1,1,0]]))
test7 = TestCase (assertEqual "calculatePowerConsumption" 10 (calculatePowerConsumption [[1,0,1], [0,0,1], [1,1,0]]))
test8 = TestCase (assertEqual "calculateEpsilon" 2 (calculateEpsilon [[1,0,1], [0,0,1], [1,1,0]]))
test9 = TestCase (assertEqual "calculatePowerConsumption testInput" 198 (calculatePowerConsumption (transpose testInput)))

tests = TestList [TestLabel "transpose" test1,
                  TestLabel "mostCommon 1" test2,
                  TestLabel "mostCommon 0" test3,
                  TestLabel "toInt 101" test4,
                  TestLabel "toInt 100" test5,
                  TestLabel "calculateGamma" test6,
                  TestLabel "calculatePowerConsumption" test7,
                  TestLabel "calculateEpsilon" test8,
                  TestLabel "calculatePowerConsumption testInput" test9]