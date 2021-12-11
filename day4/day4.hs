import Test.HUnit
import System.IO

testSequence = [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1]
testBoard1 = 
    [
    22, 13, 17, 11,  0,
    8,  2, 23,  4, 24,
    21,  9, 14, 16,  7,
    6, 10,  3, 18,  5,
    1, 12, 20, 15, 19
    ]

testBoard2 = 
    [
    3, 15,  0,  2, 22,
    9, 18, 13, 17,  5,
    19,  8,  7, 25, 23,
    20, 11, 10, 24,  4,
    14, 21, 16, 12,  6
    ]

testBoard3 =
    [
    14, 21, 17, 24,  4,
    10, 16, 15,  9, 19,
    18,  8, 23, 26, 20,
    22, 11, 13,  6,  5,
    2,  0, 12,  3,  7
    ]

testBoards = [testBoard1, testBoard2, testBoard3]

-- Pseudo code
-- 0. Build list of (boardIndex, [rows]:[columns]), 10 lists per board and ~30 boards so worst case processing 300 (5 element) lists per iteration
-- 1. Get number from the sequence
-- 1.5 Store number received from sequence in replayList
-- 2. Mark numbers? Filter out number from [rows]:[columns]
-- 3. When bingo is achieved - Check if we have any empty [rows] or [columns] for each board
--- 4. Find all unmarked numbers from that board - Concat only the remaining [rows] to get list of unmarked numbers to sum
--- 5. Sum unmarked numbers and muliply by last number received

data BingoBoard = BingoBoard { rows :: [[Int]]
                             , columns :: [[Int]]
                             } deriving (Show)

getRows :: [Int] -> [[Int]]
getRows [] = []
getRows xs = (take 5 xs) : (getRows (drop 5 xs))

getColumns :: [[Int]] -> [[Int]]
getColumns xs = 
    let n = length (head xs)
    in [[x !! y | x <- xs] | y <- [0..n-1]]

createBoard :: [Int] -> BingoBoard
createBoard xs = 
    let rows = getRows xs
        columns = getColumns rows
    in BingoBoard rows columns

markNumber :: BingoBoard -> Int -> BingoBoard
markNumber board n =
    let r = [ filter (/=n) xs | xs <- rows board ]
        c = [ filter (/=n) xs | xs <- columns board ]
    in BingoBoard r c
    
-- Application
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    --print (calculatePowerConsumption (transpose (lines contents)))
    --print (calculateLifeSupportRating (map binary (lines contents)))
    print("test")

-- Unit Tests
test1 = TestCase (assertEqual "getRows" [[1, 2, 3, 4, 5], [6, 7, 8, 9, 10]] (getRows [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]))
test2 = TestCase (assertEqual "getColumns" [[1,6],[2,7],[3,8],[4,9],[5,10]] (getColumns (getRows [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])))
test3 = TestCase (assertEqual "markNumber" [[1, 3, 4, 5], [6, 7, 8, 9, 10]] (rows (markNumber (createBoard [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) 2)))

tests = TestList [TestLabel "getRows" test1,
                  TestLabel "getColumns" test2,
                  TestLabel "markNumber" test3]