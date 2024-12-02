import System.IO
import Data.Char (isDigit, digitToInt)
import Data.List (sort, transpose)  -- Add transpose

parseNumber :: String -> Int
parseNumber s = case reads s of
    [(n, "")] -> n

count :: Int -> [Int] -> Int  
count x = length . filter (== x)

simScoreSum :: [Int] -> [Int] -> Int
simScoreSum xs ys = sum [abs (x - y) | (x, y) <- zip xs ys]

weightedSum :: [Int] -> [Int] -> Int
weightedSum xs ys = sum [x * count x ys | x <- xs]

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let linesOfFile = lines contents
        parsed_data =  map sort $ transpose $ map (map parseNumber . words) linesOfFile
        result1 = simScoreSum (head parsed_data) (last parsed_data)
        result2 = weightedSum (head parsed_data) (last parsed_data)
    print result1
    print result2
