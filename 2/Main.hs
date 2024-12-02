import System.IO

parseNumber :: String -> Int
parseNumber s = case reads s of
    [(n, "")] -> n

getDifferences :: [Int] -> [Int]
getDifferences xs = zipWith (-) (tail xs) xs

isValidDiff :: Int -> Bool
isValidDiff x = abs x >= 1 && abs x <= 3

isValidSequence :: [Int] -> Bool
isValidSequence xs = 
    let diffs = getDifferences xs
    in all isValidDiff diffs && (all (>0) diffs || all (<0) diffs)

removeOne :: [a] -> [[a]]
removeOne xs = [take i xs ++ drop (i + 1) xs | i <- [0..length xs - 1]]

isValidSequenceWithTolerance :: [Int] -> Bool
isValidSequenceWithTolerance xs = isValidSequence xs || any isValidSequence (removeOne xs)

countTrues :: [Bool] -> Int
countTrues = length . filter id

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let linesOfFile = lines contents
        parsed_data = map (map parseNumber . words) linesOfFile
        levels = map isValidSequence parsed_data
        result1 =  countTrues levels
        levels2 = (map (isValidSequenceWithTolerance . snd) . filter (not . fst)) $ zip levels parsed_data
        result2 =  result1 + countTrues levels2
    print result1
    print result2