import Data.List (transpose)
import Data.Char (digitToInt)
import Data.Maybe (listToMaybe, mapMaybe, fromMaybe)

type Grid = [[Int]]
type Position = (Int, Int)

splitIntoChunks :: Int -> [a] -> [[a]]
splitIntoChunks _ [] = []
splitIntoChunks size list = take size list : splitIntoChunks size (drop size list)

isValid :: Grid -> Int -> (Int, Int) -> Bool
isValid grid num pos = inRow grid num pos && inCol grid num pos && inBox grid num pos

inRow :: Grid -> Int -> (Int, Int) -> Bool
inRow grid num (row, _) = num `notElem` (grid !! row)

inCol :: Grid -> Int -> (Int, Int) -> Bool
inCol grid num (_, col) = num `notElem` (transpose grid !! col)

inBox :: Grid -> Int -> (Int, Int) -> Bool
inBox grid num (row, col) = num `notElem` box
  where
    startRow = row - row `mod` 3
    startCol = col - col `mod` 3
    box = concat $ take 3 $ drop startRow $ map (take 3 . drop startCol) grid

emptyCell :: Grid -> Maybe (Int, Int)
emptyCell grid = case [(row, col) | row <- [0..8], col <- [0..8], grid !! row !! col == 0] of
  [] -> Nothing
  (x:_) -> Just x

resolveSudoku :: Grid -> Grid
resolveSudoku grid = fromMaybe grid $ go grid
  where
    go g = case emptyCell g of
      Nothing -> Just g
      Just pos -> listToMaybe $ mapMaybe (\num -> if isValid g num pos then go (placeNum g num pos) else Nothing) [1..9]

placeNum :: Grid -> Int -> (Int, Int) -> Grid
placeNum grid num (row, col) = zipWith (\ r rowVals
  -> (if r == row then replace col num rowVals else rowVals)) [0..] grid

replace :: Int -> a -> [a] -> [a]
replace _ _ [] = []
replace i x (y:ys)
  | i == 0 = x : ys
  | otherwise = y : replace (i-1) x ys

main :: IO ()
main = do
  -- masked: 000080007080000015500036400000004360020060080007208000600001000040000900700000000
  -- solved: 416589237983427615572136498859714362124365789367298541635971824241853976798642153
  putStrLn "Test 1:"
  let expected = "416589237983427615572136498859714362124365789367298541635971824241853976798642153"
  putStrLn $ "\tExpected: " ++ expected
  let actual = concatMap (concatMap show) $ resolveSudoku $ splitIntoChunks 9 $ map digitToInt "000080007080000015500036400000004360020060080007208000600001000040000900700000000"
  putStrLn $ "\tActual: " ++ actual
  putStrLn $ "\tSame?: " ++ show (expected == actual)
