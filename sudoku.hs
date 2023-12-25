import Data.Char (digitToInt)

type Grid = [[Int]]

splitIntoChunks :: Int -> [a] -> [[a]]
splitIntoChunks _ [] = []
splitIntoChunks size list = take size list : splitIntoChunks size (drop size list)

isValid :: Grid -> Bool
isValid field = undefined

resolveSudoku :: Grid -> Grid
resolveSudoku field = if isValid attempt then resolveSudoku attempt else backtrack
  where
    possible = [1, 2, 3, 4, 5, 6, 7, 8, 9]
    -- fill only one cell
    attempt = [[if cell == 0 then head possible else cell | cell <- row] | row <- field]

main :: IO ()
main = do
  -- masked: ....8...7.8.....155...364.......436..2..6..8...72.8...6....1....4....9..7........
  -- masked: 000080007080000015500036400000004360020060080007208000600001000040000900700000000
  -- solved: 416589237983427615572136498859714362124365789367298541635971824241853976798642153
  putStrLn "Test 1:"
  let expected = "416589237983427615572136498859714362124365789367298541635971824241853976798642153"
  putStrLn $ "\tExpected: " ++ expected
  let actual = concatMap (concatMap show) $ resolveSudoku $ splitIntoChunks 9 $ map digitToInt "000080007080000015500036400000004360020060080007208000600001000040000900700000000"
  putStrLn $ "\tActual: " ++ show actual
  putStrLn $ "\tSame?: " ++ show (expected == actual)
