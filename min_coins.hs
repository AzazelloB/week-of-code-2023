import Data.List (delete, sort, minimumBy)
import Data.Function (on)

countCoins :: Int -> [Int] -> [Int]
countCoins sum coins = case countCoins' sum coins (maximum coins) [] of
  [] -> []
  combs -> minimumBy (compare `on` length) combs
  where
    countCoins' _ [] _ _ = []
    countCoins' 0 _ _ acc = [acc]
    countCoins' sum coins maxC acc
      | sum < maxC = countCoins' sum (delete maxC coins) (maximum coins) acc
      | otherwise = concatMap (\c -> countCoins' (sum - c) coins c (c:acc)) coins

main :: IO ()
main = do
  print "Sum:"
  sum <- readLn :: IO Int
  print "Coin values (space separated):"
  coins <- map read . words <$> getLine :: IO [Int]
  print "Coins:"
  print $ countCoins sum coins
