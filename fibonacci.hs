fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibonacci :: Integer -> Integer
fibonacci n = fibs !! fromIntegral n

main :: IO ()
main = do
  print "Enter a number:"
  n <- readLn :: IO Integer
  print $ "The " ++ show n ++ "th Fibonacci element is " ++ show (fibonacci n) ++ "."
