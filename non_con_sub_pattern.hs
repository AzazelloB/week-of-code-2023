import Data.List (subsequences, isSubsequenceOf)
import Data.Char (toLower)

longest :: Foldable f => f a -> f a -> f a
longest xs ys = if length xs > length ys then xs else ys
shortest :: Foldable f => f a -> f a -> f a
shortest xs ys = if length xs > length ys then ys else xs

subpattern :: String -> String -> [String]
subpattern s1 s2
  | length s1 <= 3 = []
  | length s2 <= 3 = []
  | otherwise = filter (`isSubsequenceOf` is1) $ subsequences is2
    where
      is1 = map toLower $ shortest s1 s2
      is2 = map toLower $ longest s1 s2

main :: IO ()
main = do
  print "String 1:"
  str1 <- getLine
  print "String 2:"
  str2 <- getLine
  print "Subpatterns:"
  print $ subpattern str1 str2
