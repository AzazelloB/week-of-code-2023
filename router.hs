import Data.List (sortOn)

data Route = Route {
  from :: String,
  to :: String,
  price :: Int,
  time :: Int,
  oneWay :: Bool
} deriving (Show, Eq)

data Result = Result {
  route :: [String],
  totalTime :: Int,
  totalCost :: Int
} deriving (Show)

routes :: [Route]
routes = [
  Route { from = "A", to = "B", price = 15, time = 20, oneWay = True },
  Route { from = "A", to = "C", price = 45, time = 60, oneWay = False },
  Route { from = "B", to = "C", price = 25, time = 40, oneWay = False },
  Route { from = "B", to = "F", price = 30, time = 30, oneWay = False },
  Route { from = "C", to = "D", price = 60, time = 60, oneWay = False },
  -- E was changed to one way
  -- because in the example B -> A is shown as impossible
  -- but it is possible with B -> C -> A
  Route { from = "E", to = "F", price = 40, time = 20, oneWay = True },
  Route { from = "D", to = "F", price = 80, time = 50, oneWay = False },
  Route { from = "C", to = "G", price = 60, time = 60, oneWay = False },
  Route { from = "D", to = "G", price = 10, time = 50, oneWay = True },
  Route { from = "F", to = "G", price = 80, time = 30, oneWay = False }
  ]

getRoute :: String -> String -> [Route] -> Result
getRoute from' to' routes =
  case paths of
    [] -> Result [] 0 0
    _  -> head $ sortOn (\(Result _ t c) -> (t + c, c, t, length routes)) paths
  where
    allRoutes = routes ++ [ Route (to r) (from r) (price r) (time r) True | r <- routes, not $ oneWay r ]
    paths = [ Result (map from path ++ [to $ last path]) (sum $ map time path) (sum $ map price path) | path <- findPaths from' to' allRoutes ]

findPaths :: String -> String -> [Route] -> [[Route]]
findPaths from' to' routes = [ path | route <- routes, from route == from', path <- generatePaths route [route] ]
  where
    generatePaths :: Route -> [Route] -> [[Route]]
    generatePaths current path
      | to current == to' = [path]
      | otherwise = concat [ generatePaths next (path ++ [next]) | next <- routes, from next == to current, next `notElem` path ]

main :: IO ()
main = do
  putStrLn "Test 1:"
  putStrLn $ "\tExpected: " ++ show (Result ["A", "B"] 20 15)
  putStrLn $ "\tActual: " ++ show (getRoute "A" "B" routes)

  putStrLn "Test 2:"
  putStrLn $ "\tExpected: " ++ show (Result [] 0 0)
  putStrLn $ "\tActual: " ++ show (getRoute "A" "E" routes)

  putStrLn "Test 3:"
  putStrLn $ "\tExpected: " ++ show (Result ["A", "B", "F"] 50 50)
  putStrLn $ "\tActual: " ++ show (getRoute "A" "F" routes)

  putStrLn "Test 4:"
  putStrLn $ "\tExpected: " ++ show (Result ["B", "F", "G"] 60 110)
  putStrLn $ "\tActual: " ++ show (getRoute "B" "G" routes)

  putStrLn "Test 5:"
  putStrLn $ "\tExpected: " ++ show (Result ["G", "C", "D"] 120 120)
  putStrLn $ "\tActual: " ++ show (getRoute "G" "D" routes)
