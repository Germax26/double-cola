{-# LANGUAGE TupleSections #-}
main :: IO ()
main = do
    putStrLn $ "1 => " ++ dc queue 1 -- Sheldon
    putStrLn $ "52 => " ++ dc queue 52 -- Penny
    putStrLn $ "10010 => " ++ dc queue 10010 -- Howard
        where queue = ["Sheldon", "Leonard", "Penny", "Rajesh", "Howard"]

appendToEnd :: [a] -> a -> [a]
appendToEnd [] a = [a]
appendToEnd (a:as) b = a : appendToEnd as b

dcImpl :: [(Integer, String)] -> Integer -> String
dcImpl ((m,p):xs) n = if m >= n
    then p
    else dcImpl (appendToEnd xs (2*m,p)) $ n - m
dcImpl [] _  = error "Unreachable"

dc :: [String] -> Integer -> String
dc xs = dcImpl $ map (1,) xs