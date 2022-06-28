module Main where

data Group = Group 
    { groupSize :: Int
    , groupName :: String
    } deriving Show

doubleGroup :: Group -> Group
doubleGroup (Group size name) = Group (2 * size) name

names :: [String]
names = ["Sheldon", "Leonard", "Penny", "Rajesh", "Howard"]

groups :: [Group]
groups = map (Group 1) names ++ map doubleGroup groups

nth :: Int -> [Group] -> String
nth _ [] = error "unreachable"
nth n (Group size name:rest)
    | n <= size = name
    | otherwise = nth (n - size) rest

main :: IO ()
main = do
    putStrLn $ "1     => " ++ nth 1 groups -- Sheldon
    putStrLn $ "52    => " ++ nth 52 groups -- Penny
    putStrLn $ "10010 => " ++ nth 10010 groups -- Howard