import AoC

main :: IO ()
main = do
    datafilename <- getDataFileName
    text <- readFile datafilename
    let masses = fmap read $ lines text
    print $ part1 masses
    print $ part2 masses

part1,part2 :: [Int] -> Int
part1 masses = foldr (+) 0 fuels
    where fuels = fmap get_fuel $ masses

part2 masses = foldr (+) 0 fuels
    where
    fuels = fmap add_fuel $ fmap get_fuel $ masses 
    

get_fuel :: Int -> Int
get_fuel mass = mass `div` 3 - 2


add_fuel :: Int -> Int
add_fuel (fuel) 
    | fuel < 9 = fuel
    | otherwise = fuel + add_fuel (get_fuel fuel)
