{-# OPTIONS_GHC -Wno-name-shadowing #-}

import AoC
import Data.Map (Map)
import Data.Map qualified as Map
import Debug.Trace

debug = flip Debug.Trace.trace

main :: IO ()
main = do
  datafilename <- getDataFileName
  -- let datafilename = "data/day2_test.txt"
  text <- readFile datafilename
  let program_int = fmap read2Int $ words text
  let program_map_init = Map.fromList (enumerate program_int)
  print $ "Start: " ++ show (Map.elems program_map_init)
  -- IMPORTANT
  let program_map1 = Map.insert 1 12 program_map_init
  let program_map2 = Map.insert 2 2 program_map1

  let final = Map.elems (part1 program_map2 0)
  print $ "End: " ++ show final
  print $ "Answer: " ++ show (final !! 0)

read2Int :: String -> Int
read2Int i = (read i :: Int)

enumerate :: [k] -> [(Int, k)]
enumerate x = zip [0 ..] x

apply_opt :: Int -> (Int -> Int -> Int) -> Map.Map Int Int -> Map.Map Int Int
apply_opt k op program = do
  let keys = map (\x -> Map.findWithDefault 0 x program) [k + 1 .. k + 3]
      value1 = Map.findWithDefault 0 (keys !! 0) program -- `debug` ("2" ++ show (Map.elems program))
      value2 = Map.findWithDefault 0 (keys !! 1) program
  Map.insert (keys !! 2) (op value1 value2) program `debug` ("keys: " ++ show keys ++ "->" ++ show [value1, value2])

part1 :: (Ord Int) => Map Int Int -> Int -> Map Int Int
part1 program pos
  | optcode == 1 = part1 (apply_opt pos (+) program) (pos + 4)
  | optcode == 2 = part1 (apply_opt pos (*) program) (pos + 4)
  | optcode == 99 = program
  | otherwise = Map.empty -- `debug` ("optcode: " ++ show optcode)
  where
    optcode = Map.findWithDefault 99 pos program -- `debug` ("pos: " ++ show pos)
