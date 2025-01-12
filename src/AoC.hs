module AoC ( getDataFileName ) where

import System.Environment

getDataFileName :: IO String
getDataFileName =
  do args <- getArgs
     progName <- getProgName
     let baseDataName =  if null args
                         then progName
                         else head args 
     let baseDataName' =  if length baseDataName < 3
                          then progName ++ baseDataName
                          else baseDataName
     let dataFileName = "./data/" ++ baseDataName' ++ ".txt"
     return dataFileName

split :: (Char) -> String -> [String]
split p s = case dropWhile p' s of
    "" -> []
    s' -> w : split p s''
            where (w, s'') = break p' s'
    where p' = (==p)
