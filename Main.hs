{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Data.List.Split(splitOn)
import qualified GHC.List as L
import qualified Data.Vector.Unboxed as V
import GHC.Arr (Array (Array), array,(!))
import GHC.Base (join)


main :: IO ()
main = do
    rulesFile <- readFile "puzzle.csv"
    let rules = getRules rulesFile
    print rules
    let solution = recursiveCheck initialiseBoard rules
    print solution

initialiseBoard :: V.Vector Int
initialiseBoard = V.fromList (replicate 81 0)

getRules :: String -> Array Int [Int]
getRules s =
    let l = (map (map (read::String->Int) . splitOn ",") . lines) s
        f =  map (\l -> map (, l) (tail l)) l
    in  array (0, 80) (concat f)

relatedCells :: Array Int [Int]
relatedCells = array (0, 80) (map (\x ->  (x, getRelatedCells x)) [0..80])
    where
        sameRow i j = i `quot` 9 == j `quot` 9
        sameCol i j = (i - j) `mod` 9 == 0
        sameBlock i j = (i `quot` 27 == j `quot` 27) && (i `rem` 9 `quot` 3 == j `rem` 9 `quot` 3)
        isRelated cell el = cell /= el && (sameRow cell el || sameCol cell el || sameBlock cell el)
        getRelatedCells cell = filter (isRelated cell) [0..80]

recursiveCheck :: V.Vector Int -> Array Int [Int] -> V.Vector Int
recursiveCheck board rules =
    let mCell = V.elemIndex 0 board
    in case mCell of
        Nothing -> board
        Just i ->
            head (map attempt [1..9])
            where
                newBoard m = board V.// [( board V.! i,m)]
                validAttempt m = L.elem m (map (board V.!) (relatedCells!i))
                                && last (rules!i) /= i
                attempt m =
                    if validAttempt m
                        then recursiveCheck (newBoard m) rules
                        else board



