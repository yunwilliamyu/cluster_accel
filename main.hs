-- Compressive acceleration function
-- Copyright (c) 2014, Yun William Yu
--

import Text.EditDistance -- For levenshteinDistance (cabal install edit-distance)
--import Data.Random.Extras -- (cabal install random-extras)
import Data.Random
import Data.Random.Source.Std
import Data.List

--import Criterion.Main  -- for timing benchmarks (cabal install criterion)

import Text.Printf
import Control.Exception
import System.CPUTime

-----------------------------------------------------------------------------------
-- |"gen_database" Takes a distance function and a list and generates the clustered database
gen_database :: (Integral b) => (a -> a -> b) -> b -> [a] -> [[a]]
gen_database _ _ [] = []
gen_database d thresh input = [x | x<-input, (d x rep)<=thresh]:(gen_database d thresh [x | x<-input, (d x rep)>thresh])
    where rep = head input

-- |The 'accel_search' function takes a distance function, a clustered database, a coarse radius, a fine radius, and a target, and returns a list of all matching
accel_search :: (Integral b) => (a -> a -> b) -> [[a]] -> b -> b -> a -> [a]
accel_search d db cr fr t = concat [[x|x<-dbx, (d x t)<=fr] | dbx <- db, d (head dbx) t <= cr]
-----------------------------------------------------------------------------------

time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v

shuffle_list x = runRVar (shuffle x) StdRandom

split :: (Eq c) => c -> [c] -> [[c]]
split _ [] = []
-- split d (s:d:t) = s:(split d (s:d:t))
split d x = a:(if b==[] then [] else split d (tail b))
    where (a,b) = span (/= d) x

delfromlist :: (Eq c) => c -> [c] -> [c]
delfromlist d x = [a | a<-x, a /= d]


main = do
    str <- readFile "6of12.txt"
    let processed_str = (split '\n' (delfromlist '&' (delfromlist '#' (delfromlist '<' (delfromlist '^' (delfromlist '=' (delfromlist '+' (delfromlist ':' ( delfromlist '\r' str)))))))))
    --input <- shuffle_list processed_str
    let input = processed_str
    let database = (gen_database (levenshteinDistance defaultEditCosts) 5 input)
    --print database
    time $ print [(length database),(length str)]
    time $ print (sort (accel_search (levenshteinDistance defaultEditCosts) database 5 3 "happiness"))
    time $ print ([x|x<-input, levenshteinDistance defaultEditCosts x "happiness" <= 3])

    --print (levenshteinDistance defaultEditCosts "hello, world" "hello, woldr")
