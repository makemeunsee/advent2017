module Main where

import Lib
import Data.Char (digitToInt)
import System.Environment (getArgs)

matchesNext :: Eq a => [a] -> [Bool]
matchesNext [] = []
matchesNext (x : xs) = matchesNext' x ( x : xs )
	where
		matchesNext' :: Eq a => a -> [a] -> [Bool]
		matchesNext' h [x] = [h == x]
		matchesNext' h (x : x' : xs) = (x == x') : (matchesNext' h (x' : xs))

halfwayAround :: [a] -> [a]
halfwayAround [] = []
halfwayAround xs = (drop n xs) ++ (take n xs)
	where
		n = length xs `div` 2

advent1 :: IO ()
advent1 = do
	args <- getArgs
	let intList = fmap digitToInt (foldl (++) "" args)
	let matchFlags = matchesNext intList
	let result = sum $ map fst $ filter snd $ zip intList matchFlags
	(putStrLn . show ) result
	let result2 = sum $ map fst $ filter (\(x,y) -> x == y) $ zip intList (halfwayAround intList)
	(putStrLn . show ) result2

main :: IO ()
main = advent1