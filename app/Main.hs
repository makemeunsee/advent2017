module Main where

import Lib
import Text.Regex
import qualified Data.Maybe as Mbe
import Data.Maybe(Maybe(Just, Nothing))
import qualified Data.Text as Txt
import Data.Text (Text)
import qualified Data.Tree as Tree
import Data.Tree (Tree(Node))
import qualified Data.Char as Chr
import qualified Data.List as Lst
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List.Split (splitOn)
import System.Environment (getArgs)
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Data.Foldable (toList)
import qualified Data.List.Zipper as Zp
import Data.List.Zipper (Zipper(Zip))

import Debug.Trace (trace)

concatArgsX :: String -> String -> String -> [String] -> String
concatArgsX prefix delimiter suffix args = (++) prefix $ foldr (++) suffix $ Lst.intersperse delimiter args

concatArgs = concatArgsX "" "" ""

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
    args <- fmap concatArgs getArgs
    let intList = fmap Chr.digitToInt args
    let matchFlags = matchesNext intList
    let result = sum $ map fst $ filter snd $ zip intList matchFlags
    (putStrLn . show ) result
    let result2 = sum $ map fst $ filter (\(x,y) -> x == y) $ zip intList (halfwayAround intList)
    (putStrLn . show ) result2


stringToInts :: String -> [Int]
stringToInts = (fmap read) . (splitOn ",")

minAndMax :: (Int, Int) -> Int -> (Int, Int)
minAndMax (currentMin, currentMax) x = (min x currentMin, max x currentMax)

evenDivisions :: [Int] -> [Int]
evenDivisions xs = [ x `div` y | x <- xs, y <- xs, x > y, x `mod` y == 0]

hasDuplicate [] = False
hasDuplicate (x : xs) = elem x xs || hasDuplicate xs
    
-- advent2 input
-- 121,59,141,21,120,67,58,49,22,46,56,112,53,111,104,130 1926,1910,760,2055,28,2242,146,1485,163,976,1842,1982,137,1387,162,789 4088,258,2060,1014,4420,177,4159,194,2794,4673,4092,681,174,2924,170,3548 191,407,253,192,207,425,580,231,197,382,404,472,164,571,500,216 4700,1161,168,5398,5227,5119,252,2552,4887,5060,1152,3297,847,4525,220,262 2417,992,1445,184,554,2940,209,2574,2262,1911,2923,204,2273,2760,506,157 644,155,638,78,385,408,152,360,588,618,313,126,172,220,217,161 227,1047,117,500,1445,222,29,913,190,791,230,1281,1385,226,856,1380 436,46,141,545,122,86,283,124,249,511,347,502,168,468,117,94 2949,3286,2492,2145,1615,159,663,1158,154,939,166,2867,141,324,2862,641 1394,151,90,548,767,1572,150,913,141,1646,154,1351,1506,1510,707,400 646,178,1228,1229,270,167,161,1134,193,1312,1428,131,1457,719,1288,989 1108,1042,93,140,822,124,1037,1075,125,941,1125,298,136,94,135,711 112,2429,1987,2129,2557,1827,477,100,78,634,352,1637,588,77,1624,2500 514,218,209,185,197,137,393,555,588,569,710,537,48,309,519,138 1567,3246,4194,151,3112,903,1575,134,150,4184,3718,4077,180,4307,4097,1705
advent2 :: IO ()
advent2 = do
    int2d <- fmap (fmap stringToInts) getArgs
    let extractMinMax = foldl minAndMax (10000, 0)
    let pairDiff = \(x,y) -> y - x
    putStrLn $ show $ sum $ fmap (pairDiff . extractMinMax) int2d
    putStrLn $ show $ sum $ fmap (head . evenDivisions) int2d

nextOdd :: Int -> Int
nextOdd n = if n `mod` 2 == 0 then n+1 else n

advent3 :: IO ()
advent3 = do
    n <- fmap (read . head) getArgs
    let side = (nextOdd . ceiling . sqrt . fromIntegral) n
    let dMin = side `div` 2
    let mins = [ side*side - dMin
               , side*side - dMin*3
               , side*side - dMin*5
               , side*side - dMin*7]
    let result = (+) dMin $ foldl min n $ fmap (abs . (n -)) mins
    putStrLn $ show $ result
    --res2: https://oeis.org/A141481 ...

advent4 :: IO ()
advent4 = do
    inputFile <- fmap head getArgs
    inputs <- readFile inputFile
    let passphrases = fmap (splitOn " ") $ splitOn "\n" inputs
    putStrLn $ show $ foldl (\r l -> if (length l) == (Set.size $ Set.fromList l) then r+1 else r) 0 passphrases
    putStrLn $ show $ foldl (\r l -> if (length l) == (Set.size $ Set.fromList l) then r+1 else r) 0 $ fmap (fmap Lst.sort) passphrases

applyN n f = foldr (.) id (replicate n f)

processAndCount :: (Zipper Int -> Zipper Int) -> Zipper Int -> Int
processAndCount rule zipper = processAndCount' 0 zipper
    where
        processAndCount' n (Zip _ []) = n
        processAndCount' n zipper = processAndCount' (n+1) $ rule zipper

rule1 (Zip ls (r : rs)) =
    if r < 0 then
        applyN (-r) Zp.left $ Zip ls (r+1 : rs)
    else
        applyN r Zp.right $ Zip ls (r+1 : rs)

rule2 (Zip ls (r : rs)) =
    if r < 0 then
        applyN (-r) Zp.left $ Zip ls (r+1 : rs)
    else if r < 3 then
        applyN r Zp.right $ Zip ls (r+1 : rs)
    else
        applyN r Zp.right $ Zip ls (r-1 : rs)

advent5 :: IO ()
advent5 = do
    inputFile <- fmap head getArgs
    input <- readFile inputFile
    let tape = Zp.fromList $ fmap read $ splitOn "\n" input
    putStrLn $ show $ processAndCount rule1 tape
    putStrLn $ show $ processAndCount rule2 tape

distributeOnce :: Seq Int -> Seq Int
distributeOnce seq = distribute max (maxId + 1) $ Seq.update maxId 0 seq
    where
        l = Seq.length seq
        (max, maxId) = Seq.foldlWithIndex (\curMax i cur -> if cur > fst curMax then (cur, i) else curMax) (Seq.index seq 0, 0) seq
        distribute 0 atId seq = seq
        distribute toDistribute atId seq = distribute (toDistribute-1) (atId+1) $ Seq.adjust (+1) (atId `mod` l) seq

distributeAndLoop :: [Int] -> ([Int], Int)
distributeAndLoop input = distributeAndLoop' 0 Set.empty $ Seq.fromList input
    where
        distributeAndLoop' n past present =
            if Set.member present past then
                (toList present, n)
            else
                distributeAndLoop' (n+1) (Set.insert present past) $ distributeOnce present

advent6 :: IO ()
advent6 = do
    let input = [10, 3, 15, 10, 5, 15, 5, 15, 9, 2, 5, 8, 5, 2, 3, 6] -- [0,2,7,0]
    let (loopStart, cycleCount) = distributeAndLoop input
    putStrLn $ show cycleCount
    putStrLn $ show $ snd $ distributeAndLoop loopStart

treeName = fst . Tree.rootLabel

hasKid :: String -> Tree (String, Int) -> Bool
hasKid kidName (Node _ kids) = Lst.elem kidName $ fmap treeName kids

nameWeightRegex = mkRegex "^([a-z]+) \\(([0-9]+)\\)$"

weightedName :: String -> Tree (String, Int)
weightedName str = Tree.Node (name, weight) []
    where
        matches = Mbe.fromJust $ matchRegex nameWeightRegex str
        name = matches !! 0
        weight = read $ matches !! 1

nameWeightKidsRegex = mkRegex "^([a-z]+) \\(([0-9]+)\\) -> (.*)$"

weightedNameAndKids :: String -> Tree (String, Int)
weightedNameAndKids str = Tree.Node (name, weight) $ fmap treeStub kids
    where
        matches = Mbe.fromJust $ matchRegex nameWeightKidsRegex str
        name = matches !! 0
        weight = read $ matches !! 1
        kids = splitOn ", " $ matches !! 2

treeFromLine :: String -> Tree (String, Int)
treeFromLine line =
    if (Chr.isAlpha $ last line) then
        weightedNameAndKids line
    else
        weightedName line

treeStub :: String -> Tree (String, Int)
treeStub name = Tree.Node (name, 0) []

pairWith :: (a -> b) -> a -> (b, a)
pairWith f x = (f x, x)

uglyDuckling :: Eq a => [a] -> [b] -> Maybe (b, a, a)
uglyDuckling [] _ = Nothing
uglyDuckling [_] _ = Nothing
uglyDuckling [_, _] _ = Nothing
uglyDuckling (x : y : z : r) (bx : by : bz : br) =
    if x == y && y == z then
        uglyDuckling (y : z : r) (by : bz : br)
    else if x == y then
        Just (bz, z, x)
    else if y == z then
        Just (bx, x, y)
    else
        Just (by, y, x)

badWeight :: Map String (Tree (String, Int)) -> Tree (String, Int) -> Maybe (Int, Int, Int)
badWeight index (Node (_, w) []) = Nothing
badWeight index t@(Node (_, w) kids) =
    if Mbe.isJust mBadKid then
        mBadKid
    else
        mBadWeight
    where
        mBadWeight = uglyDuckling kidsWeights $ fmap (snd . Tree.rootLabel) kidsWithKids
        kidsTotalWeight = sum kidsWeights
        kidsWeights = fmap (fullWeight index) kidsWithKids
        mBadKid = (Mbe.listToMaybe . Mbe.catMaybes) $ fmap (badWeight index) kidsWithKids
        kidsWithKids = fmap ((index Map.!) . treeName) kids

fullWeight :: Map String (Tree (String, Int)) -> Tree (String, Int) -> Int
fullWeight index (Node (_, w) []) = w
fullWeight index (Node (_, w) kids) = sum $ w : (fmap (fullWeight index) kidsWithKids)
    where
        kidsWithKids = fmap ((index Map.!) . treeName) kids

advent7 :: IO ()
advent7 = do
    inputs <- fmap head getArgs >>= readFile
    let lines = splitOn "\n" inputs
    let root = "hmvwl" -- from ctrl-f'ing in input file
    putStrLn root
    let index = Map.fromList $ fmap ((pairWith treeName) . treeFromLine) lines
    let mBadWeight = badWeight index $ index Map.! root
    let mCorrectWeight = fmap (\(selfWeight, fullWeight, correctFullWeight) -> selfWeight + correctFullWeight - fullWeight) mBadWeight
    putStrLn $ show mBadWeight
    putStrLn $ show mCorrectWeight

data Op = Inc | Dec
applyOp Inc = (+)
applyOp Dec = (-)
opFromString "inc" = Inc
opFromString "dec" = Dec
testFromString "!=" = (/=)
testFromString "==" = (==)
testFromString "<=" = (<=)
testFromString ">=" = (>=)
testFromString "<" = (<)
testFromString ">" = (>)

parseInstruction :: String -> (String, Op, Int, (Map String Int -> Bool))
parseInstruction line = (register, op, val, condition)
    where
        matches = Mbe.fromJust $ matchRegex instructionRegex line
        register = matches !! 0
        op = opFromString $ matches !! 1
        val = read $ matches !! 2
        conditionRegister = matches !! 3
        conditionOp = testFromString $ matches !! 4
        conditionVal = read $ matches !! 5
        condition = \registers -> conditionOp (Mbe.fromMaybe 0 $ Map.lookup conditionRegister registers) conditionVal

instructionRegex = mkRegex "^([a-z]+) (inc|dec) (-?[0-9]+) if ([a-z]+) (!=|==|<|>|<=|>=) (-?[0-9]+)$"

readInstruction :: (Map String Int, Int) -> String -> (Map String Int, Int)
readInstruction (registers, currentMax) line = readInstruction' $ parseInstruction line
    where
        readInstruction' (register, op, val, condition) =
            let newVal = applyOp op 0 val in
            let newRegisters = Map.insertWith (+) register newVal registers in
            if condition registers then
                (newRegisters, max currentMax $ newRegisters Map.! register)
            else
                (registers, currentMax)

advent8 :: IO ()
advent8 = do
    inputs <- fmap head getArgs >>= readFile
    let lines = splitOn "\n" inputs
    let (registers, grandMax) = foldl readInstruction (Map.empty, 0) lines
    putStrLn $ show $ maximum $ Map.elems registers
    putStrLn $ show grandMax

main :: IO ()
main = advent8