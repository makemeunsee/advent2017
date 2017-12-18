module Main where

import Lib
import Control.Applicative
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
import Data.List.Split (splitOn, chunksOf)
import System.Environment (getArgs)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Data.Foldable (toList)
import qualified Data.List.Zipper as Zp
import Data.List.Zipper (Zipper(Zip))
import Data.Char as Chr
import Data.Bits (xor, popCount, (.&.))
import Text.Printf (printf)
import Numeric (readHex)

import Debug.Trace (traceShowId, traceShow)

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

data StreamState = Block | Garbage

processStream :: String -> (Int, Int)
processStream = processStream' 0 0 0 False Block
    where
        processStream' accA accB depth ignoreNext streamState [] = (accA, accB)
        processStream' accA accB depth True streamState (c : cs) = processStream' accA accB depth False streamState cs
        processStream' accA accB depth False Garbage ('!' : cs) = processStream' accA accB depth True Garbage cs
        processStream' accA accB depth False Garbage ('>' : cs) = processStream' accA accB depth False Block cs
        processStream' accA accB depth False Garbage (_ : cs) = processStream' accA (accB+1) depth False Garbage cs
        processStream' accA accB depth False Block ('{' : cs) = processStream' accA accB (depth+1) False Block cs
        processStream' accA accB depth False Block ('}' : cs) = processStream' (accA+depth) accB (depth-1) False Block cs
        processStream' accA accB depth False Block ('<' : cs) = processStream' accA accB depth False Garbage cs
        processStream' accA accB depth False Block (_ : cs) = processStream' accA accB depth False Block cs

advent9 :: IO ()
advent9 = do
    stream <- fmap head getArgs >>= readFile
    putStrLn $ show $ processStream stream

morphism :: Int -> (Int, Int) -> (Int -> Int)
morphism _ (0, _) = id
morphism fullL (l, st) = \i ->
    if (i >= actualSt) `binOp` (i < actualEnd) then
        (2 * actualSt + l - 1 - i) `mod` fullL
    else
        i
    where
        actualSt = st `mod` fullL
        actualEnd = (st + l) `mod` fullL
        rev = actualEnd <= actualSt
        binOp = if rev then (||) else (&&)

prepareHashArgs :: [Int] -> [(Int, Int)]
prepareHashArgs lengths = r
    where
        (r, _, _) = foldl (\(rs, start, skip) l -> ((l, start) : rs, start+skip+l, skip+1)) ([], 0, 0) lengths

densifyHash :: [Int] -> [Int]
densifyHash sparseHash = fmap (foldr xor 0) $ chunksOf 16 sparseHash

stringifyHash :: [Int] -> String
stringifyHash = concat . fmap (printf "%02x")

fullHash :: Int -> String -> [Int]
fullHash size input =
    let suffix = [17, 31, 73, 47, 23] in
    let lengths = (fmap Chr.ord input) ++ suffix in
    let lengthsx64 = concat $ replicate 64 lengths in
    let args = prepareHashArgs lengthsx64 in
    let morphisms = fmap (morphism size) args in
    let sparseHash = foldl (flip fmap) [0..size-1] morphisms in
    densifyHash sparseHash

advent10 :: IO ()
advent10 = do
    size <- fmap (read . head . tail) getArgs
    input <- fmap head getArgs >>= readFile
    let lengths = fmap read $ splitOn "," input
    let args = prepareHashArgs lengths
    let morphisms = fmap (morphism size) args
    -- mapM_ print $ fmap (\m -> fmap m [0..size-1]) $ reverse morphisms
    let (r0, r1) = foldl (\(i0, i1) m -> (m i0, m i1)) (0, 1) morphisms
    print $ r0 * r1
    putStrLn $ stringifyHash $ fullHash size input

tokenToCoords :: String -> (Int, Int, Int)
tokenToCoords "se" = (1,-1,0)
tokenToCoords "n" = (0,1,-1)
tokenToCoords "sw" = (-1,0,1)
tokenToCoords "nw" = (-1,1,0)
tokenToCoords "s" = (0,-1,1)
tokenToCoords "ne" = (1,0,-1)

dist :: (Int, Int, Int) -> Int
dist (x, y, z) = (abs x + abs y + abs z) `div` 2

foldFct :: ((Int, Int, Int), Int) -> (Int, Int, Int) -> ((Int, Int, Int), Int)
foldFct ((x0, y0, z0), max0) (x, y, z) =
    let r = (x+x0, y+y0, z+z0) in
    (r, max (dist r) max0)

advent11 :: IO ()
advent11 = do
    input <- fmap head getArgs >>= readFile
    let coords = fmap tokenToCoords $ splitOn "," input
    let (pos, maxDist) = foldl foldFct ((0,0,0), 0) coords
    print $ dist pos
    print maxDist

lineToRelations :: String -> (String, Set String)
lineToRelations line = (refPid, Set.fromList $ refPid : pids)
    where
        [refPid, rawPids] = splitOn " <-> " line
        pids = splitOn ", " rawPids

group' :: Map String (Set String) -> [String] -> Set String -> Set String -> Set String
group' _ [] _ acc = acc
group' relations (s : ss) checked acc
    | Set.member s checked = group' relations ss checked acc
    | otherwise            = group' relations (ss ++ (Set.toList newToCheck)) (Set.insert s checked) (Set.union acc $ newRelations)
    where
        newRelations = relations Map.! s
        newToCheck = newRelations Set.\\ checked

group :: Map String (Set String) -> String -> Set String
group relations element = group' relations [element] Set.empty Set.empty

relationGroups :: Map String (Set String) -> Set String -> Set (Set String)
relationGroups relations keysToCheck = groups (group relations) keysToCheck Set.empty

groups :: Ord a => (a -> Set a) -> Set a -> Set (Set a) -> Set (Set a)
groups groupFct candidates acc
    | Set.null candidates = acc
    | otherwise           = groups groupFct (candidates Set.\\ newGroup Set.\\ Set.singleton element) (Set.insert newGroup acc)
    where
        newGroup = groupFct element
        element = Set.elemAt 0 candidates

advent12 :: IO ()
advent12 = do
    input <- fmap head getArgs >>= readFile
    let relations = Map.fromList $ fmap lineToRelations $ lines input
    print $ Set.size $ group relations "0"
    print $ Set.size $ relationGroups relations $ Map.keysSet relations

severity :: (Int, Int) -> Int
severity (depth, range) = if depth `mod` period == 0 then depth * range else 0
    where period = (range - 1) * 2

noCatch :: [(Int, Int)] -> Int -> Bool
noCatch layers delay = all (\(layer, range) -> (layer + delay) `mod` ((range - 1) * 2) /= 0) layers

advent13 :: IO ()
advent13 = do
    input <- fmap head getArgs >>= readFile
    let lineToLayer = (\[d, r] -> (read d, read r)) . (splitOn ": ")
    let layers = fmap lineToLayer $ lines input
    print $ sum $ fmap severity layers
    print $ head $ filter (noCatch layers) [0..]

hashToBinMap :: [Int] -> String
hashToBinMap = concat . fmap (printf "%08b")

fragGroup :: Map (Int, Int) Char -> Set (Int, Int) -> (Int, Int) -> (Set (Int, Int), Set (Int, Int))
fragGroup = fragGroup' Set.empty
    where
        fragGroup' acc binMap checked p@(x, y)
            | Map.lookup p binMap == Nothing  = (acc, Set.insert p checked)
            | Map.lookup p binMap == Just '0' = (acc, Set.insert p checked)
            | Map.lookup p binMap == Just '1' =
                let neighbours = [(x-1,y), (x+1,y), (x,y-1), (x,y+1)] in
                let filteredNeighbours = filter (\n -> Set.notMember n checked) neighbours in
                foldr (\p' (acc', checked') -> fragGroup' acc' binMap checked' p') (Set.insert p acc, Set.insert p checked) filteredNeighbours

betterGroups :: Ord a => (Set a -> a -> (Set a, Set a)) -> Set a -> Set (Set a)
betterGroups = betterGroups' Set.empty Set.empty
    where
        betterGroups' elementsChecked acc groupFct elementsToCheck
            | Set.null elementsToCheck = acc
            | otherwise                = betterGroups' checked' acc' groupFct toCheck'
            where
                element = Set.elemAt 0 elementsToCheck
                (newGroup, checked') = groupFct elementsChecked element
                toCheck' = elementsToCheck Set.\\ checked'
                acc' = Set.insert newGroup acc

advent14 :: IO ()
advent14 = do
    input <- fmap ((++ "-") . head) getArgs
    let inputs = take 128 $ fmap ((++) input . show) [0..]
    let hashes = fmap (fullHash 256) inputs
    let r1 = sum $ fmap (sum . (fmap popCount)) hashes
    print r1
    let binMap = Map.fromList $ do
            (col, x) <- zip (fmap hashToBinMap hashes) [0..]
            (bit, y) <- zip col [0..]
            return ((x, y), bit)
    let sizePlusOne = Set.size $ betterGroups (fragGroup binMap) (Map.keysSet binMap)
    -- minus one as we have the empty set in there
    print $ sizePlusOne - 1

gen :: Int -> Int -> Int
gen factor prev = (prev * factor) `mod` 2147483647

sameLowest16Bits :: Int -> Int -> Bool
sameLowest16Bits m n = (m .&. 0xffff) == (n .&. 0xffff)

advent15 :: IO ()
advent15 = do
    seedA <- fmap (read . head) getArgs
    let genA = iterate (gen 16807) seedA
    seedB <- fmap (read . head . tail) getArgs
    let genB = iterate (gen 48271) seedB
    print $ length $ filter (uncurry sameLowest16Bits) $ take 40000000 $ drop 1 $ zip genA genB
    let genA' = filter (\n -> n `mod` 4 == 0) genA
    let genB' = filter (\n -> n `mod` 8 == 0) genB
    print $ length $ filter (uncurry sameLowest16Bits) $ take 5000000 $ drop 1 $ zip genA' genB'

applyMove :: Seq Char -> String -> Seq Char
applyMove programs ['p', p1, '/', p2] = Seq.update p1Id p2 $ Seq.update p2Id p1 programs
    where
        p1Id = Mbe.fromJust $ Seq.elemIndexL p1 programs
        p2Id = Mbe.fromJust $ Seq.elemIndexL p2 programs
applyMove programs ('s' : r) = (Seq.drop l programs) Seq.>< (Seq.take l programs)
    where
        l = 16 - read r
applyMove programs ('x' : r) = Seq.update id1 p2 $ Seq.update id2 p1 programs
    where
        p1 = Seq.index programs id1
        p2 = Seq.index programs id2
        id1 = read $ head parts
        id2 = read $ head $ tail parts
        parts = splitOn "/" r

countUntil :: (a -> Bool) -> [a] -> Int
countUntil stopCondition as = length $ takeWhile (not . stopCondition) as

advent16 :: IO ()
advent16 = do
    input <- fmap head getArgs >>= readFile
    let moves = splitOn "," input
    let programs0 = Seq.fromList $ take 16 ['a'..]
    let iterateDance = iterate (\programs -> foldl applyMove programs moves) programs0
    let applyMoves n = head $ drop n iterateDance
    print $ applyMoves 1

    let period = countUntil (== programs0) (tail iterateDance) + 1
    print $ applyMoves $ 1000000000 `mod` period

moveAndInsert :: Int -> Int -> Int -> (Seq Int, Int) -> (Seq Int, Int)
moveAndInsert limit inc val t@(seq, pos)
    | limit == val = t
    | otherwise = moveAndInsert limit inc (val+1) (newSeq, newPos)
    where
        newPos = 1 + (pos + inc) `mod` val
        newSeq = Seq.insertAt newPos val seq

moveAndFakeInsert :: Int -> Int -> Int -> (Int, Int) -> (Int, Int)
moveAndFakeInsert limit inc val t@(x, pos)
    | val > limit = t
    | otherwise = moveAndFakeInsert limit inc (val+1) (newX, newPos)
    where
        newPos = 1 + (pos + inc) `mod` val
        newX = if newPos == 1 then val else x

advent17 :: IO ()
advent17 = do
    input <- fmap (read . head) getArgs
    let (r, i) = moveAndInsert 2017 input 1 (Seq.singleton 0, 0)
    print $ Seq.index r (i+1)
    print $ fst $ moveAndFakeInsert 50000000 input 1 (0, 0)

main :: IO ()
main = advent17