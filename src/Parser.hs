module Parser where

import Data.List.Extra
import System.Random

type InternalMap = [(String, String)]

boardEdgeLength :: Int
boardEdgeLength = 3

data Player = X | O deriving (Show, Read, Eq)
type Position = (Int, Int)
type Board = [[Maybe Player]]

strToPlayer :: String -> Player
strToPlayer "+" = X
strToPlayer "x" = X
strToPlayer "X" = X
strToPlayer "o" = O
strToPlayer "O" = O
strToPlayer "0" = O

playerToStr :: Maybe Player -> String
playerToStr player =
    case player of
        Just X -> "x"
        Just O -> "o"

emptyBoard :: Board
emptyBoard = replicate boardEdgeLength $ replicate boardEdgeLength Nothing

replaceNth :: Int -> Maybe Player -> [Maybe Player] -> [Maybe Player]
replaceNth n newVal (head:tail)
     | n == 0 = newVal:tail
     | otherwise = head:replaceNth (n-1) newVal tail

findParam :: InternalMap -> String -> String -> String
findParam map param errorMsg =
    case lookup param map of
        Just val -> val
        Nothing -> error errorMsg

trimElem :: String -> String -> String -> String -> String
trimElem str elemPrefix elemPostfix errorMsg = 
    case stripPrefix elemPrefix str of
        Just rest -> case stripSuffix elemPostfix rest of
            Just rest -> rest
            Nothing -> error errorMsg
        Nothing -> error errorMsg

getMapContents :: String -> InternalMap -> InternalMap
getMapContents [] acc = acc
getMapContents str acc =
    let 
        item = takeWhile (/= ' ') str
        value = takeWhile (/= ' ') (drop (length item + 1) str)
        rest = drop (length item + length value + 2) str
    in reverse $ getMapContents rest ((item, value) : acc)

getMapElem :: String -> Maybe (String, String)
getMapElem [] = Nothing
getMapElem str =
    let 
        key = takeWhile (/= ')') str ++ ")"
        rest = drop (length key + 2) str
    in Just (key, rest)

parseList :: String -> [String] -> [String]
parseList [] acc = acc
parseList str acc =
    case getMapElem str of
        Just (key, rest) -> parseList rest (key : acc)
        Nothing -> error "Unknown error."

parseMaps :: [String] -> [InternalMap] -> [InternalMap]
parseMaps [] acc = acc
parseMaps (head:tail) acc =
    let
        num = takeWhile (/= '(') head
        denum = drop (length num) head
        striped = filter (/= '"') (trimElem denum "(m " ")" "Not a map.")
        parsed = getMapContents striped []
    in parseMaps tail (parsed : acc)

parseSExpr :: String -> [InternalMap]
parseSExpr str =
    let
        listContent = trimElem str "(m " ")" "Not a list."
        parsedData = parseMaps (parseList listContent []) []
    in parsedData

fillTheGrid :: [InternalMap] -> [Maybe Player] -> Board
fillTheGrid [] grid = chunksOf boardEdgeLength grid
fillTheGrid (x:xs) grid =
    let
        pos1 = read (findParam x "x" "x not defined.") :: Int
        pos2 = read (findParam x "y" "y not defined.") :: Int
        player = findParam x "v" "player not defined."
        index = boardEdgeLength * pos1 + pos2
        newGrid = replaceNth index (Just (strToPlayer player)) grid
    in fillTheGrid xs newGrid

getWinSeqs :: Board -> [[Maybe Player]]
getWinSeqs grid = horizontal ++ vertical ++ [fDiag, bDiag]
  where horizontal = grid
        vertical = transpose grid
        fDiag = zipWith (!!) (reverse grid) [0..]
        bDiag = zipWith (!!) grid [0..]

getWinner :: String -> String
getWinner map
    | winner X  = "Winner: X"
    | winner O  = "Winner: O"
    | otherwise = "Winner: there is none"
    where
        grid = fillTheGrid (parseSExpr map) (concat emptyBoard)
        winner :: Player -> Bool
        winner player = any (all (== Just player)) $ getWinSeqs grid

serializeMapContents :: [Maybe Player] -> Int -> [String] -> [String]
serializeMapContents [] n moves = moves
serializeMapContents (x:xs) n moves =
    if x /= Nothing then let
        (xVal,yVal) = (divMod n 3)
        currentMove = "{\"x\":" ++ (show xVal) ++ ", \"y\":" ++ (show yVal) ++ ", \"v\":\"" ++ (playerToStr x) ++ "\"}"
    in serializeMapContents xs (n+1) (currentMove:moves)
    else serializeMapContents xs (n+1) moves

serializeBoard :: [Maybe Player] -> String
serializeBoard board =
  let listContent = intercalate ", " (reverse $ serializeMapContents board 0 [])
  in "[" ++ listContent ++ "]"

shuffle :: Int -> Int -> [a] -> [a]
shuffle seed 0   _  = []
shuffle seed len xs = 
        let
                n = fst $ randomR (0, len - 1) (mkStdGen seed)
                (y, ys) =  choose n xs
                ys' = shuffle seed (len - 1) ys
        in y:ys'

choose _ [] = error "choose: index out of range"
choose 0 (x:xs) = (x, xs)
choose i (x:xs) = let (y, ys) = choose (i - 1) xs in (y, x:ys)

randomMove :: Board -> Player -> Int -> Board
randomMove board player seed = 
    let
        concatenatedBoard = concat board
        list = findIndices (==Nothing) concatenatedBoard
        len = length list
        idx = seed `mod` len
        shuffledList = shuffle seed len list
        h = list !! (len-idx-1)
    in chunksOf boardEdgeLength (replaceNth h (Just player) concatenatedBoard)