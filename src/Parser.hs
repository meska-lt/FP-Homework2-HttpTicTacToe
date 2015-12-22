module Parser where

import Data.List.Extra
import System.Random

type InternalMap = [(String, String)]

boardEdgeLength :: Int
boardEdgeLength = 3

data Player = X | O deriving (Show, Read, Eq)
type Marking = Maybe Player
type Position = (Int, Int)
type Board = [[Maybe Player]]

strToPlayer :: String -> Player
strToPlayer "x" = X
strToPlayer "X" = X
strToPlayer "o" = O
strToPlayer "O" = O

playerToStr :: Maybe Player -> String
playerToStr player =
    case player of
        Just X -> "x"
        Just O -> "o"

emptyBoard :: Board
emptyBoard = replicate boardEdgeLength $ replicate boardEdgeLength Nothing

replaceNth :: Int -> Marking -> [Marking] -> [Marking]
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
        rest = drop (length key + 1) str
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
        listContent = trimElem str "(l " ")" "Not a list."
        parsedData = parseMaps (parseList listContent []) []
    in parsedData

fillTheGrid :: [InternalMap] -> [Marking] -> Board
fillTheGrid [] grid = chunksOf boardEdgeLength grid
fillTheGrid (x:xs) grid =
    let
        pos1 = read (findParam x "x" "x not defined.") :: Int
        pos2 = read (findParam x "y" "y not defined.") :: Int
        player = findParam x "v" "player not defined."
        index = boardEdgeLength * pos1 + pos2
        newGrid = replaceNth index (Just (strToPlayer player)) grid
    in fillTheGrid xs newGrid

getWinSeqs :: Board -> [[Marking]]
getWinSeqs grid = horizontal ++ vertical ++ [fDiag, bDiag]
  where horizontal = grid
        vertical = transpose grid
        fDiag = zipWith (!!) (reverse grid) [0..]
        bDiag = zipWith (!!) grid [0..]

getWinner :: String -> Maybe Char
getWinner map
    | winner X  = Just 'x'
    | winner O  = Just 'o'
    | otherwise = Nothing
    where
        grid = fillTheGrid (parseSExpr map) (concat emptyBoard)
        winner :: Player -> Bool
        winner player = any (all (== Just player)) $ getWinSeqs grid

serializeMapContents :: [Maybe Player] -> Int -> Int -> [String] -> [String]
serializeMapContents [] moveId moveNumber serializedPart = serializedPart
serializeMapContents (head:tail)  moveId moveNumber serializedPart =
    if head /= Nothing then let
        yVal = moveNumber `mod` boardEdgeLength
        xVal = (moveNumber - yVal) `div` boardEdgeLength
        str = "(m \"x\" " ++ (show xVal) ++ " \"y\" " ++ (show yVal) ++ " \"v\" \"" ++ (playerToStr head) ++ "\")"
    in serializeMapContents tail (moveId + 1) (moveNumber + 1) (str:serializedPart)
    else serializeMapContents tail (moveId) (moveNumber + 1) serializedPart

serializeBoard :: [Maybe Player] -> String
serializeBoard board =
  let listContent = intercalate " " (reverse $ serializeMapContents board 0 0 [])
  in "(l " ++ listContent ++ ")"

predefinedMove :: Board -> Int -> Board
predefinedMove board moveNumber =
    case moveNumber of
        0 -> chunksOf boardEdgeLength (replaceNth (boardEdgeLength * 1 + 2) (Just X) (concat board))
        1 -> chunksOf boardEdgeLength (replaceNth (boardEdgeLength * 1 + 1) (Just O) (concat board))
        2 -> chunksOf boardEdgeLength (replaceNth (boardEdgeLength * 2 + 2) (Just X) (concat board))
        3 -> chunksOf boardEdgeLength (replaceNth (boardEdgeLength * 1 + 0) (Just O) (concat board))
        4 -> chunksOf boardEdgeLength (replaceNth (boardEdgeLength * 0 + 1) (Just X) (concat board))
        5 -> chunksOf boardEdgeLength (replaceNth (boardEdgeLength * 0 + 2) (Just O) (concat board))
        6 -> chunksOf boardEdgeLength (replaceNth (boardEdgeLength * 2 + 1) (Just X) (concat board))
        7 -> chunksOf boardEdgeLength (replaceNth (boardEdgeLength * 0 + 0) (Just O) (concat board))
        _ -> chunksOf boardEdgeLength (replaceNth (boardEdgeLength * 2 + 0) (Just X) (concat board))