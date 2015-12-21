module Main where

import System.Environment
import Network.HTTP
import Network.TCP
import Network.URI

import Parser

maxRoundAmount :: Int
maxRoundAmount = 10

programStartMessage :: String
programStartMessage = "Programa darbą pradėjo."

programFinishMessage :: Bool -> String
programFinishMessage success =
    if success
    then "Programa darbą baigė sėkmingai."
    else "Programa darbą baigė nesėkmingai."

printStartMessage :: IO ()
printStartMessage = putStrLn programStartMessage

printFinishMessage :: Bool -> IO ()
printFinishMessage success = putStrLn (programFinishMessage success)

startNetworkingWithStartParams :: IO ()
startNetworkingWithStartParams = do
  params <- getArgs
  case params of
    [aString] -> do
      executeNetworkingWithId aString
    _ -> do
      name <- getProgName
      putStrLn ("Usage: " ++ name ++ " <string>")
      printFinishMessage False

updateUriWithGameId :: String -> URI
updateUriWithGameId id = case parseURI ("http://tictactoe.homedir.eu/game/" ++ id ++ "/player/1") of
  Just url -> url

postRequestWithIdAndState :: String -> String -> Request String
postRequestWithIdAndState gameId gameState = Request {
  rqURI = (updateUriWithGameId gameId) :: URI,
  rqMethod = POST :: RequestMethod,
  rqHeaders = [Header HdrContentType "application/s-expr+map", Header HdrContentLength (show (length gameState) :: String)] :: [Header],
  rqBody = gameState
}

getRequestWithIdAndState :: String -> String -> Request String
getRequestWithIdAndState gameId gameState = Request {
  rqURI = (updateUriWithGameId gameId) :: URI,
  rqMethod = GET :: RequestMethod,
  rqHeaders = [Header HdrAccept "application/s-expr+map"] :: [Header],
  rqBody = ""
}

executeNetworkingWithId :: String -> IO ()
executeNetworkingWithId gameId = do
  putStrLn ("Zaidimo id: " ++ gameId)
  connection <- openStream "tictactoe.homedir.eu" 80
  makeRoundViaConnection connection gameId 0

makeMoveViaConnection :: HandleStream String -> String -> String -> Int -> Int -> IO ()
makeMoveViaConnection connection gameId gameState round move =
    if (mod move 2) == 1
        then do
                let serial = ""
                rawResponse <- sendHTTP connection (getRequestWithIdAndState (gameId ++ (show round)) serial)
                body <- getResponseBody rawResponse
                putStrLn ("GET Received: " ++ body)
                if ((move < 9) && (getWinner body == Nothing))
                    then makeMoveViaConnection connection gameId body round (move + 1)
                    else makeRoundViaConnection connection gameId (round + 1)
        else if (mod move 2) == 0
            then do
                    putStrLn ("POST Sent: " ++ gameState)
                    rawResponse <- sendHTTP connection (postRequestWithIdAndState (gameId ++ (show round)) gameState)
                    body <- getResponseBody rawResponse
                    putStrLn ("POST Received: " ++ body)
                    if ((move < 9) && (getWinner gameState == Nothing))
                        then makeMoveViaConnection connection gameId gameState round (move + 1)
                        else makeRoundViaConnection connection gameId (round + 1)
            else return()

makeRoundViaConnection :: HandleStream String -> String  -> Int -> IO ()
makeRoundViaConnection connection gameId round =
    if round < maxRoundAmount
        then do
                let serial = serializeBoard $ concat $ randomMove emptyBoard X 0 :: String
                makeMoveViaConnection connection gameId serial round 0
        else printFinishMessage True

main :: IO ()
main = do
    printStartMessage
    startNetworkingWithStartParams