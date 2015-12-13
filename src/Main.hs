module Main where

import System.Environment
import Network.HTTP
import Network.TCP
import Network.URI

import Parser

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
  rqHeaders = [Header HdrContentType "application/json+list", Header HdrContentLength (show (length gameState) :: String)] :: [Header],
  rqBody = gameState
}

executeNetworkingWithId :: String -> IO ()
executeNetworkingWithId gameId = do
  putStrLn ("Zaidimo id: " ++ gameId)
  connection <- openStream "tictactoe.homedir.eu" 80
  let serial = serializeBoard $ concat $ randomMove emptyBoard X 0 :: String
  putStrLn ("Serialized: " ++ serial)
  rawResponse <- sendHTTP connection (postRequestWithIdAndState gameId serial)
  body <- getResponseBody rawResponse
  print body
  printFinishMessage True

main :: IO ()
main = do
    printStartMessage
    startNetworkingWithStartParams