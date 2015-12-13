import System.Environment

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

gameId :: String
gameId = "test1"

startNetworkingWithStartParams :: IO ()
startNetworkingWithStartParams = do
  params <- getArgs
  case params of
    [aString] -> do
      putStrLn ("Zaidimo id: " ++ aString)
      printFinishMessage True
    _ -> do
      name <- getProgName
      putStrLn ("Usage: " ++ name ++ " <string>")
      printFinishMessage False

main :: IO ()
main = do
    printStartMessage
    startNetworkingWithStartParams
