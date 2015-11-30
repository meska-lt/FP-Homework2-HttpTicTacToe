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

printFinishMessage :: IO ()
printFinishMessage = putStrLn (programFinishMessage True)

main :: IO ()
main = do
	printStartMessage
	printFinishMessage