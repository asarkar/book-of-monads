import Chapter08.Lib

main :: IO ()
main = addName ["Barry", "John"] "David" >>= print