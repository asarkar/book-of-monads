import Ch08

main :: IO ()
main = addName ["Barry", "John"] "David" >>= print
