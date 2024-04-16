main :: IO ()
main =
  putStrLn "This program will tell you a secret" *>
    whenIO confirm (putStrLn "IO is actually pretty awesome") *>
      putStrLn "Bye"

whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action =
  cond >>= \result ->
    if result
      then action
      else pure ()

confirm :: IO Bool
confirm =
  putStrLn "Are you sure? (y/n)" *>
    getLine >>= \answer ->
      case answer of
        "y" -> pure True
        "n" -> pure False
        _ ->
          putStrLn "Invalid response. use y or n" *>
            confirm

