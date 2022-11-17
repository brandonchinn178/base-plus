{-# LANGUAGE LambdaCase #-}

import Example

main :: Main
main = runMain $ do
  putStrLn "Starting program"

  putStrLn "Running foo..."
  foo `unwrapIOE` \case
    Left e -> error (show e)
    Right () -> pure ()

  putStrLn "Type in a number:"
  input <- getLine
  decodeInt input `unwrapIOE` \case
    Left _ -> putStrLn "You did not type in a number!"
    Right x -> putStrLn $ "You typed in: " ++ show x
