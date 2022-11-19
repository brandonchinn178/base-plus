module Example where

import Control.Exception (Exception, SomeException)
import System.Environment (lookupEnv)

foo :: IO ()
foo = withCheckE $ do
  checkE $ putStrLn "hello world!"
  user <- checkE $ lookupEnv "USER"
  putStrLn $
    case user of
      Nothing -> "$USER is not set"
      Just user' -> "$USER is set to: " ++ user'

data DecodeError = DecodeError String
  deriving (Show)

instance Exception DecodeError

decodeInt :: String -> IOE DecodeError Int
decodeInt s =
  case read s of
    Just x -> pureIO x
    Nothing -> badIO $ DecodeError $ "Not an int: " ++ s
