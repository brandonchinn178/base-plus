module Prelude (
  IOE,
  IO,
  mapError,
  liftError,
  throw,
  catch,
  try,
  convertIO,
  unsafeConvertIOWith,
  Main,
  runMain,
  runMainWith,
  MainOptions (..),
  defaultMainOptions,
  putStrLn,
  read,
  getLine,
  Bool (..),
  Char,
  Either (..),
  FilePath,
  Int,
  Integer,
  Maybe (..),
  Show,
  String,
  error,
  otherwise,
  pure,
  return,
  show,
  ($),
  (++),
  (.),
  (==),
  (>>=),
) where

import "base" Text.Read (Read, readMaybe)
import "base" Prelude as X (
  Bool (..),
  Char,
  Either (..),
  FilePath,
  Int,
  Integer,
  Maybe (..),
  Show,
  String,
  error,
  otherwise,
  pure,
  return,
  show,
  ($),
  (++),
  (.),
  (==),
  (>>=),
 )
import qualified "base" Prelude as X

import "this" System.IO

{-
TODO:
  * No MonadFail for IO
  * Add ImpureException type with String message + Dynamic metadata
  * Support async exceptions (bracket + mask)
  * Implement ReaderT with MonadThrowable
-}

putStrLn :: String -> IO ()
putStrLn = convertIO . X.putStrLn

read :: Read a => String -> Maybe a
read = readMaybe

getLine :: IO String
getLine = convertIO X.getLine

{-
head :: Foldable f => f a -> Maybe a
head = foldr go Nothing
  where
    go a m = m <|> Just a
-}
