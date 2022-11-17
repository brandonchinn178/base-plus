module Prelude (
  IO,
  IOE,
  Main,
  runMain,
  liftError,
  unwrapIOE,
  liftIO,
  throwIO,
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
  (++),
  (==),
  ($),
  (.),
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
 )
import qualified "base" Prelude as X

import "this" GHC.IO

throwIO :: e -> IOE e a
throwIO e = toIOE $ pure (Left e)

-- TODO: LocaleEncodingError?
putStrLn :: String -> IO ()
putStrLn = unsafeToIO . X.putStrLn

read :: Read a => String -> Maybe a
read = readMaybe

-- TODO: IOE
getLine :: IO String
getLine = unsafeToIO X.getLine

{-
head :: Foldable f => f a -> Maybe a
head = foldr go Nothing
  where
    go a m = m <|> Just a
-}
