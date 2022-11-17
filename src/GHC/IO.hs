{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module GHC.IO (
  IO,
  unsafeToIO,
  IOE,
  convertIO,
  unsafeConvertIOWith,
  fromIOE,
  toIOE,
  unwrapIOE,
  mapError,
  liftError,

  -- * Type classes
  MonadIO (..),

  -- * Main functions
  -- $mainFunctions
  Main,
  runMain,
  runMainWith,
  MainOptions (..),
  defaultMainOptions,
) where

import "base" Control.Exception (Exception, SomeException, toException, try)
import "base" Data.Bifunctor (first)
import qualified "base" GHC.IO as X
import "base" GHC.IO.Encoding (TextEncoding, setLocaleEncoding)
import qualified "base" GHC.IO.Encoding as Encoding
import "base" GHC.Stack (HasCallStack)
import "base" Prelude (
  Applicative (..),
  Either (..),
  Functor (..),
  Maybe (..),
  Monad (..),
  error,
  id,
  maybe,
  show,
  ($),
  (++),
  (.),
  (<$>),
 )

newtype IO a = IO (X.IO a)
  deriving (Functor, Applicative, Monad)

-- | Convert an action using 'X.IO' from `base` into the new 'IO' type.
--
-- Caller should ensure that the given action cannot throw an error. If
-- it can, use 'convertIO' instead.
unsafeToIO :: X.IO a -> IO a
unsafeToIO = IO

newtype IOE e a = IOE {unIOE :: IO (Either e a)}

instance Functor (IOE e) where
  fmap f = IOE . (fmap . fmap) f . unIOE
instance Applicative (IOE e) where
  pure = IOE . pure . Right
  IOE mf <*> IOE ma =
    IOE $
      mf >>= \case
        Left e -> pure (Left e)
        Right f -> fmap f <$> ma
instance Monad (IOE e) where
  IOE ma >>= k =
    IOE $
      ma >>= \case
        Left e -> pure (Left e)
        Right a -> unIOE (k a)

-- TODO: don't catch async exceptions
convertIO :: X.IO a -> IOE SomeException a
convertIO = IOE . unsafeToIO . try

-- TODO: don't catch async exceptions
unsafeConvertIOWith :: HasCallStack => (SomeException -> Maybe e) -> X.IO a -> IOE e a
unsafeConvertIOWith f = mapError go . convertIO
  where
    go e =
      case f e of
        Just e' -> e'
        Nothing -> error $ "unsafeConvertIOWith called on action that threw an unexpected error: " ++ show e

fromIOE :: IOE e a -> IO (Either e a)
fromIOE = unIOE

toIOE :: IO (Either e a) -> IOE e a
toIOE = IOE

-- | Handle the 'IOE' with the given callback.
--
-- Useful with LambdaCase:
--
-- @
-- main :: Main
-- main = runMain $ do
--   action `unwrapIOE` \case
--     Left e -> putStrLn $ "Got error: " ++ show e
--     Right x -> putStrLn $ "Got success: " ++ show x
-- @
unwrapIOE :: IOE e a -> (Either e a -> IO b) -> IO b
unwrapIOE (IOE m) f = m >>= f

mapError :: (e1 -> e2) -> IOE e1 a -> IOE e2 a
mapError f = IOE . fmap (first f) . unIOE

liftError :: Exception e => IOE e a -> IOE SomeException a
liftError = mapError toException

{----- Typeclasses -----}

class MonadIO m where
  liftIO :: IO a -> m a
instance MonadIO IO where
  liftIO = id
instance MonadIO (IOE e) where
  liftIO = toIOE . fmap Right

{----- Main functions -----}

-- $mainFunctions
-- Because GHC requires the 'main' function to use its own 'X.IO' type,
-- we'll need to do some interop. Entrypoints should look like this:
--
-- @
-- main :: Main
-- main = runMain $ ...
-- @

type Main = X.IO ()

runMain :: IO () -> Main
runMain = runMainWith defaultMainOptions

runMainWith :: MainOptions -> IO () -> Main
runMainWith MainOptions{..} (IO m) =
  setEncoding >> m
  where
    setEncoding = maybe (pure ()) setLocaleEncoding localeEncoding

data MainOptions = MainOptions
  { localeEncoding :: Maybe TextEncoding
  -- ^ The locale to use in your program (default: utf8). Set to
  -- Nothing to use whatever locale is configured on your system,
  -- or explicitly set to some other encoding.
  --
  -- https://serokell.io/blog/haskell-with-utf8
  }

defaultMainOptions :: MainOptions
defaultMainOptions =
  MainOptions
    { localeEncoding = Just Encoding.utf8
    }
