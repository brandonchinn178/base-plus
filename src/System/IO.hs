{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module System.IO (
  IOE (UnsafeIOE),
  IO,
  mapError,
  liftError,
  throw,
  catch,
  try,

  -- * Interop with unchecked IO
  convertIO,
  unsafeConvertIOWith,

  -- * Main functions
  -- $mainFunctions
  Main,
  runMain,
  runMainWith,
  MainOptions (..),
  defaultMainOptions,
) where

import "base" Control.Exception (
  Exception,
  SomeAsyncException (..),
  SomeException,
  fromException,
  toException,
  tryJust,
 )
import "base" Data.Bifunctor (first)
import qualified "base" GHC.IO as X
import "base" GHC.IO.Encoding (TextEncoding, setLocaleEncoding)
import qualified "base" GHC.IO.Encoding as Encoding
import "base" GHC.Stack (HasCallStack, withFrozenCallStack)
import "base" Prelude (
  Applicative (..),
  Bool (..),
  Either (..),
  Functor (..),
  Maybe (..),
  Monad (..),
  either,
  error,
  errorWithoutStackTrace,
  maybe,
  otherwise,
  show,
  ($),
  (++),
  (.),
  (<$>),
 )

-- | A checked IO action that can throw exceptions of the given type.
newtype IOE e a = UnsafeIOE {unIOE :: X.IO (Either e a)}

instance Functor (IOE e) where
  fmap f = UnsafeIOE . (fmap . fmap) f . unIOE
instance Applicative (IOE e) where
  pure = UnsafeIOE . pure . Right
  UnsafeIOE mf <*> UnsafeIOE ma =
    UnsafeIOE $
      mf >>= \case
        Left e -> pure (Left e)
        Right f -> fmap f <$> ma
instance Monad (IOE e) where
  UnsafeIOE ma >>= k =
    UnsafeIOE $
      ma >>= \case
        Left e -> pure (Left e)
        Right a -> unIOE (k a)

-- | A checked IO action that returns any possible exception.
type IO = IOE SomeException

{----- IOE operations -----}

mapError :: (e1 -> e2) -> IOE e1 a -> IOE e2 a
mapError f = UnsafeIOE . fmap (first f) . unIOE

liftError :: Exception e => IOE e a -> IO a
liftError = mapError toException

throw :: e -> IOE e a
throw = UnsafeIOE . pure . Left

catch :: IOE e a -> (e -> IOE e' a) -> IOE e' a
catch (UnsafeIOE m) f = UnsafeIOE $ m >>= either (unIOE . f) (pure . Right)

try :: IOE e a -> IOE e' (Either e a)
try = UnsafeIOE . fmap Right . unIOE

{----- Interop with unchecked IO -----}

-- | Convert an unchecked IO action into a checked IO action.
convertIO :: X.IO a -> IO a
convertIO = UnsafeIOE . tryJust isSyncException
  where
    isSyncException e
      -- not async exceptions
      | isAsyncException e = Nothing
      -- otherwise, it's sync
      | otherwise = Just e

-- | Convert an unchecked IO action into a checked IO action that can return an
-- exception specified by the given function.
--
-- If the IO action threw a synchronous exception (precise or imprecise) that is
-- not handled by the given function, this function will error.
unsafeConvertIOWith :: HasCallStack => (SomeException -> Maybe e) -> X.IO a -> IOE e a
unsafeConvertIOWith f = UnsafeIOE . tryJust isWantedException
  where
    isWantedException e =
      case f e of
        _ | isAsyncException e -> Nothing
        Just e' -> Just e'
        Nothing ->
          withFrozenCallStack . error $
            "unsafeConvertIOWith called on action that threw an unexpected error: "
              ++ show e

isAsyncException :: SomeException -> Bool
isAsyncException e =
  case fromException e of
    Just SomeAsyncException{} -> True
    Nothing -> False

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
runMainWith MainOptions{..} (UnsafeIOE m) =
  setEncoding
    >> m
    >>= \case
      Left e -> errorWithoutStackTrace (show e)
      Right () -> pure ()
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
