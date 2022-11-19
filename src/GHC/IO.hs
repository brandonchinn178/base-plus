{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module GHC.IO (
  IOThrowable (..),
  IOTotal,
  IOE,
  IO,
  pureIO,
  badIO,
  mapError,
  liftError,

  -- * Interop with unchecked IO
  convertIO,
  unsafeConvertIOWith,

  -- * Typeclasses
  MonadIO (..),
  LiftException (..),
  MonadThrowable (..),

  -- * Main functions
  -- $mainFunctions
  Main,
  runMain,
  runMainWith,
  MainOptions (..),
  defaultMainOptions,
) where

import "base" Control.Exception (
  ErrorCall (..),
  Exception,
  SomeAsyncException (..),
  SomeException,
  fromException,
  toException,
  tryJust,
 )
import "base" Data.Bifunctor (first)
import "base" Data.Void (Void, absurd)
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
  id,
  maybe,
  otherwise,
  show,
  ($),
  (++),
  (.),
  (<$>),
 )

-- | A checked IO action that can throw exceptions of type @e@.
newtype IOThrowable e a = UnsafeIOThrowable {unsafeIOThrowable :: X.IO (Either e a)}

instance Functor (IOThrowable e) where
  fmap f = UnsafeIOThrowable . (fmap . fmap) f . unsafeIOThrowable
instance Applicative (IOThrowable e) where
  pure = UnsafeIOThrowable . pure . Right
  UnsafeIOThrowable mf <*> UnsafeIOThrowable ma =
    UnsafeIOThrowable $
      mf >>= \case
        Left e -> pure (Left e)
        Right f -> fmap f <$> ma
instance Monad (IOThrowable e) where
  UnsafeIOThrowable ma >>= k =
    UnsafeIOThrowable $
      ma >>= \case
        Left e -> pure (Left e)
        Right a -> unsafeIOThrowable (k a)

-- | A checked IO action that can not throw any exceptions.
type IOTotal = IOThrowable Void

-- | A checked IO action that returns possible exceptions.
type IOE e a = IOTotal (Either e a)

-- | A checked IO action that returns any possible exception.
type IO a = IOE SomeException a

{----- IOThrowable operations -----}

pureIO :: a -> IOE e a
pureIO = pure . Right

badIO :: e -> IOE e a
badIO = pure . Left

mapError :: (e1 -> e2) -> IOE e1 a -> IOE e2 a
mapError f m = first f <$> m

liftError :: Exception e => IOE e a -> IOE SomeException a
liftError = mapError toException

{----- Interop with unchecked IO -----}

-- | Convert an unchecked IO action into a checked IO action.
convertIO :: X.IO a -> IO a
convertIO = UnsafeIOThrowable . fmap Right . tryJust isSyncException
  where
    isSyncException e
      -- not async exceptions
      | isAsyncException e = Nothing
      -- not exceptions from `error` or `undefined`
      -- TODO: or should we capture impure exceptions?
      | Just ErrorCallWithLocation{} <- fromException e = Nothing
      -- otherwise, it's sync
      | otherwise = Just e

-- | Convert an unchecked IO action into a checked IO action that can return an
-- exception specified by the given function.
--
-- If the IO action threw a synchronous exception (precise or imprecise) that is
-- not handled by the given function, this function will error.
unsafeConvertIOWith :: HasCallStack => (SomeException -> Maybe e) -> X.IO a -> IOE e a
unsafeConvertIOWith f = UnsafeIOThrowable . fmap Right . tryJust isWantedException
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

{----- Typeclasses -----}

class MonadIO m where
  liftIO :: IOTotal a -> m a
instance MonadIO (IOThrowable e) where
  liftIO = UnsafeIOThrowable . fmap (first absurd) . unsafeIOThrowable

class LiftException e1 e2 where
  liftE :: e1 -> e2
instance {-# OVERLAPPABLE #-} Exception e => LiftException e SomeException where
  liftE = toException
instance {-# OVERLAPPABLE #-} LiftException e e where
  liftE = id

class MonadThrowable mTotal mThrowable where
  type ThrowableException mThrowable

  checkE ::
    (LiftException e1 e2, ThrowableException mThrowable ~ e2) =>
    mTotal (Either e1 a)
    -> mThrowable a

  withCheckE ::
    (ThrowableException mThrowable ~ e) =>
    mThrowable (Either e a)
    -> mTotal (Either e a)

instance MonadThrowable (IOThrowable Void) (IOThrowable e) where
  type ThrowableException (IOThrowable e) = e
  checkE (UnsafeIOThrowable m) =
    UnsafeIOThrowable $
      m >>= \case
        Left e -> absurd e
        Right (Left e1) -> pure $ Left (liftE e1)
        Right (Right x) -> pure $ Right x
  withCheckE = UnsafeIOThrowable . fmap (Right . either Left id) . unsafeIOThrowable

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
runMainWith MainOptions{..} m =
  setEncoding
    >> unsafeIOThrowable m
    >>= \case
      Left e -> absurd e
      Right (Left e) -> errorWithoutStackTrace (show e)
      Right (Right ()) -> pure ()
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
