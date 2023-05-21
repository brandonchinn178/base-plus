{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- TODO: rewrite with brandonchinn178/checked-io
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
import "base" Data.Foldable (asum)
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

-- | A checked IO action that can not throw any exceptions.
newtype UIO a = UnsafeUIO {unUIO :: X.IO a}
  deriving (Functor, Applicative, Monad)

class MonadUIO m where
  liftUIO :: UIO a -> m a

-- | A checked IO action that can only throw synchronous exceptions
-- of the given type. But it can throw any imprecise exceptions (like
-- `error`, just like any pure function) or asynchronous exceptions
-- (which you should generally not worry about)
--
-- Morally equivalent to `UIO (Either e a)`, but implemented without
-- the `Either` for performance.
newtype IOE e a = UnsafeIOE {unIOE :: X.IO a}
  deriving (Functor, Applicative, Monad)

type IO = IOE X.IOException

class MonadIOE e m where
  liftIOE :: IOE e a -> m a

liftIO :: MonadIOE X.IOException m => IO a -> m a
liftIO = liftIOE

{----- IOE operations -----}

mapError :: (Exception e1, Exception e2) => (e1 -> e2) -> IOE e1 a -> IOE e2 a
mapError f = UnsafeIOE . X.handle (X.throwIO . f) . unIOE

convertE :: FromError e1 e2 => IOE e1 a -> IOE e2 a
convertE = mapError fromError

throw :: Exception e => e -> IOE e a
throw = UnsafeIOE . X.throwIO

-- | If your handler does not throw an error, consider using 'catchUIO'.
catch :: IOE e1 a -> (e1 -> IOE e2 a) -> IOE e2 a
catch (UnsafeIOE m) f = UnsafeIOE $ m `X.catch` (unIOE . f)

-- | Same as 'toUIO', except returns as 'IOE'.
try :: Exception e => IOE e a -> IOE e' (Either e a)
try = toIOE . toUIO

toIOE :: UIO a -> IOE e a
toIOE = UnsafeIOE . unUIO

toUIO :: IOE e a -> UIO (Either e a)
toUIO (UnsafeIOE m) f = UnsafeUIO $ m `X.catch` f

catchUIO :: IOE e a -> (e -> UIO a) -> UIO a
catchUIO (UnsafeIOE m) f = UnsafeUIO $ m `X.catch` f

{----- Interop with unchecked IO -----}

-- | Convert an unchecked IO action into a checked IO action.
convertIO :: X.IO a -> IOE SomeSyncException a
convertIO = UnsafeIOE . tryJust isSyncException
  where
    isSyncException e =
      case toAnyException e of
        SyncException e -> Just (SomeSyncException e)
        _ -> Nothing

-- | Convert an unchecked IO action into a checked IO action that can return an
-- exception specified by the given function.
--
-- If the IO action threw a synchronous exception that is not handled by the given
-- function, this function will error.
unsafeConvertIOWith :: HasCallStack => (SomeException -> Maybe e) -> X.IO a -> IOE e a
unsafeConvertIOWith f = UnsafeIOE . tryJust isWantedException
  where
    isWantedException e =
      case toAnyException e of
        SyncException e ->
          case f e of
            Just e' -> Just e'
            Nothing ->
              withFrozenCallStack . error $
                "unsafeConvertIOWith called on action that threw an unexpected error: "
                  ++ show e
        _ -> Nothing

isAsyncException :: SomeException -> Bool
isAsyncException e =
  case fromException e of
    Just SomeAsyncException{} -> True
    Nothing -> False

data SomeSyncException = forall e. Exception e => SomeSyncException e
  deriving (Show)

instance Exception SomeSyncException

class (Exception e1, Exception e2) => FromError e1 e2 where
  fromError :: e1 -> e2
instance {-# OVERLAPPABLE #-} Exception e => FromError e e where
  fromError = id

-- TODO: should ExitCode be handled specially?
data AnyException
  = forall e. Exception e => SyncException e
  | forall e. Exception e => ImpreciseException e
  | forall e. Exception e => AsyncException e
  deriving (Show)

toAnyException :: SomeException -> AnyException
toAnyException e@(X.SomeException e') =
  fromMaybe (SyncException e) . asum $
    [ ImpreciseException <$> fromE @X.ErrorCall
    , ImpreciseException <$> fromE @X.TypeError
    , ImpreciseException <$> fromE @X.ArithException
    , ImpreciseException <$> fromE @X.ArrayException
    , ImpreciseException <$> fromE @X.AssertionFailed
    , ImpreciseException <$> fromE @X.NestedAtomically
    , ImpreciseException <$> fromE @X.NoMethodError
    , ImpreciseException <$> fromE @X.PatternMatchFail
    , ImpreciseException <$> fromE @X.RecConError
    , ImpreciseException <$> fromE @X.RecSelError
    , ImpreciseException <$> fromE @X.RecUpdError
    , AsyncException <$> fromE @X.AsyncException
    , AsyncException <$> fromE @X.CompactionFailed
    , AsyncException <$> fromE @X.FixIOException
    , AsyncException <$> fromE @X.AsyncException
    , AsyncException <$> fromE @X.BlockedIndefinitelyOnSTM
    , AsyncException <$> fromE @X.BlockedIndefinitelyOnMVar
    , AsyncException <$> fromE @X.Deadlock
    , AsyncException <$> fromE @X.NonTermination
    , (\(X.SomeAsyncException e) -> AsyncException e) <$> fromE
    ]
  where
    fromE :: forall e. Exception e => Maybe e
    fromE = fromException e

instance Exception where
  fromException = Just . toAnyException

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
