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
import qualified "base" Prelude as X
import "base" Prelude (
  Applicative (..),
  Bool (..),
  Either (..),
  Functor (..),
  Maybe (..),
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

-- | A checked IO action that can not throw any exceptions.
newtype IOTotal a = UnsafeIOTotal {unsafeIOTotal :: X.IO a}
  deriving (Functor, Applicative, X.Monad)

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
convertIO = UnsafeIOTotal . tryJust isSyncException
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
unsafeConvertIOWith f = UnsafeIOTotal . tryJust isWantedException
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

{----- IOThrowable -----}

-- $iothrowable-usage
-- A common pattern is to allow a function to early-exit if we see any
-- errors. This functionality is enabled by 'checkE' and 'withCheckE':
--
-- @
-- foo :: IO ()
-- foo = withCheckE $ do
--   s <- checkE $ readFile "foo.txt"
--   writeFile "foo_upper.txt" (map toUpper s) >>= \case
--     Left e -> putStrLn $ "Could not write output: " ++ show e
--     Right () -> return ()
-- @
--
-- Internally, this is implemented with 'IOThrowable', but that should be
-- considered an implementation detail.

-- | A checked IO action that can throw exceptions of type @e@.
newtype IOThrowable e a = IOThrowable {unIOThrowable :: IOTotal (Either e a)}

instance Functor (IOThrowable e) where
  fmap f = IOThrowable . (fmap . fmap) f . unIOThrowable
instance Applicative (IOThrowable e) where
  pure = IOThrowable . pure . Right
  IOThrowable mf <*> IOThrowable ma =
    IOThrowable $
      mf >>= \case
        Left e -> pure (Left e)
        Right f -> fmap f <$> ma
instance X.Monad (IOThrowable e) where
  IOThrowable ma >>= k =
    IOThrowable $
      ma >>= \case
        Left e -> pure (Left e)
        Right a -> unIOThrowable (k a)

checkE :: Convertible e1 e2 => IOE e1 a -> IOThrowable e2 a
checkE = IOThrowable . fmap (first convert)

withCheckE :: IOThrowable e (Either e a) -> IOE e a
withCheckE = fmap (either Left id) . unIOThrowable

{----- Typeclasses -----}

class Convertible from to where
  convert :: from -> to
class Convertible1 from to where
  convert1 :: from a -> to a
instance {-# OVERLAPPABLE #-} Exception e => Convertible e SomeException where
  convert = toException
instance {-# OVERLAPPABLE #-} Convertible a a where
  convert = id
instance {-# OVERLAPPABLE #-} Convertible1 f f where
  convert1 = id

instance Convertible1 IOTotal (IOThrowable e) where
  convert1 = IOThrowable . fmap Right

(>>=) :: (Convertible1 n m, X.Monad m) => n a -> (a -> n b) -> m b
m >>= k = convert1 m X.>>= convert1 . k

(>>) :: (Convertible1 n m, X.Monad m) => n a -> n b -> m b
a >> b = convert1 a X.>> convert1 b

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
    >> unsafeIOTotal m
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
