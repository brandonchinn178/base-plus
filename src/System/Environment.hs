module System.Environment (
  lookupEnv,
) where

import "base" Control.Exception (Exception, SomeException, fromException)
import "base" GHC.IO.Exception (IOException (..))
import qualified "base" System.Environment as X
import "this" System.IO (IOE, unsafeConvertIOWith)
import "base" Prelude (Maybe (..), Show, String, ($), (.))

-- TODO: move to GHC.IO.Encoding.Failure
data LocaleEncodingError = LocaleEncodingError String
  deriving (Show)

instance Exception LocaleEncodingError

lookupEnv :: String -> IOE LocaleEncodingError (Maybe String)
lookupEnv = unsafeConvertIOWith toLocaleEncodingError . X.lookupEnv

toLocaleEncodingError :: SomeException -> Maybe LocaleEncodingError
toLocaleEncodingError e =
  case fromException e of
    Just (IOError _ _ "recoverDecode" s _ _) -> Just $ LocaleEncodingError s
    Just (IOError _ _ "recoverEncode" s _ _) -> Just $ LocaleEncodingError s
    _ -> Nothing
