{-# LANGUAGE ScopedTypeVariables #-}

module Hmus.Lib.Exception
  ( ErrMsg
  , alreadyExistsError

  , tryIO
  , ioExceptionDefaultWrap
  ) where

import qualified Control.Exception as E
import qualified GHC.IO.Exception  as GE


-- TODO
type ErrMsg = String


tryIO :: forall a. a -> IO a -> IO a
tryIO defaultVal action = either id id <$> E.tryJust pred action
  where
    pred :: E.SomeException -> Maybe a
    pred = const $ Just defaultVal


ioExceptionDefaultWrap :: IO a -> IO (Either String a)
ioExceptionDefaultWrap = E.tryJust pred
  where
    pred :: E.IOException -> Maybe String
    pred = defaultExceptionPredicate


defaultExceptionPredicate :: E.Exception e => e -> Maybe String
defaultExceptionPredicate = Just . show


alreadyExistsError :: String -> IOError
alreadyExistsError str =
  GE.IOError Nothing GE.AlreadyExists "" str Nothing Nothing
