
{-|
Module      : Util
Description : Various utilities used by Glisha framework
License     : MIT
Maintainer  : bananu7@o2.pl
Stability   : stable
Portability : full

This module contains various utilities missing from @Prelude@
or @Control.Monad@.
-}

module Util where

import Control.Monad (unless)

-- |A "functional" if-statement.
bool :: Bool -> a -> a -> a
bool b falseRes trueRes = if b then trueRes else falseRes

-- |Monad "unless" that automatically extracts the value from the action.
unless' :: Monad m => m Bool -> m () -> m ()
unless' action falseAction = do
    b <- action
    unless b falseAction

-- |A bit cleaner "maybe".
maybe' :: Maybe a -> b -> (a -> b) -> b
maybe' m nothingRes f = case m of
    Nothing -> nothingRes
    Just x  -> f x
    
