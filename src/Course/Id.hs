{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Id where

import Course.Core
import qualified Prelude as P

data Id a = Id a deriving (Eq, Show)

runId :: Id a -> a
runId (Id a) = a

mapId :: (a -> b) -> Id a -> Id b
mapId f (Id a)    = Id (f a)

bindId :: (a -> Id b) -> Id a -> Id b
bindId f (Id a) = f a

instance P.Functor Id where
	fmap f (Id x) = Id (f x)

instance P.Applicative Id where
  pure              = Id
  (Id f) <*> (Id x) = Id (f x )

instance P.Monad Id where
  (>>=) =
    flip bindId
  return =
    Id
