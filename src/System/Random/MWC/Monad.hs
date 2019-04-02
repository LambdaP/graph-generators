{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module System.Random.MWC.Monad where

import           Control.Monad.Primitive
import           Control.Monad.Reader
import qualified System.Random.MWC             as MWC

type Mwc m a = ReaderT (MWC.Gen (PrimState m)) m a

runMwc :: (PrimMonad m) => Mwc m a -> MWC.Gen (PrimState m) -> m a
runMwc = runReaderT

class Variate a where
  uniform :: (PrimMonad m) => Mwc m a
  uniformR :: (PrimMonad m) => (a, a) -> Mwc m a

instance (MWC.Variate a) => Variate a where
  uniform = ask >>= MWC.uniform
  uniformR bounds = ask >>= MWC.uniformR bounds
