module Util.Monad.Primitive.Unsafe where

import Prelude (seq)
import Control.Applicative
import Control.Category (Category (..))
import Control.Monad (Monad (..))
import Control.Monad.Primitive
import Data.Maybe (Maybe (..))

unsafeInterleaveWhileJust :: PrimBase m => m (Maybe a) -> (a -> m ()) -> m [a]
unsafeInterleaveWhileJust mma f = go
  where
    go = mma >>= unsafeInterleave . \ case
        Nothing -> pure []
        Just a -> (a :) <$> unsafeInterleave (a `seq` f a *> go)
