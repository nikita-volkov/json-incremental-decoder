module JSONIncrementalDecoder.Prelude
( 
  module Exports,
  concatMany,
  eventually,
  eventuallyLifting,
  scanl,
  manyWithIndex,
  shouldFail,
  skipSepBy,
  skipSepBy1,
  skipMany,
  skipMany1,
)
where


-- base-prelude
-------------------------
import BasePrelude as Exports hiding (Alt, scanl)

-- transformers
-------------------------
import Control.Monad.IO.Class as Exports
import Control.Monad.Trans.Class as Exports
import Control.Monad.Trans.Except as Exports hiding (liftCallCC, liftCatch, liftListen, liftPass)
import Control.Monad.Trans.Reader as Exports hiding (liftCallCC, liftCatch, liftListen, liftPass)
import Control.Monad.Trans.State.Strict as Exports hiding (liftCallCC, liftCatch, liftListen, liftPass)
import Control.Monad.Trans.Writer.Strict as Exports

-- supplemented
-------------------------
import Supplemented as Exports

-- unsequential
-------------------------
import Unsequential as Exports

-- interspersed
-------------------------
import Interspersed as Exports

-- matcher
-------------------------
import Matcher as Exports hiding (run)

-- success
-------------------------
import Success.Pure as Exports (Success)

-- hashable
-------------------------
import Data.Hashable as Exports (Hashable)

-- unordered-containers
-------------------------
import Data.HashMap.Strict as Exports (HashMap)

-- bytestring
-------------------------
import Data.ByteString as Exports (ByteString)

-- text
-------------------------
import Data.Text as Exports (Text)

-- scientific
-------------------------
import Data.Scientific as Exports (Scientific)

-- Utils
-------------------------

concatMany :: (Alternative m, Monoid a) => m a -> m a
concatMany consume =
  step <|> end
  where
    step =
      mappend <$> consume <*> concatMany consume
    end =
      pure mempty

{-# INLINABLE eventually #-}
eventually :: Alternative f => f () -> f a -> f a
eventually skip consume =
  fix $ \loop -> consume <|> (skip *> loop)

-- |
-- Given an immediate consumption function,
-- which executes a deeper effect,
-- and such an effect,
-- produces an outer effect,
-- which traverses the input until the deeper effect succeeds.
-- If the deeper effect never succeeds, the outer one fails as well.
-- 
-- This combinator is useful for matching any of the remaining object rows or
-- array elements.
-- I.e., one of the remaining.
-- E.g.,
-- 
-- @
-- eventuallyLifting 'row' :: 'Row' a -> 'Rows' a
-- @
-- 
-- where
-- 
-- @
-- row :: Row a -> Rows a
-- @
-- 
{-# INLINE eventuallyLifting #-}
eventuallyLifting :: (Alternative g, Applicative f) => (forall b. f b -> g b) -> f a -> g a
eventuallyLifting lift fx =
  eventually (lift (pure ())) (lift fx)

{-# INLINE scanl #-}
scanl :: MonadPlus m => (a -> b -> a) -> a -> m b -> m a
scanl snoc init parser =
  loop init
  where
    loop acc =
      mplus (parser >>= loop . snoc acc) (return acc)

{-# INLINE manyWithIndex #-}
manyWithIndex :: Alternative f => (Int -> f ()) -> f ()
manyWithIndex indexHandler =
  loop 0
  where
    loop index =
      indexHandler index *> loop (succ index) <|> pure ()

-- |
-- Note: this parser does not consume any input.
{-# INLINE shouldFail #-}
shouldFail :: MonadPlus m => m a -> m ()
shouldFail p =
  join (mplus (p >> return mzero) (return (return ())))

{-# INLINE skipSepBy #-}
skipSepBy :: Alternative m => m () -> m () -> m ()
skipSepBy one sep =
  skipSepBy1 one sep <|> pure ()

{-# INLINABLE skipSepBy1 #-}
skipSepBy1 :: Alternative m => m () -> m () -> m ()
skipSepBy1 one sep =
  one *> remainders
  where
    remainders =
      (sep *> one *> remainders) <|> pure ()

{-# INLINABLE skipMany #-}
skipMany :: Alternative f => f a -> f ()
skipMany fx =
  loop
  where
    loop =
      (fx *> loop) <|> pure ()

{-# INLINE skipMany1 #-}
skipMany1 :: Alternative f => f a -> f ()
skipMany1 fx =
  fx *> skipMany fx
