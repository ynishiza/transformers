{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Arbitrary
  ( Bot (..),
    BaseMonad,
    isStrictType,
    isStrictMonad,
    F1 (..),
    F1Bot (..),
    withBaseMonad,
    isBottomError,
  )
where

import           Control.Exception (ErrorCall, Exception (displayException),
                                    evaluate, try)

import           Data.Data
import           Data.Functor.Identity (Identity (..))
import           Data.List (isInfixOf)
import           Data.Tuple.Solo

import           GHC.IO (unsafePerformIO)

import           Test.ChasingBottoms.IsBottom (isBottom)
import           Test.QuickCheck

bottom :: forall a. a
bottom = error "<bottom>"

-- Check error message only i.e. the prefix, ignoring the stacktrace.
isBottomError :: ErrorCall -> Bool
isBottomError e = "<bottom>" `isInfixOf` displayException e

-- | Arbitrary (Bot a) values may be bottom.
--
-- Borrowed from container tests: https://github.com/haskell/containers/
newtype Bot a
  = Bot { unBot :: a }

-- Strictness as a data type
-- e.g.
--
--    newtype A = ...    strict
--    data A a = A a     lazy
--    data A a = A !a    strict
--
data TypeStrictness = StrictType | LazyType
  deriving (Eq, Show)

--  Strictness in monadic sequencing.
--  e.g.
--
--     Identity    m        >>= k  = k (runIdentity m)   lazy due to runIdentity
--
--     Solo        MkSolo x >>= k  = k x                 strict due to MkSolo 
--
data MonadStrictness = StrictMonad | LazyMonad
  deriving (Eq, Show)

data BaseMonad
  = forall m. (Typeable m, Monad m) => BaseMonad TypeStrictness MonadStrictness (forall a. m a -> IO a)

-- Use the underlying Monad of a BaseMonad.
withBaseMonad :: BaseMonad -> (forall m. (Monad m) => m a) -> IO a
withBaseMonad (BaseMonad _ _ v)   = v

isStrictType :: BaseMonad -> Bool
isStrictType (BaseMonad t _ _)  = t == StrictType

isStrictMonad :: BaseMonad -> Bool
isStrictMonad (BaseMonad _ t _)  = t == StrictMonad

instance Arbitrary BaseMonad where
  arbitrary =
    elements
      [ BaseMonad LazyType StrictMonad id,                         -- IO
        BaseMonad StrictType LazyMonad (evaluate . runIdentity),   -- Identity
        BaseMonad LazyType StrictMonad (evaluate . getSolo),       -- Solo
        BaseMonad LazyType LazyMonad (\f -> evaluate $ f ())       -- constant function () -> a
      ]

instance Show BaseMonad where
  show v@(BaseMonad _ _ m) = getName m <> " [" <> typeLabel <> " " <> monadLabel <> "]"
    where
      typeLabel = "type:" <> (if isStrictType v then "strict" else "lazy")
      monadLabel = "monad:" <> (if isStrictMonad v then "strict" else "lazy")
      getName :: forall m. (Typeable m) => (forall a. m a -> IO a) -> String
      getName _ = show $ typeRep (Proxy @m)

instance Show a => Show (Bot a) where
  show (Bot x) = if isBottom x then "<bottom>" else show x

instance Arbitrary a => Arbitrary (Bot a) where
  arbitrary =
    frequency
      [ (1, pure bottom),
        (4, Bot <$> arbitrary)
      ]

-- | Arbitrary function of one argument.
newtype F1 a b
  = F1 { unF1 :: a -> b }
  deriving newtype (Arbitrary)

instance (Typeable a, Typeable b) => Show (F1 a b) where
  show :: F1 a b -> String
  show _ = a <> " -> " <> b
    where
      a = show $ typeRep (Proxy @a)
      b = show $ typeRep (Proxy @b)

-- | Arbitrary function that may bottom in the output.
--
-- To be precise, the function is either
-- a) a valid arbitrary function i.e. never bottoms, or
-- b) a constant function which always bottoms.
--
-- In particular, the QuickCheck built-in Func cannot be used for this, since
-- Func a (Bot b) generates a function which may or may not bottom depending on the input.
newtype F1Bot a b
  = F1Bot { unF1Bot :: a -> b }

instance (CoArbitrary a, Arbitrary b) => Arbitrary (F1Bot a b) where
  arbitrary = do
    useBottomFunc <- arbitrary :: Gen Bool
    F1Bot <$>
      if useBottomFunc
        then return (const bottom)
        else (arbitrary :: Gen (a -> b))

instance (Typeable a, Typeable b) => Show (F1Bot a b) where
  show :: F1Bot a b -> String
  show (F1Bot f) = if unsafePerformIO isBottomFunc
                      then a <> " -> bottom"
                      else a <> " -> " <> b
    where
      isBottomFunc = do
          result <- try (evaluate (f undefined))
          case result of
            Left (e :: ErrorCall) -> return $ isBottomError e
            _                     -> return False
      a = show $ typeRep (Proxy @a)
      b = show $ typeRep (Proxy @b)
