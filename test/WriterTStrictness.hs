{-# LANGUAGE TypeApplications #-}

module WriterTStrictness (test) where

import qualified Control.Monad.Trans.Writer.CPS as CPS
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import Test.ChasingBottoms.IsBottom (isBottom)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Utils (Bot (..))

type Output = [()]

------------------------------------------------------------------------

-- * Test list

test :: IO ()
test = do
  defaultMain $ testGroup "Writer" tests

tests :: [TestTree]
tests =
  [ testFunction "execWriter" (prop_strictExecWriter @() @String) prop_lazyExecWriter prop_cpsExecWriter,
    testFunction "tell" prop_strictTellBottomOutput prop_lazyTellBottomOutput prop_cpsTell,
    testFunction "listen" (prop_strictListen @() @String) prop_lazyListen prop_cpsListen
  ]

testFunction :: (Testable a) => String -> a -> a -> a -> TestTree
testFunction name strict lazy cps =
  testGroup
    name
    [ testProperty "strict" strict,
      testProperty "lazy" lazy,
      testProperty "cps" cps
    ]

prop_lazyListen :: Bot (a, w) -> Property
prop_lazyListen (Bot w) =
  isBottom
    ( Lazy.runWriter $
        Lazy.listen
          (Lazy.writer w)
    )
    === False

prop_strictListen :: Bot (a, w) -> Property
prop_strictListen (Bot i) =
  isBottom
    ( Strict.runWriter $
        Strict.listen
          (Strict.writer i)
    )
    === isBottom i

prop_cpsListen :: (Monoid w) => Bot (a, w) -> Property
prop_cpsListen (Bot i) =
  isBottom
    ( CPS.runWriter $
        CPS.listen
          (CPS.writer i)
    )
    === isBottom i

prop_lazyExecWriter :: Bot (a, w) -> Property
prop_lazyExecWriter (Bot i) =
  isBottom
    ( Lazy.execWriter
        (Lazy.writer i)
    )
    === isBottom i

prop_strictExecWriter :: Bot (a, w) -> Property
prop_strictExecWriter (Bot i) =
  isBottom
    ( Strict.execWriter
        (Strict.writer i)
    )
    === isBottom i

prop_cpsExecWriter :: (Monoid w) => Bot (a,w) -> Property
prop_cpsExecWriter (Bot w) =
  isBottom
    ( CPS.execWriter
        (CPS.writer w)
    )
    === isBottom w

prop_lazyTellBottomOutput :: Bot a -> Property
prop_lazyTellBottomOutput (Bot w) =
  isBottom
    ( Lazy.runWriter
        (Lazy.tell w)
    )
    === False

prop_strictTellBottomOutput :: Bot a -> Property
prop_strictTellBottomOutput (Bot w) =
  isBottom
    ( Strict.runWriter
        (Strict.tell w)
    )
    -- NOTE: tell involves no binding so result is the same as lazy.
    === False

prop_cpsTell :: Bot Output -> Property
prop_cpsTell (Bot w) =
  isBottom
    ( CPS.runWriter
        (CPS.tell w)
    )
    === isBottom w
