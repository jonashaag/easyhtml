{-# LANGUAGE CPP #-}
{-# LANGUAGE ImplicitParams #-}
{- | Very basic support for diffing with HUnit.

Source: https://github.com/dylex/HUnit-Diff

Supports both line diffs using 'groom' and word diffs.
Unexpected additions are marked with +red, and delections from expected are -green.

Limitations:

  * Prints the whole value, not just the difference with a few lines of
    context.

  * Relies on the similarity of pretty-printed 'show' results which
    sorta-kinda works much of the time but may sometimes highlight
    differences too eagerly.

  * Always colors the differences for ANSI terminals, regardless of output
    target.

Despite these limitations, I find it more useful than HUnit's defaults.

-}
module HUnitDiff ( assertEqualDiffGroomUnified, assertEqualDiffGroom
                 , assertEqualDiffWordUnified,  assertEqualDiffWord
                 , assertEqualDiff, assertEqualDiffUnified
                 , assertEqualDiff', assertEqualDiffUnified'
                 , (@?==), (@==?) ) where

import Control.Monad       ( unless, ap )
import Data.Char           ( isSpace )

import Data.Algorithm.Diff ( getGroupedDiff, Diff(First, Second, Both) )
import System.Console.ANSI ( Color(Green, Red), ColorIntensity(Dull)
                           , ConsoleLayer(Foreground), SGR(SetColor, Reset)
                           , setSGRCode )
import Test.HUnit          ( Assertion, assertFailure )
import Text.Groom          ( groom )

#if MIN_VERSION_base(4,8,1)
import GHC.Stack
#define with_loc (?loc :: CallStack) =>
#else
#define with_loc
#endif

color :: Color -> String -> String
color c s = setSGRCode [SetColor Foreground Dull c]
  ++ s ++ setSGRCode [Reset]

colorDiff :: Diff a -> String -> String
colorDiff (Both _ _) = id
colorDiff (First _)  = color Green
colorDiff (Second _) = color Red

assertEqualWithDiff :: with_loc (Eq a, Show a)
                              => (a -> [String])
                              -> ([Diff [String]] -> String)
                              -> String -> a -> a -> Assertion
assertEqualWithDiff items msgfmt preface expected actual =
  unless (actual == expected) $ assertFailure $
    (if null preface then "" else preface ++ "\n") ++
    (msgfmt $ getGroupedDiff (items expected)
                             (items actual))

assertEqualWithDiffUnified :: with_loc (Eq a, Show a)
                              => (a -> [String])
                              -> (Diff [String] -> String)
                              -> String -> a -> a -> Assertion
assertEqualWithDiffUnified sp jn =
  assertEqualWithDiff sp $ concatMap $ colorDiff `ap` jn

assertEqualWithDiffSplit :: with_loc (Eq a, Show a)
                              => (a -> [String])
                              -> (Diff [String] -> String)
                              -> String -> a -> a -> Assertion
assertEqualWithDiffSplit sp jn =
  assertEqualWithDiff sp $ \diff ->
      "expected: " ++ fmt msk1 diff ++
    "\n but got: " ++ fmt msk2 diff
 where
  fmt msk = concatMap $ msk $ colorDiff `ap` jn
  msk1 _ (Second _) = ""
  msk1 f d          = f d
  msk2 f (Both _ s) = f (Both s s)
  msk2 _ (First  _) = ""
  msk2 f d          = f d

unlines' :: [String] -> String
unlines' [] = ""
unlines' (s:l) = '\n':s ++ unlines' l

formatLine :: Diff [String] -> [String]
formatLine (Both s _) = map (' ' :) s
formatLine (First s)  = map ('-' :) s
formatLine (Second s) = map ('+' :) s

-- |Like 'Text.HUnit.assertEqual' but producing a colored, unified line diff on failure.
assertEqualDiffUnified :: with_loc (Eq a, Show a)
                              => String -- ^ The message prefix
                              -> a      -- ^ The expected value
                              -> a      -- ^ The actual value
                              -> Assertion
assertEqualDiffUnified =
  assertEqualWithDiffUnified (lines . show) (unlines' . formatLine)

-- |Like 'Text.HUnit.assertEqual' but producing colored line diff on failure.
assertEqualDiff :: with_loc (Eq a, Show a)
                              => String -- ^ The message prefix
                              -> a      -- ^ The expected value
                              -> a      -- ^ The actual value
                              -> Assertion
assertEqualDiff =
  assertEqualWithDiffSplit (lines . show) (unlines' . formatLine)
  
-- |Like 'Text.HUnit.assertEqual' but producing a colored, unified line diff on failure.
assertEqualDiffUnified' :: String -- ^ The message prefix
                        -> String -- ^ The expected value
                        -> String -- ^ The actual value
                        -> Assertion
assertEqualDiffUnified' =
  assertEqualWithDiffUnified lines (unlines' . formatLine)

-- |Like 'Text.HUnit.assertEqual' but producing colored line diff on failure.
assertEqualDiff' :: String -- ^ The message prefix
                 -> String -- ^ The expected value
                 -> String -- ^ The actual value
                 -> Assertion
assertEqualDiff' =
  assertEqualWithDiffSplit (lines . show) (unlines' . formatLine)

-- |Like 'Text.HUnit.assertEqual' but producing a colored, unified 'groom'ed line diff on failure.
assertEqualDiffGroomUnified :: with_loc (Eq a, Show a)
                              => String -- ^ The message prefix
                              -> a      -- ^ The expected value
                              -> a      -- ^ The actual value
                              -> Assertion
assertEqualDiffGroomUnified =
  assertEqualWithDiffUnified (lines . groom) (unlines' . formatLine)

-- |Like 'Text.HUnit.assertEqual' but producing colored 'groom'ed line diff on failure.
assertEqualDiffGroom :: with_loc (Eq a, Show a)
                              => String -- ^ The message prefix
                              -> a      -- ^ The expected value
                              -> a      -- ^ The actual value
                              -> Assertion
assertEqualDiffGroom =
  assertEqualWithDiffSplit (lines . groom) (unlines' . formatLine)

splitWords :: String -> [String]
splitWords "" = []
splitWords s = (sw++ss') : splitWords sr where
  (sw,ss) = break isSpace s
  (ss',sr) = span isSpace ss

formatWords :: Diff [String] -> String
formatWords (Both s _) = concat s
formatWords (First s)  = "{-" ++ concat s ++ "-}"
formatWords (Second s) = "{+" ++ concat s ++ "+}"

-- |Like 'Text.HUnit.assertEqual' but producing a colored, unified character diff on failure.
assertEqualDiffWordUnified :: with_loc (Eq a, Show a)
                              => String -- ^ The message prefix
                              -> a      -- ^ The expected value
                              -> a      -- ^ The actual value
                              -> Assertion
assertEqualDiffWordUnified =
  assertEqualWithDiffUnified (splitWords . show) formatWords

-- |Like 'Text.HUnit.assertEqual' but producing a colored character diff on failure.
assertEqualDiffWord :: with_loc (Eq a, Show a)
                              => String -- ^ The message prefix
                              -> a      -- ^ The expected value
                              -> a      -- ^ The actual value
                              -> Assertion
assertEqualDiffWord =
  assertEqualWithDiffSplit (splitWords . show) formatWords

-- | Like 'Test.HUnit.@?=' but producing a colored diff on failure using 'assertEqualDiffGroomUnified'.
(@?==) :: (Eq a, Show a) => a -> a -> Assertion
actual @?== expected = assertEqualDiffGroomUnified "" expected actual

-- | Like 'Test.HUnit.@=?' but producing a colored diff on failure using 'assertEqualDiffGroomUnified'.
(@==?) :: (Eq a, Show a) => a -> a -> Assertion
expected @==? actual = assertEqualDiffGroomUnified "" expected actual