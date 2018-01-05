module Unit where

import System.FilePath.Glob
import System.Directory
import System.FilePath.Posix
import Test.HUnit
import Data.Text hiding (map)
import Parser
import HUnitDiff

t s f = unpack $ f $ pack s

getTestFiles :: IO [(String,String)]
getTestFiles = map inAndOut <$> glob "**/*.ehtml"
  where
    inAndOut f = (f, t f (dropEnd 5) ++ "html")
    
mkTestCase (i, o) = TestCase $ assertEqualDiffUnified' "" o (genHtml $ parseEHtml i)

readTestCase (i, o) = do
   ic <- readFile i
   oc <- readFile o
   return $ TestLabel (takeFileName i) $ mkTestCase (ic, oc)

main = do
  fs <- getTestFiles
  tests <- TestList <$> mapM readTestCase fs
  runTestTT tests