module Convert where
  
import System.Environment
import Data.List
import Data.Char
import qualified Data.Text as Text
import Text.HTML.TagSoup.Tree
import Text.HTML.TagSoup

isSpaceTextLeaf (TagLeaf (TagText s)) = all isSpace s
isSpaceTextLeaf _ = False

compactChildren = filter (not . isSpaceTextLeaf)

mustEscapeText s = '|' `elem` s || case s of
  f:rest -> isLower f
  [] -> False
escapeEHtml s | mustEscapeText s = "| " ++ s
escapeEHtml s = s

strip = Text.unpack . Text.strip . Text.pack

genEHtml (TagBranch tag attrs children)
  = tag ++ attrsEhtml ++ childrenEhtml
  where
    attrsEhtml = if null attrs then "" else " " ++ (intercalate " " $ map (\(k, v) -> k ++ "=\"" ++ escapeHTML v ++ "\"") attrs)
    children' = compactChildren children
    childrenEhtml = if null children' then "" else "\n" ++ (indent $ intercalate "\n" $ map genEHtml children')
    indent = unlines . map ("  " ++) . lines
genEHtml (TagLeaf (TagText s)) = escapeEHtml $ strip s
genEHtml _ = ""

main = do
  f : _ <- getArgs
  c <- readFile f
  --putStrLn $ show $ parseTree c
  putStrLn $ intercalate "\n" $ map genEHtml $ parseTree c