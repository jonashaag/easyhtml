{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser (parseEHtml, genHtml, HTML) where
import qualified Data.Text as Text
import Text.Parsec
import Text.Parsec.Indent
import Data.List
import Text.Regex
import Debug.Trace


left (Left x) = x
left (Right x) = error $ "Right: " ++ show x
right (Right x) = x
right (Left x) = error $ "Left: " ++ show x

strip :: String -> String
strip = Text.unpack . Text.strip . Text.pack

data HTML
  = HtmlElement String String [HTML]
  | HtmlVoidElement String String
  | HtmlText String
  | HtmlRaw String
  | WhitespaceOffset Int HTML

lines' x | last x == '\n' = lines x ++ [""]
lines' x | otherwise      = lines x

unlines' = foldl (++) "" . intersperse "\n"

(.>) = flip (.)
($>) = flip ($)
--(<$$>) = flip (<$>)

showIndent :: Int -> String -> String
showIndent n s
  = lines' s
    $> map maybeIndent
    $> unlines'
  where
    maybeIndent "" = ""
    maybeIndent x = replicate n ' ' ++ x
showIndentMany :: Int -> [String] -> String
showIndentMany n s
  = map (showIndent n) s
    $> unlines'
    

conditionalEscapeHtml s
  = subRegex (mkRegex "&([a-zA-Z]+;)") s "__amp__\\1"
  $> Text.pack
  $> Text.replace "<" "&lt;"
  $> Text.replace ">" "&gt;"
  $> Text.replace "&" "&amp;"
  $> Text.replace "__amp__" "&"
  $> Text.unpack
  
debug x = ""
--debug = id


instance Show HTML where
  show (HtmlVoidElement tag "") = "<" ++ tag ++ " />"
  show (HtmlVoidElement tag attrs) = "<" ++ tag ++ " " ++ attrs ++ " />"
  show (HtmlElement tag attrs children) = openingTag ++ childrenHTML ++ closingTag
    where
      openingTag = "<" ++ tag ++ maybeAttrs ++ ">"
      maybeAttrs = if null attrs then "" else " " ++ attrs
      closingTag = "</" ++ tag ++ ">"
      childrenHTML = if null children then "" else "\n" ++ (showIndentMany 2 $ map show children) ++ "\n"
  show (HtmlRaw s) = (debug "RAW: ") ++ s
  show (HtmlText s) = (debug "TEXT: ") ++ (conditionalEscapeHtml s)
  show (WhitespaceOffset n html) = showIndent n $ show html




spaces' = many $ oneOf " \t"
--untilEol = many $ noneOf "\n"
untilEol1 = do
  t <- many1 $ noneOf "\n"
  newlines <- many $ char '\n'
  return $ t ++ safeInit newlines
  where
    safeInit [] = []
    safeInit xs = init xs

a <||> b = try a <|> b

checkArbitrarilyIndented = checkIndent <|> indented
arbitrarilyIndentedBlock p = withPos $ many1 (checkArbitrarilyIndented *> p)

--withIndentedBlock = withBlock
--withIndentedBlock' f a p = withIndentedBlock f (a <* spaces) (p <* spaces)
withSameBlock f a p = withPos $ do
  x <- a
  y <- option [] $ many $ checkIndent *> p
  return $ f x y
withSameBlock' f a p = withSameBlock f (a <* spaces) (p <* spaces)

withArbitrarilyIndentedBlock f a p = withPos $ do
  r1 <- a
  r2 <- option [] (indented *> arbitrarilyIndentedBlock p)
  return (f r1 r2)
withArbitrarilyIndentedBlock' f a p = withArbitrarilyIndentedBlock f (a <* spaces) (p <* spaces)




pAttrs = option "" (try $ spaces' *> untilEol1)

pTagName = many1 lower

pVoidElementClose = try $ char '/'

pVerbatimText = do
  char '\''
  spaces'
  HtmlRaw <$> intercalate " " <$> withSameBlock' (:) untilEol1 untilEol1

pSmartText = HtmlRaw <$> untilEol1

--pVerbatimSmartText = HtmlRaw <$> untilEol <* newline

pBlankLine = HtmlRaw "\n" <$ newline

pTag = withPos $ pBlankLine <||> pHtmlVoidElement <||> pHtmlElement <||> pVerbatimText <||> pSmartText
  where
    pHtmlVoidElement = HtmlVoidElement
      <$> pTagName
      <*  pVoidElementClose
      <*> pAttrs
    pHtmlElement = do
      withArbitrarilyIndentedBlock' ($) (HtmlElement <$> pTagName <*> pAttrs) pChildTag

pChildTag = pTag <||> pSmartText

pEHtmlLine = do
  spaces'
  sourceCol <- sourceColumn <$> getPosition
  line <- pTag <* spaces
  return $ WhitespaceOffset (sourceCol - 1) line
  

parseEHtml :: String -> [HTML]
parseEHtml = right . runIndentParser parser () ""
  where
    parser = (many1 pEHtmlLine <* eof)

genHtml :: [HTML] -> String
genHtml html = intercalate "\n" (map show html) ++ "\n"