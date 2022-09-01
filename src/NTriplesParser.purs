module RDFPS.NTriplesParser (parse) where

import Prelude

import Data.Array (fromFoldable) as A
import Data.Char (fromCharCode)
import Data.Either (Either)
import Data.Int (fromStringAs, hexadecimal)
import Data.List (List(..), fold, many, (:))
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (fromFoldable) as S
import Data.String (codePointFromChar)
import Data.String.CodeUnits (fromCharArray)
import Parsing (ParseError, Parser, fail, runParser)
import Parsing.Combinators (lookAhead, many1, manyTill, optionMaybe, try, (<|>))
import Parsing.String (anyChar, char, string)
import Parsing.String.Basic (alphaNum, digit, hexDigit, letter, noneOf, oneOf, skipSpaces)
import RDF (Quad, Term, Graph, blankNode, defaultGraph, literalLang, literalType, namedNode, quad)

parse :: String -> Either ParseError Graph
parse s = runParser s ntriplesDoc

ntriplesDoc :: Parser String Graph
ntriplesDoc = do
  _ <- skipSpaces
  firstTriple :: Maybe Quad <- optionMaybe triple
  _ <- skipSpaces
  triples :: List Quad <- many (try $ eol *> triple)
  _ <- optionMaybe eol
  pure (quads firstTriple triples)
    where
      quads ft ts = case ft of 
                    Just t -> S.fromFoldable (t : ts)
                    Nothing -> S.fromFoldable ts

triple :: Parser String Quad
triple = do
  _ <- skipSpaces
  s <- subject
  _ <- skipSpaces
  p <- predicate
  _ <- skipSpaces
  o <- object
  _ <- skipSpaces
  _ <- char '.'
  pure (quad s p o defaultGraph)

subject :: Parser String Term
subject = do
  s <- iriRef <|> blankNodeLabel
  pure s

predicate :: Parser String Term
predicate = do
  p <- iriRef
  pure p

object :: Parser String Term
object = do
  o <- iriRef <|> blankNodeLabel <|> literal
  pure o

literal :: Parser String Term
literal = do
  sl <- stringLiteralQuote
  l <- optionMaybe (((string "^^" *> iriRef) >>= \t -> pure (literalType sl t)) <|> (langTag >>= \lang -> pure (literalLang sl lang)))
  pure (l' l sl)
    where
      l' l sl = case l of 
             Nothing -> literalType sl (namedNode "http://www.w3.org/2001/XMLSchema#string")
             Just lit -> lit

langTag :: Parser String String
langTag = do
  _ <- char '@'
  lang1 <- many1 letter
  lang2 <- many langRest
  pure ((fromCharArray $ A.fromFoldable $ lang1) <> (fold lang2))
    where
      langRest :: Parser String String
      langRest = do
        _ <- char '-'
        ls :: NonEmptyList Char <- many1 alphaNum
        pure ("-" <> (fromCharArray $ A.fromFoldable ls))

eol :: Parser String Unit
eol = do
  _ <- many (char '\xD' <|> char '\xA')
  pure unit

iriRef :: Parser String Term
iriRef = do
  _ <- char '<'
  iri <- manyTill ((noneOf [
    '\x00', '\x01', '\x02', '\x03', '\x04', '\x05', '\x06', '\x07', '\x08', '\x09',
    '\x10', '\x11', '\x12', '\x13', '\x14', '\x15', '\x16', '\x17', '\x18', '\x19',
    '\x20', '<', '>', '"', '{' ,'}', '|', '^', '`', '\\']) <|> uChar) (char '>')
  pure (namedNode $ fromCharArray $ A.fromFoldable iri)

stringLiteralQuote :: Parser String String
stringLiteralQuote = do
  _ <- char '"'
  litString <- manyTill ((noneOf []) <|> eChar <|> uChar) (char '"')
  pure (fromCharArray $ A.fromFoldable litString)

blankNodeLabel :: Parser String Term
blankNodeLabel = do
  _ <- string "_:"
  first :: Char <- pnCharsU <|> digit
  end <- optionMaybe $ manyTill (pnChars <|> char '.') (lookAhead $ char ' ') -- TODO last char not .
  pure (blankNode $ fromCharArray (A.fromFoldable (first : (fromMaybe Nil end))))

uChar :: Parser String Char
uChar = do
  u <- uChar4 <|> uChar6
  pure u 
    where
      uChar4 :: Parser String Char
      uChar4 = do
        _ <- string "\\u" <|> string "\\U"
        h1 <- hexDigit
        h2 <- hexDigit
        h3 <- hexDigit
        h4 <- hexDigit
        let ch = do
              codePoint <- getCodePoint h1 h2 h3 h4
              fromCharCode codePoint
        case ch of 
          Nothing -> fail "uChar is not a valid character"
          Just c -> pure c
          where
            getCodePoint h1 h2 h3 h4 = fromStringAs hexadecimal (fromCharArray [ h1, h2, h3, h4])
      uChar6 :: Parser String Char
      uChar6 = do
        _ <- string "\\u" <|> string "\\U"
        h1 <- hexDigit
        h2 <- hexDigit
        h3 <- hexDigit
        h4 <- hexDigit
        h5 <- hexDigit
        h6 <- hexDigit
        let ch = do
              codePoint <- getCodePoint h1 h2 h3 h4 h5 h6
              fromCharCode codePoint
        case ch of 
          Nothing -> fail "uChar is not a valid character"
          Just c -> pure c
          where
            getCodePoint h1 h2 h3 h4 h5 h6 = fromStringAs hexadecimal (fromCharArray [ h1, h2, h3, h4, h5, h6 ])

eChar :: Parser String Char
eChar = do
  _ <- char '\\'
  e <- oneOf [ 't', 'b', 'n', 'r', 'f', '"', ''', '\\' ]
  let c = case e of 
          't' -> '\t'
          'b' -> '\x08'
          'n' -> '\n'
          'r' -> '\r'
          'f' -> '\x0c'
          '"' -> '"'
          ''' -> '''
          '\\' -> '\\'
          _ -> '\x00'
  pure c

pnCharsBase :: Parser String Char
pnCharsBase = do
  c <- anyChar
  let cp = codePointFromChar c
  if codePointValid cp then pure c else fail $ "Character " <> show c <> " not allowed in pnCharsBase"
    where
      codePointValid cp' = cp' >= (codePointFromChar '\x0041') && cp' <= (codePointFromChar '\x005A') ||
                           cp' >= (codePointFromChar '\x0061') && cp' <= (codePointFromChar '\x007A') ||
                           cp' >= (codePointFromChar '\x00C0') && cp' <= (codePointFromChar '\x00D6') ||
                           cp' >= (codePointFromChar '\x00D8') && cp' <= (codePointFromChar '\x00F6') ||
                           cp' >= (codePointFromChar '\x00F8') && cp' <= (codePointFromChar '\x02FF') ||
                           cp' >= (codePointFromChar '\x0370') && cp' <= (codePointFromChar '\x037D') ||
                           cp' >= (codePointFromChar '\x037F') && cp' <= (codePointFromChar '\x1FFF') ||
                           cp' >= (codePointFromChar '\x200C') && cp' <= (codePointFromChar '\x200D') ||
                           cp' >= (codePointFromChar '\x2070') && cp' <= (codePointFromChar '\x218F') ||
                           cp' >= (codePointFromChar '\x2C00') && cp' <= (codePointFromChar '\x2FEF') ||
                           cp' >= (codePointFromChar '\x3001') && cp' <= (codePointFromChar '\xD7FF') ||
                           cp' >= (codePointFromChar '\xF900') && cp' <= (codePointFromChar '\xFDCF') ||
                           cp' >= (codePointFromChar '\xFDF0') && cp' <= (codePointFromChar '\xFFFD')
                           -- TODO cp' >= (codePointFromChar '\x10000') && cp' <= (codePointFromChar '\xEFFFF')

pnCharsU :: Parser String Char
pnCharsU = do
  c <- char '_' <|> char ':' <|> pnCharsBase
  pure c

pnChars :: Parser String Char
pnChars = do
  c <- char '-' <|> digit <|> char '\x00B7' <|> pnCharsU -- \x0300 - \x036F <|> \x203F - \x2040
  pure c