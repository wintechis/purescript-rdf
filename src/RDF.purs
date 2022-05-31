module RDF (
  Term,
  Quad,
  Graph,
  dateTimeToTerm,
  namedNode,
  namedNode',
  blankNode,
  literalType,
  literalLang,
  variable,
  defaultGraph,
  quad,
  triple,
  termType,
  value,
  language,
  datatype,
  subject,
  predicate,
  object,
  graph,
  serialize,
  termToBoolean,
  termToInt
) where

import Prelude hiding (map)

import Data.DateTime (DateTime)
import Data.Foldable (foldl)
import Data.Formatter.DateTime (Formatter, FormatterCommand(..), format)
import Data.Int (fromString)
import Data.List (fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Set (Set, map)
import RDF.Prefixes (Prefix(..), xsd)

data Term = NamedNode String | BlankNode String | LiteralLang String String | LiteralType String Term | Variable String | DefaultGraph
derive instance eqTerm :: Eq Term
derive instance ordTerm :: Ord Term
instance showTerm :: Show Term where
  show (NamedNode s) = "<" <> s <> ">"
  show (BlankNode s) = "_:" <> s
  show (LiteralLang s l) = "\"" <> s <> "\"@" <> l
  show (LiteralType s t) = "\"" <> s <> "\"^^" <> show t
  show (Variable s) = "?" <> s
  show DefaultGraph = ""

data Quad = Quad Term Term Term Term
derive instance eqQuad :: Eq Quad
derive instance ordQuad :: Ord Quad
instance showQuad :: Show Quad where
  show (Quad s p o DefaultGraph) = show s <> " " <> show p <> " " <> show o <> " ."
  show (Quad s p o g) = show s <> " " <> show p <> " " <> show o <> " " <> show g <> " ."

type Graph = Set Quad

namedNode :: String -> Term
namedNode s = NamedNode s

namedNode' :: Prefix -> String -> Term
namedNode' (Prefix prefix) s = NamedNode $ prefix <> s

blankNode :: String -> Term
blankNode s = BlankNode s

literalType :: String -> Term -> Term
literalType s t = LiteralType s t

literalLang :: String -> String -> Term
literalLang s l = LiteralLang s l

variable :: String -> Term
variable s = Variable s

defaultGraph :: Term
defaultGraph = DefaultGraph

quad :: Term -> Term -> Term -> Term -> Quad
quad s p o g = Quad s p o g

triple :: Term -> Term -> Term -> Quad
triple s p o = Quad s p o defaultGraph

termType :: Term -> String
termType (NamedNode _) = "NamedNode"
termType (BlankNode _) = "BlankNode"
termType (LiteralLang _ _) = "Literal"
termType (LiteralType _ _) = "Literal"
termType (Variable _) = "Variable"
termType DefaultGraph = "DefaultGraph"

value :: Term -> String
value (NamedNode v) = v
value (BlankNode v) = v
value (LiteralLang v _) = v
value (LiteralType v _) = v
value (Variable v) = v
value DefaultGraph = ""

language :: Term -> Maybe String
language (LiteralLang _ l) = Just l
language _ = Nothing

datatype :: Term -> Maybe Term
datatype (LiteralType _ t) = Just t
datatype _ = Nothing

subject :: Quad -> Term
subject (Quad s _ _ _) = s

predicate :: Quad -> Term
predicate (Quad _ p _ _) = p

object :: Quad -> Term
object (Quad _ _ o _) = o

graph :: Quad -> Term
graph (Quad _ _ _ g) = g

serialize :: Graph -> String
serialize g = foldl (\q1 q2 -> q1 <> "\n" <> q2) "" $ map show g

termToBoolean :: Term -> Maybe Boolean
termToBoolean (LiteralType v t) = if t == namedNode' xsd "boolean" then
  case v of 
    "true" -> Just true
    "false" -> Just false
    _ -> Nothing
  else Nothing
termToBoolean _ = Nothing

termToInt :: Term -> Maybe Int
termToInt (LiteralType v t) = if t == namedNode' xsd "integer" then fromString v else Nothing
termToInt _ = Nothing

iso8601Format :: Formatter
iso8601Format = fromFoldable
  [ YearFull
  , Placeholder "-"
  , MonthTwoDigits
  , Placeholder "-"
  , DayOfMonthTwoDigits
  , Placeholder "T"
  , Hours24
  , Placeholder ":"
  , MinutesTwoDigits
  , Placeholder ":"
  , SecondsTwoDigits
  , Placeholder "."
  , Milliseconds
  , Placeholder "Z"
  ]

dateTimeToTerm :: DateTime -> Term
dateTimeToTerm dt = literalType (format iso8601Format dt) (namedNode' xsd "dateTimeStamp")