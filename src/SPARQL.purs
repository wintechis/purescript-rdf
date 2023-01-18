module RDF.SPARQL where

import Prelude

import Data.Array (catMaybes, foldl, fromFoldable)
import Data.Foldable (and)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Effect (Effect)
import Effect.Console (logShow)
import RDF (Graph, Quad, Term, blankNode, defaultGraph, literalType, namedNode, namedNode', object, predicate, quad, subject, termType, variable)
import RDF.Prefixes (xsd)

data TP
    = TP Term Term Term

data Pattern
    = BGP (Array TP)
    | Join Pattern Pattern
    | LeftJoin Pattern Pattern Expression
    | Filter Expression Pattern
    | Union Pattern Pattern
    | Graph Term Pattern

type SolutionMapping = Map Term Term

eval :: Graph -> Pattern -> Array SolutionMapping
eval _ (BGP []) = [ Map.empty ]
eval g (BGP tps) = map filterMappingsForVars $ foldl mergeSMArrays [ Map.empty ] $ map mappingsForTP tps
    where
        mappingsForTP :: TP -> Array SolutionMapping
        mappingsForTP tp = catMaybes $ fromFoldable $ Set.map (\q -> getSolutionMapping tp q) g
eval _ _ = []

filterMappingsForVars :: SolutionMapping -> SolutionMapping
filterMappingsForVars sm = Map.filterKeys (\key -> termType key == "Variable") sm

mergeSMArrays :: Array SolutionMapping -> Array SolutionMapping -> Array SolutionMapping
mergeSMArrays sms1 sms2 = catMaybes do
  sm1 <- sms1
  sm2 <- sms2
  pure $ mergeIfCompatible sm1 sm2

mergeIfCompatible :: SolutionMapping -> SolutionMapping -> Maybe SolutionMapping
mergeIfCompatible sm1 sm2 = if compatible then Just $ Map.union sm1 sm2 else Nothing
    where
        compatible :: Boolean
        compatible = and $ Set.map (\key -> maybeTermCompatible (Map.lookup key sm1) (Map.lookup key sm2)) keySet
            where
                maybeTermCompatible :: Maybe Term -> Maybe Term -> Boolean
                maybeTermCompatible (Just t1) (Just t2) = t1 == t2
                maybeTermCompatible _ _ = true
        keySet :: Set Term
        keySet = Map.keys sm1 <> Map.keys sm2

getSolutionMapping :: TP -> Quad -> Maybe SolutionMapping
getSolutionMapping (TP s p o) q = tripleTermCheck (checkTerm s $ subject q) (checkTerm p $ predicate q) (checkTerm o $ object q)

data TermCheck = Match | NoMatch | Binds SolutionMapping

tripleTermCheck :: TermCheck -> TermCheck -> TermCheck -> Maybe SolutionMapping
tripleTermCheck Match Match Match = Just Map.empty
tripleTermCheck (Binds sm1) Match Match = Just sm1
tripleTermCheck Match (Binds sm1) Match = Just sm1
tripleTermCheck Match Match (Binds sm1) = Just sm1
tripleTermCheck (Binds sm1) (Binds sm2) Match = mergeIfCompatible sm1 sm2
tripleTermCheck (Binds sm1) Match (Binds sm2) = mergeIfCompatible sm1 sm2
tripleTermCheck Match (Binds sm1) (Binds sm2) = mergeIfCompatible sm1 sm2
tripleTermCheck (Binds sm1) (Binds sm2) (Binds sm3) = do
    sm <- mergeIfCompatible sm1 sm2
    mergeIfCompatible sm sm3
tripleTermCheck _ _ _  = Nothing

checkTerm :: Term -> Term -> TermCheck
checkTerm var quadTerm | termType var == "Variable" = Binds $ Map.singleton var quadTerm
checkTerm bn quadTerm | termType bn == "BlankNode" && (termType quadTerm == "NamedNode" || termType quadTerm == "BlankNode") = Binds $ Map.singleton bn quadTerm
checkTerm patternTerm quadTerm = if patternTerm == quadTerm then Match else NoMatch

data Expression
    = Not Expression
    | Or Expression Expression
    | And Expression Expression
    | NumEqual Number Number
    | StringEqual String String
    | BooleanEqual Boolean Boolean
    | NumUnequal Number Number
    | StringUnequal String String
    | BooleanUnequal Boolean Boolean
    | TermEqual Term Term
    | TermUnequal Term Term

evalExpression :: Expression -> Boolean
evalExpression (Not expr) = not $ evalExpression expr
evalExpression (Or expr1 expr2) = evalExpression expr1 || evalExpression expr2
evalExpression (And expr1 expr2) = evalExpression expr1 && evalExpression expr2
evalExpression (NumEqual num1 num2) = num1 == num2
evalExpression (StringEqual string1 string2) = string1 == string2
evalExpression (BooleanEqual bool1 bool2) = bool1 == bool2
evalExpression (NumUnequal num1 num2) = num1 /= num2
evalExpression (StringUnequal string1 string2) = string1 /= string2
evalExpression (BooleanUnequal bool1 bool2) = bool1 /= bool2
evalExpression (TermEqual term1 term2) = term1 == term2
evalExpression (TermUnequal term1 term2) = term1 /= term2

testGraph :: Graph
testGraph = Set.fromFoldable [
    quad (namedNode "https://ex.org/a") (namedNode "https://ex.org/p") (literalType "3" (namedNode' xsd "integer")) defaultGraph,
    quad (namedNode "https://ex.org/a") (namedNode "https://ex.org/q") (literalType "4" (namedNode' xsd "integer")) defaultGraph
]

testBGP :: Pattern
testBGP = BGP [
    TP (blankNode "x") (namedNode "https://ex.org/p") (variable "y"),
    TP (blankNode "x") (namedNode "https://ex.org/q") (variable "z")
]

main :: Effect Unit
main = do
  logShow $ eval testGraph testBGP