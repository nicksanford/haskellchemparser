{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}


module Main where

import Text.Parsec
import Data.Maybe (fromJust)
import Data.Char (digitToInt)
import Control.Monad
import Data.Functor.Identity
import Data.Char (isUpper, isLower)
import Text.Parsec.Number (int)
import Data.Map (fromListWith)

type Element = String
type Count = Integer
data ParseTree = EmptyParseTree | Leaf (Element, Count) | Node Integer [ParseTree] deriving Show
type Parser = Parsec Char () Char

parens :: Stream s m Char => ParsecT s () m a -> ParsecT s () m a
parens = between (char '(') (char ')')

parseUpper :: Stream s m Char => ParsecT s () m Char
parseUpper = satisfy isUpper

parseLower :: Stream s m Char => ParsecT s () m Char
parseLower = satisfy isLower

--instance Stream Char Identity Char
elementParser :: Stream s m Char => ParsecT s () m String
elementParser = do
  u <- parseUpper
  l <- optionMaybe parseLower

  case l of
    Nothing ->
      return [u]
    Just a ->
      return [u, a]

coefParser ::   Stream s m Char => ParsecT s () m Integer
coefParser = int

leafParser :: Stream s m Char => ParsecT s () m ParseTree
leafParser = do
  a <- elementParser
  i <- optionMaybe coefParser
  case i of
    Nothing ->
      return $ Leaf (a, 1)
    Just ii ->
      return $ Leaf (a, ii)

-- f "K4(ON(SO3)2)2" #=> [Token "K4", Token ]
exprParser :: Stream s m Char => ParsecT s () m ParseTree
exprParser = do
  --K4
  leaves <- many1 leafParser
  --(ON(SO3)2)
  maybeEmbeddedSyntaxTree <- optionMaybe $ parens exprParser
  --2
  case maybeEmbeddedSyntaxTree of
    Nothing ->
      return $ Node 1 leaves
    Just (Node int treeList) -> do
      stuffco <- coefParser
--      more <- exprParser stuff
      let tree = Node 1 $ leaves ++ [Node stuffco treeList]
      return $ tree


test = parse exprParser "" "K4(ON(SO3)2)2"

--data ParseTree = EmptyParseTree | Leaf (Element, Count) | Node Integer [ParseTree] deriving Show
walk ::  (Integer, [(String, Integer)]) -> ParseTree -> (Integer, [(String, Integer)])
walk acc@(currentMultiplier, tups) p =
  case p of
    EmptyParseTree ->
      acc
    Leaf (element, count) ->
      (currentMultiplier, (element, count * currentMultiplier):tups)
    Node newMultiplier trees ->
      futureLists
      where futureLists :: (Integer, [(String, Integer)])
            futureLists = ourConcat $ map (walk (newMult, tups)) trees
            newMult = currentMultiplier * newMultiplier
            ourConcat xs = foldr ff (1,[]) xs
            ff (x1, ys1) (x2,ys2) = (x1*x2, ys1++ys2)






main :: IO ()
main = do
  line <- getLine

  case parse exprParser "" line of
      Right a ->
        putStrLn $ show $ fromListWith (+) $ snd $ walk (1, []) a
      _ ->
        putStrLn "ERROR: Invalid chemical expression"

-- main :: IO ()

-- main = do
--   let result <- parse elementParser "K4(ON(SO3)2)2"
--   case result of
--     _           -> putStrLn $ "We got an error :( )"

--test0 = stringLiteral "hello"

-- Input: 
-- formula = "H2O"
-- Output: "H2O"
-- Explanation: 
-- The count of elements are {'H': 2, 'O': 1}.

-- Input: 
-- formula = "Mg(OH)2"
-- Output: "H2MgO2"
-- Explanation: 
-- The count of elements are {'H': 2, 'Mg': 1, 'O': 2}.

-- Input: 
-- formula = "K4(ON(SO3)2)2"
-- Output: "K4N2O14S4"
-- Explanation: 
-- The count of elements are {'K': 4, 'N': 2, 'O': 14, 'S': 4}.
