{-# LANGUAGE CPP             #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module PhiToGraph.Interpret where

import Data.Maybe (mapMaybe)
import           PhiToGraph.Syntax.Abs
import qualified PhiToGraph.Syntax.Par    as Phi
import qualified PhiToGraph.Syntax.Print  as Phi

interpretIO :: String -> IO ()
interpretIO input = do
  case interpret input of
    Left err -> do
      putStrLn "ERROR: Syntax error:"
      putStrLn err
    Right outputs -> mapM_ putStrLn outputs

interpretTermIO :: Term -> IO ()
interpretTermIO = mapM_ putStrLn . interpretTerm

interpret :: String -> Either String [String]
interpret input = do
  let tokens = Phi.myLexer input
  interpretTerm <$> Phi.pTerm tokens

interpretTerm :: Term -> [String]
interpretTerm term = [Phi.printTree (termToRules term)]

termToCommands :: Term -> [Command]
termToCommands (TermAttr attr) = [ CommandBind 1 2 attr ]
termToCommands (TermObj mappings) = concat
  [ [CommandAdd 3] | TermMap x t <- mappings ]

termToRules :: Term -> Rules
termToRules term =
  case applyRules rules [term] of
    Nothing -> error "Cannot apply rules!"
    Just rules' -> Rules rules'

applyRules :: [RuleHandler] -> [Term] -> Maybe [Rule]
applyRules rules [] = Just []
applyRules rules (term:terms) =
  case mapMaybe (\rule -> rule term) rules of
    [] -> Nothing
    [(rule, subterms)] ->
      case applyRules rules (subterms ++ terms) of
        Nothing        -> Nothing
        Just rules' -> Just (rules' ++ [rule])
    _ -> Nothing

rules :: [RuleHandler]
rules = [rule1, rule2, rule3]

type RuleHandler = Term -> Maybe (Rule, [Term])

rule1 :: RuleHandler
rule1 term = case term of
  TermAttr attr -> Just (Rule term (Ident "R1")
    (Commands [ CommandBind 1 2 attr ]), [])
  _ -> Nothing

rule2 :: RuleHandler
rule2 term = case term of
  TermObj mappings -> Just (Rule term (Ident "R2") (Commands []), mappings)
  _ -> Nothing

rule3 :: RuleHandler
rule3 term = case term of
  TermMap attr t -> Just (Rule term (Ident "R3") (Commands [CommandAdd 3]), [])
  _ -> Nothing

rules123 :: RuleHandler
rules123 term = case term of
  TermAttr attr -> Just (Rule term (Ident "R1")
    (Commands [ CommandBind 1 2 attr ]), [])
  TermObj mappings -> Just (Rule term (Ident "R2") (Commands []), mappings)
  TermMap attr t -> Just (Rule term (Ident "R3") (Commands [CommandAdd 3]), [])
