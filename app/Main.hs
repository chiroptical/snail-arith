module Main where

import Snail.Shell
import Data.Text qualified as Text

data ArithAST
  = ATrue
  | AFalse
  | Zero
  | IsZero ArithAST
  | Succ ArithAST
  | Pred ArithAST
  | If ArithAST ArithAST ArithAST
  deriving (Eq, Show)

snailToArith :: SExpression -> ArithAST
snailToArith = \case
  TextLiteral _ -> error "arith doesn't support text literals"

  Lexeme (_, "true") -> ATrue
  Lexeme (_, "false") -> AFalse
  Lexeme (_, "zero") -> Zero
  Lexeme (_, unknown) ->
    error $ "encountered unknown lexeme `" <> Text.unpack unknown <> "`"

  -- 'isZero' cases
  SExpression _ [Lexeme (_, "isZero"), Lexeme (_, "zero")] -> ATrue
  SExpression _ [Lexeme (_, "isZero"), Lexeme (_, "true")] -> AFalse
  SExpression _ [Lexeme (_, "isZero"), Lexeme (_, "false")] -> AFalse
  SExpression prefix [l@(Lexeme (_, "isZero")), expr] -> IsZero $ snailToArith expr
  SExpression _ sexpr@(Lexeme (_, "isZero") : _) ->
    error "isZero can take a lexeme or expression, i.e. `isZero zero` or `isZero (zero)`"

  _ -> ATrue

main :: IO ()
main = do
  eSExpressions <- readSnailFile "./arith/hello.arith"
  case eSExpressions of
    Left err -> print err
    Right snailAsts -> print $ snailToArith <$> snailAsts
