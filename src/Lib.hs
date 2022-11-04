module Lib where

import Control.Monad (forM_)
import Control.Monad.Trans.Except
import Data.Text (Text)
import Data.Text qualified as Text
import Snail.Shell

data ArithAST
    = ATrue
    | AFalse
    | Zero
    | IsZero ArithAST
    | Succ ArithAST
    | Pred ArithAST
    | If ArithAST ArithAST ArithAST
    deriving stock (Eq, Show)

data ArithError
    = TextLiteralUnsupported
    | UnknownLexeme Text
    | UnableToAcceptBoolean Text
    | UnableToAcceptIf Text
    | KeywordOnlyAcceptsOneExpression Text
    | IfGivenInvalidExpression ArithAST
    | NotImplementedYet
    deriving stock (Eq, Show)

isZero :: ArithAST -> Except ArithError ArithAST
isZero = \case
    ATrue -> throwE $ UnableToAcceptBoolean "isZero"
    AFalse -> throwE $ UnableToAcceptBoolean "isZero"
    Zero -> pure ATrue
    IsZero _ -> throwE $ UnableToAcceptBoolean "isZero"
    Succ _ -> pure AFalse
    Pred _ -> pure AFalse
    If{} -> throwE $ UnableToAcceptIf "isZero"

succE :: ArithAST -> Except ArithError ArithAST
succE = \case
    ATrue -> throwE $ UnableToAcceptBoolean "succ"
    AFalse -> throwE $ UnableToAcceptBoolean "succ"
    Zero -> pure $ Succ Zero
    IsZero _ -> throwE $ UnableToAcceptBoolean "succ"
    x@(Succ _) -> pure $ Succ x
    Pred x -> pure x
    If{} -> throwE $ UnableToAcceptIf "succ"

predE :: ArithAST -> Except ArithError ArithAST
predE = \case
    ATrue -> throwE $ UnableToAcceptBoolean "pred"
    AFalse -> throwE $ UnableToAcceptBoolean "pred"
    Zero -> pure Zero
    IsZero _ -> throwE $ UnableToAcceptBoolean "pred"
    Succ x -> pure x
    Pred x -> predE x
    If{} -> throwE $ UnableToAcceptIf "pred"

ifE :: ArithAST -> ArithAST -> ArithAST -> Except ArithError ArithAST
ifE expr t f =
    case expr of
        ATrue -> pure t
        AFalse -> pure f
        _ -> throwE $ IfGivenInvalidExpression expr

snailToArith :: SExpression -> Except ArithError ArithAST
snailToArith = \case
    TextLiteral _ -> throwE TextLiteralUnsupported
    Lexeme (_, "true") -> pure ATrue
    Lexeme (_, "false") -> pure AFalse
    Lexeme (_, "zero") -> pure Zero
    Lexeme (_, unknown) -> throwE $ UnknownLexeme unknown
    SExpression _ [expr] -> snailToArith expr
    -- 'isZero' cases
    SExpression _ [Lexeme (_, "isZero"), Lexeme (_, "zero")] -> pure ATrue
    SExpression _ [Lexeme (_, "isZero"), Lexeme (_, "true")] ->
        throwE $ UnableToAcceptBoolean "isZero"
    SExpression _ [Lexeme (_, "isZero"), Lexeme (_, "false")] ->
        throwE $ UnableToAcceptBoolean "isZero"
    SExpression _ [Lexeme (_, "isZero"), expr] -> isZero =<< snailToArith expr
    SExpression _ (Lexeme (_, "isZero") : _) ->
        throwE $ KeywordOnlyAcceptsOneExpression "isZero"
    -- 'succ' cases
    SExpression _ [Lexeme (_, "succ"), Lexeme (_, "zero")] -> pure $ Succ Zero
    SExpression _ [Lexeme (_, "succ"), expr] -> succE =<< snailToArith expr
    SExpression _ (Lexeme (_, "succ") : _) ->
        throwE $ KeywordOnlyAcceptsOneExpression "succ"
    -- 'pred' cases
    SExpression _ [Lexeme (_, "pred"), Lexeme (_, "zero")] -> pure Zero
    SExpression _ [Lexeme (_, "pred"), expr] -> predE =<< snailToArith expr
    SExpression _ (Lexeme (_, "pred") : _) ->
        throwE $ KeywordOnlyAcceptsOneExpression "pred"
    -- 'if' cases
    SExpression _ [Lexeme (_, "if"), Lexeme (_, "true"), expr, _] -> snailToArith expr
    SExpression _ [Lexeme (_, "if"), Lexeme (_, "false"), _, expr] -> snailToArith expr
    SExpression _ [Lexeme (_, "if"), expr, trueExpr, falseExpr] -> do
        expr' <- snailToArith expr
        true' <- snailToArith trueExpr
        false' <- snailToArith falseExpr
        ifE expr' true' false'

    -- TODO...
    SExpression _ [] -> throwE NotImplementedYet
    SExpression _ ((TextLiteral _) : _ : _) -> throwE NotImplementedYet
    SExpression _ ((SExpression _ _) : _ : _) -> throwE NotImplementedYet
    -- Catch-all for anything we may have missed...
    _ -> throwE NotImplementedYet

sExpressionToString :: SExpression -> String
sExpressionToString = \case
    TextLiteral (_, txt) -> "\"" <> Text.unpack txt <> "\""
    Lexeme (_, lexeme) -> Text.unpack lexeme
    SExpression Nothing exprs ->
        let strings = unwords $ sExpressionToString <$> exprs
         in "(" <> strings <> ")"
    SExpression (Just c) exprs ->
        let strings = unwords $ sExpressionToString <$> exprs
         in pure c <> "(" <> strings <> ")"

main :: IO ()
main = do
    eSExpressions <- readSnailFile "./arith/hello.arith"
    case eSExpressions of
        Left err -> print err
        Right snailAsts -> forM_ snailAsts $ \snail -> do
            let arith = runExcept $ snailToArith snail
            putStrLn $ sExpressionToString snail <> " ==> " <> show arith
