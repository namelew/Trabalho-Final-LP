module Lexer where 

import Data.Char 

data Ty = TBool
        | TNum
        | TPair
        | TFun Ty Ty 
        deriving (Show, Eq)

data Expr = BTrue
          | BFalse
          | Num Int 
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Not Expr
          | Or Expr Expr 
          | And Expr Expr 
          | If Expr Expr Expr 
          | Var String
          | Lam String Ty Expr 
          | App Expr Expr 
          | Pair Int Expr Expr
          | Let String Expr Expr
          | Paren Expr
          | Eq Expr Expr
          | Gth Expr Expr
          | Geq Expr Expr
          deriving (Show, Eq)

data Token = TokenTrue 
           | TokenFalse 
           | TokenNum Int 
           | TokenAdd
           | TokenSub
           | TokenMul
           | TokenNot 
           | TokenAnd
           | TokenOr
           | TokenIf 
           | TokenThen
           | TokenElse 
           | TokenVar String 
           | TokenLam
           | TokenDot
           | TokenComma
           | TokenColon
           | TokenArrow 
           | TokenLet
           | TokenAss
           | TokenIn
           | TokenLCurlyB
           | TokenRCurlyB
           | TokenLParen
           | TokenRParen
           | TokenBoolean
           | TokenNumber
           | TokenEq
           | TokenGth
           | TokenGeq
           deriving Show 

isToken :: Char -> Bool
isToken c = elem c "->&|=!"

lexer :: String -> [Token]
lexer [] = [] 
lexer ('+':cs) = TokenAdd : lexer cs
lexer ('*':cs) = TokenMul : lexer cs 
lexer ('\\':cs) = TokenLam : lexer cs
lexer (':':cs) = TokenColon : lexer cs
lexer ('(':cs) = TokenLParen : lexer cs
lexer (')':cs) = TokenRParen : lexer cs
lexer ('.':cs) = TokenDot : lexer cs
lexer (',':cs) = TokenComma : lexer cs
lexer ('{':cs) = TokenLCurlyB : lexer cs
lexer ('}':cs) = TokenRCurlyB : lexer cs
lexer (c:cs) | isSpace c = lexer cs 
             | isDigit c = lexNum (c:cs)
             | isAlpha c = lexKW (c:cs)
             | isToken c = lexSymbol (c:cs)
lexer _ = error "Lexical error: caracter inválido!"

lexNum :: String -> [Token]
lexNum cs = case span isDigit cs of 
              (num, rest) -> TokenNum (read num) : lexer rest 

lexKW :: String -> [Token]
lexKW cs = case span isAlpha cs of 
             ("true", rest)  -> TokenTrue : lexer rest 
             ("false", rest) -> TokenFalse : lexer rest 
             ("if", rest)    -> TokenIf : lexer rest 
             ("then", rest)  -> TokenThen : lexer rest 
             ("else", rest)  -> TokenElse : lexer rest 
             ("let", rest)  -> TokenLet : lexer rest 
             ("in", rest)  -> TokenIn : lexer rest 
             ("Bool", rest)  -> TokenBoolean : lexer rest 
             ("Number", rest)  -> TokenNumber : lexer rest 
             (var, rest)     -> TokenVar var : lexer rest 

lexSymbol :: String -> [Token]
lexSymbol cs = case span isToken cs of
                   ("->", rest) -> TokenArrow  : lexer rest
                   ("&&", rest) -> TokenAnd    : lexer rest
                   ("||", rest) -> TokenOr     : lexer rest
                   ("!", rest) -> TokenNot     : lexer rest
                   ("-", rest) -> TokenSub     : lexer rest
                   ("==", rest) -> TokenEq     : lexer rest
                   (">", rest) -> TokenGth     : lexer rest
                   (">=", rest) -> TokenGeq    : lexer rest
                   ("=", rest) -> TokenAss     : lexer rest

                   _ -> error "Lexical error: símbolo inválido!"