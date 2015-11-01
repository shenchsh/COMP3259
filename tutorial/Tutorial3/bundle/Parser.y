{
module Parser (parseExpr) where
import Data.Char (isDigit, isSpace, isAlpha)
import Data.List (stripPrefix)
import Declare (Exp(..))
}


%name parser
%tokentype { Token }
%error { parseError }

%token
    var     { TokenKeyword "var" }
    id      { TokenIdent $$ }
    int     { TokenInt $$ }
    '+'     { TokenSymbol "+" }
    '-'     { TokenSymbol "-" }
    '*'     { TokenSymbol "*" }
    '/'     { TokenSymbol "/" }
    '('     { TokenSymbol "(" }
    ')'     { TokenSymbol ")" }
    ';'     { TokenSymbol ";" }
    '='     { TokenSymbol "=" }

%right ';'
%left '+' '-'
%left '*' '/'

%%

Exp : var id '=' Exp ';' Exp { Decl $2 $4 $6 }
    | Exp '+' Exp            { Add $1 $3 }
    | Exp '-' Exp            { Sub $1 $3 }
    | Exp '*' Exp            { Mult $1 $3 }
    | Exp '/' Exp            { Div $1 $3 }
    | int                    { Num $1 }
    | id                     { Var $1 }
    | '(' Exp ')'            { $2 }


{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token
      = TokenInt Int
      | TokenKeyword String
      | TokenSymbol String
      | TokenIdent String

lexer :: [String] -> [String] -> String -> [Token]
lexer symbols keywords = lexer'
  where lexer' [] = []
        lexer' s@(c:cs)
          | isSpace c = lexer' cs
          | isDigit c = lexNum s
          | isAlpha c = lexVar s
          | otherwise = lexSym s symbols

        lexNum cs = TokenInt (read num) : lexer' rest
          where (num, rest) = span isDigit cs

        lexVar cs = token : lexer' rest
          where (var, rest) = span isAlpha cs
                token = if var `elem` keywords
                        then TokenKeyword var
                        else TokenIdent var

        lexSym cs (s:ss) = case stripPrefix s cs of
                            Just rest -> TokenSymbol s : lexer' rest
                            Nothing -> lexSym cs ss
        lexSym cs [] = error $ "Cannot tokenize " ++ cs

symbols = ["+", "-", "*", "/", "(", ")", "^", ";", "="]
keywords = ["var"]

parseExpr = parser . lexer symbols keywords

}
