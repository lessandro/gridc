module GridC.Lexer where

import Data.Functor.Identity
import Text.Parsec (ParsecT)
import Text.Parsec.Language (javaStyle)
import qualified Text.Parsec.Token as P

type Parser u a = ParsecT String u Identity a

lexer :: P.GenTokenParser String u Identity
lexer = P.makeTokenParser javaStyle {
    P.reservedNames = ["int", "void", "for", "while", "return"]
}

parens :: Parser u a -> Parser u a
parens = P.parens lexer

braces :: Parser u a -> Parser u a
braces = P.braces lexer

identifier :: Parser u String
identifier = P.identifier lexer

symbol :: String -> Parser u String
symbol = P.symbol lexer

commaSep :: Parser u a -> Parser u [a]
commaSep = P.commaSep lexer

semiSep :: Parser u a -> Parser u [a]
semiSep = P.semiSep lexer

comma :: Parser u String
comma = P.comma lexer

semi :: Parser u String
semi = P.semi lexer

integer :: Parser u Integer
integer = P.integer lexer
