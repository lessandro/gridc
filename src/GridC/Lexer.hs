module GridC.Lexer where

import Data.Functor.Identity (Identity)
import Text.Parsec (ParsecT, letter, oneOf, (<|>))
import Text.Parsec.Language (javaStyle)
import qualified Text.Parsec.Token as P

type Parser u a = ParsecT String u Identity a

lexer :: P.GenTokenParser String u Identity
lexer = P.makeTokenParser javaStyle {
    P.reservedNames = ["int", "void", "for", "while", "return"],
    P.identStart = letter <|> oneOf "_@"
}

parens :: Parser u a -> Parser u a
parens = P.parens lexer

braces :: Parser u a -> Parser u a
braces = P.braces lexer

brackets :: Parser u a -> Parser u a
brackets = P.brackets lexer

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

reservedOp :: String -> Parser u ()
reservedOp = P.reservedOp lexer
