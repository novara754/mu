module Mu.Parser (Identifier, Aliased(..), AST(..), program) where

import Data.Void
import qualified Data.Text as T
import Text.Megaparsec as M
import Text.Megaparsec.Char
import Control.Monad.Combinators.NonEmpty as NE
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void T.Text

-- | An identifier for a bound or free variable.
type Identifier = T.Text

-- | Represents input to the REPL which is either an aliase lambda calculus expression
--   or an unaliased expression.
data Aliased
  = Aliased Identifier AST
  | Unaliased AST

-- | Represents the possible constructs of the language.
data AST
  = Variable Identifier        -- ^ Reference to a variable
  | Abstraction Identifier AST -- ^ Function definition with a bound variable and body
  | Application AST AST        -- ^ Function application
  deriving (Show, Eq)

-- | Parser for skipping whitespace and line comments
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") empty

-- | Parser for turning any parser into the same parser that also consumes any trailing
--   whitespace or comments.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Parser for matching any text and consuming any trailing whitespace or comments.
symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

-- | Turns any parser into the same parser that requires parentheses around its
--   input.
parenthesized :: Parser a -> Parser a
parenthesized p = do
  _ <- symbol "("
  v <- p
  _ <- symbol ")"
  return v

-- | Parse an identifier for an alias.
aliasIdent :: Parser Identifier
aliasIdent = T.pack <$> lexeme (M.some alphaNumChar)

-- | Parse an identifier for a variable.
varIdent :: Parser Identifier
varIdent = T.singleton <$> lexeme (lowerChar <?> "a lowercase character for variables")

-- | Parse a variable reference.
variable :: Parser AST
variable = Variable <$> (aliasIdent <|> varIdent)

-- | Parse an abstraction (function definition).
abstraction :: Parser AST
abstraction = do
  _ <- symbol "\\" <|> symbol "λ"
  ident <- varIdent
  _ <- symbol "."
  body <- application
  return $ Abstraction ident body

-- | Parse a function application.
application :: Parser AST
application = parenthesized application <|> foldl1 Application <$> NE.some term

-- | Parse a variable or abstraction
term :: Parser AST
term = choice
  [ abstraction
  , variable
  , parenthesized application
  ]

aliased :: Parser Aliased
aliased = do
  alias <- optional . try $ do
    ident <- aliasIdent
    _ <- symbol "="
    return ident
  expr <- application
  return $ maybe (Unaliased expr) (\a -> Aliased a expr) alias

-- | Parse a whole lambda calculus program (expression followed by eof).
program :: Parser Aliased
program = do
  ast <- aliased
  _ <- eof
  return ast
