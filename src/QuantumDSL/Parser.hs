{-# LANGUAGE FlexibleContexts #-}

module QuantumDSL.Parser 
  ( parseProgram
  , parseCircuit
  ) where

import Text.Parsec hiding (Empty)
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token
import qualified Text.Parsec as P
import Control.Monad (void)
import Data.Char (toLower)

import QuantumDSL.Types (QuantumCircuit(Empty, Gate, MeasureQubit), QuantumGate(..))
import QuantumDSL.Syntax (Program(..), Statement(..), Expression(..))

-- Lexer definition
lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
  where
    style = emptyDef
      { Token.commentStart    = "/*"
      , Token.commentEnd      = "*/"
      , Token.commentLine     = "//"
      , Token.identStart      = letter
      , Token.identLetter    = alphaNum <|> oneOf "_'"
      , Token.reservedNames   = [ "init", "apply", "measure"
                               , "let", "hadamard", "paulix"
                               , "pauliy", "pauliz", "cnot"
                               , "phase", "toffoli"
                               ]
      , Token.reservedOpNames = ["+", "*", "=", "|"]
      }

-- Parser helpers
integer :: Parser Integer
integer = Token.integer lexer

identifier :: Parser String
identifier = Token.identifier lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

brackets :: Parser a -> Parser a
brackets = Token.brackets lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

-- Gate parsers
gateParser :: Parser QuantumGate
gateParser = choice
  [ Hadamard <$ reserved "hadamard"
  , PauliX <$ reserved "paulix"
  , PauliY <$ reserved "pauliy"
  , PauliZ <$ reserved "pauliz"
  , CNOT <$ reserved "cnot"
  , Toffoli <$ reserved "toffoli"
  , Phase <$> (reserved "phase" *> parens (fromIntegral <$> integer))
  ]

-- Qubit list parser
qubitListParser :: Parser [Int]
qubitListParser = brackets $ commaSep (fromIntegral <$> integer)
  where
    commaSep = Token.commaSep lexer

-- Statement parsers
initializeParser :: Parser Statement
initializeParser = do
  reserved "init"
  n <- fromIntegral <$> integer
  return $ Initialize n

applyParser :: Parser Statement
applyParser = do
  reserved "apply"
  gate <- gateParser
  qubits <- qubitListParser
  return $ Apply gate qubits

measureParser :: Parser Statement
measureParser = do
  reserved "measure"
  qubit <- fromIntegral <$> integer
  return $ Measure qubit

letBinding :: Parser Statement
letBinding = do
  string "let" >> spaces
  name <- identifier
  spaces >> char '=' >> spaces
  expr <- circuitExpr <|> (Variable <$> identifier)
  return $ Let name expr

statementParser :: Parser Statement
statementParser = choice
  [ initializeParser
  , applyParser
  , measureParser
  , letBinding
  ]

-- Expression parsers
circuitExpr :: Parser Expression
circuitExpr = do
  string "Circuit" >> spaces
  char '(' >> spaces
  circuit <- many1 statementParser
  char ')' >> spaces
  return $ Circuit (statementsToCircuit circuit)

variableExprParser :: Parser Expression
variableExprParser = Variable <$> identifier

sequenceExprParser :: Parser Expression
sequenceExprParser = Sequence <$> many1 expressionParser

expressionParser :: Parser Expression
expressionParser = choice
  [ circuitExpr
  , variableExprParser
  , parens sequenceExprParser
  ]

-- | Convert statements to a quantum circuit
statementsToCircuit :: [Statement] -> QuantumCircuit
statementsToCircuit [] = Empty
statementsToCircuit (stmt:stmts) = case stmt of
  Apply gate qubits -> Gate gate qubits (statementsToCircuit stmts)
  Measure qubit -> MeasureQubit qubit (statementsToCircuit stmts)
  _ -> error "Invalid statement in circuit"

-- Circuit parser
circuitParser :: Parser QuantumCircuit
circuitParser = do
  stmts <- many1 statementParser
  return $ statementsToCircuit stmts

-- Program parser
programParser :: Parser Program
programParser = do
  whiteSpace
  stmts <- many1 statementParser
  P.eof
  return $ Program stmts

-- Public parsing functions
parseProgram :: String -> Either ParseError Program
parseProgram = parse programParser ""

parseCircuit :: String -> Either ParseError QuantumCircuit
parseCircuit = parse circuitParser ""
