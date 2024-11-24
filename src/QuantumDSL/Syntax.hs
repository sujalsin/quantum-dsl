{-# LANGUAGE GADTs #-}

module QuantumDSL.Syntax 
  ( Program(..)
  , Statement(..)
  , Expression(..)
  , compile
  ) where

import QuantumDSL.Types

-- | A quantum program is a sequence of statements
data Program where
  Program :: [Statement] -> Program

-- | Statements in our quantum DSL
data Statement where
  Initialize :: Int -> Statement              -- ^ Initialize n qubits
  Apply      :: QuantumGate -> [Int] -> Statement  -- ^ Apply a gate to qubits
  Measure    :: Int -> Statement             -- ^ Measure a qubit
  Let        :: String -> Expression -> Statement  -- ^ Bind a value to a name

-- | Expressions in our quantum DSL
data Expression where
  Circuit   :: QuantumCircuit -> Expression
  Variable  :: String -> Expression
  Sequence  :: [Expression] -> Expression

-- | Compile a program into a quantum circuit
compile :: Program -> Either String QuantumCircuit
compile (Program stmts) = Right $ foldr convertStatement Empty stmts
  where
    convertStatement :: Statement -> QuantumCircuit -> QuantumCircuit
    convertStatement stmt next = case stmt of
      Apply gate qubits -> Gate gate qubits next
      Measure qubit    -> MeasureQubit qubit next
      _               -> next  -- Other statements are handled differently
