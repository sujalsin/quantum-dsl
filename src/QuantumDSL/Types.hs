{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module QuantumDSL.Types where

import Data.Complex

-- | Represents the number of qubits in a quantum register
newtype QubitCount = QubitCount Int
  deriving (Show, Eq)

-- | Complex amplitude in quantum states
type Amplitude = Complex Double

-- | Basic quantum gates
data QuantumGate
  = Hadamard      -- ^ Hadamard gate for superposition
  | PauliX        -- ^ NOT gate / Pauli-X
  | PauliY        -- ^ Pauli-Y gate
  | PauliZ        -- ^ Pauli-Z gate
  | CNOT          -- ^ Controlled-NOT gate
  | Phase Double  -- ^ Phase rotation gate
  | Toffoli       -- ^ Three-qubit controlled-NOT gate
  deriving (Show, Eq)

-- | Quantum circuit as a sequence of gates
data QuantumCircuit where
  Empty    :: QuantumCircuit
  Gate     :: QuantumGate -> [Int] -> QuantumCircuit -> QuantumCircuit
  MeasureQubit :: Int -> QuantumCircuit -> QuantumCircuit
  Parallel :: QuantumCircuit -> QuantumCircuit -> QuantumCircuit

-- | Quantum state representation
data QuantumState = QuantumState
  { stateVector :: [Amplitude]  -- ^ Amplitudes in computational basis
  , qubitCount  :: QubitCount   -- ^ Number of qubits in the state
  } deriving (Show, Eq)

-- | Result of quantum measurement
data MeasurementResult
  = Zero  -- ^ Measured |0⟩
  | One   -- ^ Measured |1⟩
  deriving (Show, Eq)

-- | Quantum program errors
data QuantumError
  = InvalidQubitIndex Int
  | InvalidQubitCount Int
  | InvalidGateApplication String
  | MeasurementError String
  deriving (Show, Eq)
