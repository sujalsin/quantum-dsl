{-# LANGUAGE FlexibleContexts #-}

module QuantumDSL.Core 
  ( initializeState
  , applyGate
  , measure
  , measureQubit
  , runCircuit
  , module QuantumDSL.Types
  ) where

import Data.Complex
import Data.Bits (testBit, setBit, clearBit, xor, shiftL)
import Data.List (transpose)
import QuantumDSL.Types

-- | Initialize a quantum state with the given number of qubits
initializeState :: QubitCount -> QuantumState
initializeState n@(QubitCount qc) = QuantumState
  { stateVector = 1.0 : replicate (2^qc - 1) 0.0  -- First state is |0...0⟩ with amplitude 1
  , qubitCount = n
  }

-- | Hadamard matrix
hadamardMatrix :: [[Complex Double]]
hadamardMatrix = 
  let h = 1 / sqrt 2
  in [ [h :+ 0, h :+ 0]
     , [h :+ 0, (-h) :+ 0]
     ]

-- | Pauli-X matrix (NOT gate)
pauliXMatrix :: [[Complex Double]]
pauliXMatrix = [[0 :+ 0, 1 :+ 0],
                [1 :+ 0, 0 :+ 0]]

-- | Pauli-Y matrix
pauliYMatrix :: [[Complex Double]]
pauliYMatrix = [[0 :+ 0, 0 :+ (-1)],
                [0 :+ 1, 0 :+ 0]]

-- | Pauli-Z matrix
pauliZMatrix :: [[Complex Double]]
pauliZMatrix = [[1 :+ 0, 0 :+ 0],
                [0 :+ 0, (-1) :+ 0]]

-- | Phase gate matrix
phaseMatrix :: Double -> [[Complex Double]]
phaseMatrix theta = [[1 :+ 0, 0 :+ 0],
                     [0 :+ 0, cos theta :+ sin theta]]

-- | Apply a quantum gate to the given qubits
applyGate :: QuantumGate -> [Int] -> QuantumState -> QuantumState
applyGate gate qubits state = 
  case gate of
    Hadamard -> applyHadamard (safeHead qubits) state
    PauliX   -> applyMatrix pauliXMatrix (safeHead qubits) state
    PauliY   -> applyMatrix pauliYMatrix (safeHead qubits) state
    PauliZ   -> applyMatrix pauliZMatrix (safeHead qubits) state
    CNOT     -> applyCNOT qubits state
    Phase theta -> applyMatrix (phaseMatrix theta) (safeHead qubits) state
    Toffoli  -> applyToffoli qubits state
  where
    safeHead [] = error "Empty qubit list"
    safeHead (x:_) = x

-- | Apply a matrix to a quantum state
applyMatrix :: [[Complex Double]] -> Int -> QuantumState -> QuantumState
applyMatrix matrix qubit state@QuantumState{qubitCount = QubitCount n, stateVector = sv}
  | qubit < 0 || qubit >= n = error $ "Invalid qubit index: " ++ show qubit
  | otherwise = state { stateVector = newStateVector }
  where
    dim = 2^n
    -- For each basis state, apply the matrix to the qubit's subspace
    newStateVector = [ if testBit i (n - 1 - qubit)  -- Reverse qubit indexing
                      then matrix !! 1 !! 0 * sv !! clearBit i (n - 1 - qubit) +
                           matrix !! 1 !! 1 * sv !! i
                      else matrix !! 0 !! 0 * sv !! i +
                           matrix !! 0 !! 1 * sv !! setBit i (n - 1 - qubit)
                    | i <- [0..dim-1]
                    ]

-- | Apply Hadamard gate
applyHadamard :: Int -> QuantumState -> QuantumState
applyHadamard qubit state@QuantumState{qubitCount = QubitCount n, stateVector = sv}
  | qubit < 0 || qubit >= n = error $ "Invalid qubit index: " ++ show qubit
  | otherwise = state { stateVector = newStateVector }
  where
    dim = 2^n
    h = 1 / sqrt 2
    newStateVector = [ if testBit i (n-1-qubit)
                      then h * sv !! clearBit i (n-1-qubit) - h * sv !! i
                      else h * sv !! i + h * sv !! setBit i (n-1-qubit)
                    | i <- [0..dim-1] ]

-- | CNOT matrix for two qubits
cnotMatrix :: [[Complex Double]]
cnotMatrix =
  [ [1, 0, 0, 0]
  , [0, 1, 0, 0]
  , [0, 0, 0, 1]
  , [0, 0, 1, 0]
  ]

-- | Apply CNOT gate
applyCNOT :: [Int] -> QuantumState -> QuantumState
applyCNOT qubits state@QuantumState{qubitCount = QubitCount n, stateVector = sv}
  | length qubits /= 2 = error $ "CNOT requires exactly 2 qubits, but got " ++ show (length qubits)
  | any (< 0) qubits || any (>= n) qubits = error $ "Invalid qubit index: " ++ show (maximum qubits)
  | otherwise = case qubits of
      [control, target] -> state { stateVector = newStateVector }
        where
          dim = 2^n
          -- For each basis state, if the control qubit is 1, flip the target qubit
          newStateVector = [ let i' = if testBit i (n - 1 - control)
                                   then flipBit i (n - 1 - target)
                                   else i
                           in sv !! i'
                         | i <- [0..dim-1] ]
          flipBit x pos = if testBit x pos
                         then clearBit x pos
                         else setBit x pos
      _ -> error $ "Invalid qubit list for CNOT gate"

-- | Apply Toffoli gate
applyToffoli :: [Int] -> QuantumState -> QuantumState
applyToffoli qubits state@QuantumState{qubitCount = QubitCount n, stateVector = sv}
  | length qubits /= 3 = error $ "Toffoli gate requires exactly 3 qubits, but got " ++ show (length qubits)
  | any (< 0) qubits || any (>= n) qubits = error $ "Invalid qubit index: " ++ show (maximum qubits)
  | otherwise = case qubits of
      [c1, c2, target] -> state { stateVector = newStateVector }
        where
          dim = 2^n
          newStateVector = [ if testBit i c1 && testBit i c2
                          then sv!!(flipBit i target)
                          else sv!!i
                        | i <- [0..dim-1] ]
          flipBit x b = if testBit x b then clearBit x b else setBit x b
      _ -> error $ "Invalid qubit list for Toffoli gate"

-- | Measure a single qubit
measure :: Int -> QuantumState -> (MeasurementResult, QuantumState)
measure qubit state@QuantumState{qubitCount = QubitCount n, stateVector = sv}
  | qubit < 0 || qubit >= n = error $ "Invalid qubit index: " ++ show qubit
  | otherwise = (result, collapsedState)
  where
    dim = 2^n
    -- Calculate probabilities for |0⟩ and |1⟩
    prob0 = sum [ magnitude (sv!!i) ^ (2 :: Int) | i <- [0..dim-1], not (testBit i (n-1-qubit)) ]
    prob1 = 1 - prob0
    
    -- For now, deterministically choose the more probable outcome
    -- In a real implementation, this should be random based on probabilities
    (result, normFactor) = if prob0 >= prob1 
                          then (Zero, sqrt prob0)
                          else (One, sqrt prob1)
    
    -- Collapse the state vector
    collapsedState = state { stateVector = normalizedSv }
    normalizedSv = [ if (result == Zero && not (testBit i (n-1-qubit))) ||
                       (result == One && testBit i (n-1-qubit))
                    then sv!!i / (normFactor :+ 0)
                    else 0 :+ 0
                    | i <- [0..dim-1] ]

-- | Measure a qubit in the computational basis
measureQubit :: Int -> QuantumState -> (MeasurementResult, QuantumState)
measureQubit qubit state@QuantumState{qubitCount = QubitCount n, stateVector = sv} 
  | qubit < 0 || qubit >= n = error $ "Invalid qubit index: " ++ show qubit
  | otherwise = (result, collapsedState)
  where
    dim = 2^n
    -- Calculate probabilities for |0⟩ and |1⟩ outcomes
    prob0 = sum [ magnitude (sv!!i) ^ 2 
                | i <- [0..dim-1]
                , not (testBit i (n-1-qubit)) ]
    prob1 = sum [ magnitude (sv!!i) ^ 2 
                | i <- [0..dim-1]
                , testBit i (n-1-qubit) ]
    
    -- For now, deterministically choose the more probable outcome
    -- In a real implementation, this should be random based on probabilities
    (result, normFactor) = if prob0 >= prob1 
                          then (Zero, sqrt prob0)
                          else (One, sqrt prob1)
    
    -- Collapse the state vector
    collapsedState = state { stateVector = normalizedSv }
    normalizedSv = [ if (result == Zero && not (testBit i (n-1-qubit))) ||
                       (result == One && testBit i (n-1-qubit))
                    then sv!!i / (normFactor :+ 0)
                    else 0 :+ 0
                    | i <- [0..dim-1] ]

-- | Run a quantum circuit on an initial state
runCircuit :: QuantumCircuit -> QuantumState -> QuantumState
runCircuit circuit state = case circuit of
  Empty -> state
  Gate g qubits next -> runCircuit next (applyGate g qubits state)
  MeasureQubit qubit next -> runCircuit next (snd (measureQubit qubit state))
  Parallel c1 c2 -> runCircuit c2 (runCircuit c1 state)
