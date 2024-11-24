module Examples.BellState where

import QuantumDSL.Types
import QuantumDSL.Core
import QuantumDSL.Parser
import QuantumDSL.Interpreter

-- | Create a Bell state (maximally entangled state)
-- | |β₀₀⟩ = (|00⟩ + |11⟩)/√2
bellStateProgram :: String
bellStateProgram = unlines
  [ "// Initialize two qubits in |00⟩ state"
  , "init 2"
  , ""
  , "// Apply Hadamard to first qubit"
  , "apply hadamard [0]"
  , ""
  , "// Apply CNOT with control=0, target=1"
  , "apply cnot [0, 1]"
  , ""
  , "// Measure both qubits"
  , "measure 0"
  , "measure 1"
  ]

-- | Run the Bell state program
runBellState :: IO ()
runBellState = case parseProgram bellStateProgram of
  Left err -> putStrLn $ "Parse error: " ++ show err
  Right program -> case interpret program of
    Left err -> putStrLn $ "Runtime error: " ++ show err
    Right (finalState, _) -> 
      putStrLn $ "Final state: " ++ show (quantum finalState)
