module Examples.Teleportation where

import QuantumDSL.Types
import QuantumDSL.Core
import QuantumDSL.Parser
import QuantumDSL.Interpreter

-- | Quantum teleportation protocol
-- | Teleports the state of qubit 0 to qubit 2 using entanglement
teleportationProgram :: String
teleportationProgram = unlines
  [ "// Initialize three qubits"
  , "init 3"
  , ""
  , "// Prepare the state to teleport on qubit 0 (arbitrary state)"
  , "apply hadamard [0]"
  , "apply phase(45) [0]"
  , ""
  , "// Create Bell pair between qubits 1 and 2"
  , "apply hadamard [1]"
  , "apply cnot [1, 2]"
  , ""
  , "// Perform Bell measurement on qubits 0 and 1"
  , "apply cnot [0, 1]"
  , "apply hadamard [0]"
  , "measure 0"
  , "measure 1"
  , ""
  , "// Apply corrections based on measurement results"
  , "apply paulix [2]"
  , "apply pauliz [2]"
  , ""
  , "// Final state should be on qubit 2"
  , "measure 2"
  ]

-- | Run the quantum teleportation protocol
runTeleportation :: IO ()
runTeleportation = case parseProgram teleportationProgram of
  Left err -> putStrLn $ "Parse error: " ++ show err
  Right program -> case interpret program of
    Left err -> putStrLn $ "Runtime error: " ++ show err
    Right (finalState, _) -> 
      putStrLn $ "Final state: " ++ show (quantum finalState)
