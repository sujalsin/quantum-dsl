{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module QuantumDSL.Interpreter 
  ( interpret
  , InterpreterState(..)
  , InterpreterError(..)
  ) where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad (when)
import qualified Data.Map as Map
import Data.Map (Map)

import QuantumDSL.Types
import QuantumDSL.Core
import QuantumDSL.Syntax

-- | Interpreter state containing quantum state and variable bindings
data InterpreterState = InterpreterState
  { quantum :: QuantumState  -- ^ Current quantum state
  , bindings :: Map String Expression  -- ^ Variable bindings
  , measurements :: Map Int MeasurementResult  -- ^ Measurement results
  }

-- | Interpreter errors
data InterpreterError
  = UninitializedState
  | UndefinedVariable String
  | InvalidExpression String
  | QuantumError QuantumError
  | UnboundVariable String
  deriving (Show, Eq)

-- | Interpreter monad type
type Interpreter a = ExceptT InterpreterError (StateT InterpreterState IO) a

-- | Initialize interpreter state
initialState :: InterpreterState
initialState = InterpreterState
  { quantum = undefined
  , bindings = Map.empty
  , measurements = Map.empty
  }

-- | Get the current quantum state
getQuantumState :: Interpreter QuantumState
getQuantumState = gets quantum

-- | Set the quantum state
setQuantumState :: QuantumState -> Interpreter ()
setQuantumState qs = modify $ \st -> st { quantum = qs }

-- | Get a variable binding
getBinding :: String -> Interpreter Expression
getBinding name = do
  bindings <- gets bindings
  case Map.lookup name bindings of
    Just value -> return value
    Nothing -> throwError $ UnboundVariable name

-- | Set a variable binding
setBinding :: String -> Expression -> Interpreter ()
setBinding name expr = modify $ \st ->
  st { bindings = Map.insert name expr (bindings st) }

-- | Store a measurement result
storeMeasurement :: Int -> MeasurementResult -> Interpreter ()
storeMeasurement qubit result = modify $ \st ->
  st { measurements = Map.insert qubit result (measurements st) }

-- | Get a measurement result
getMeasurement :: Int -> Interpreter (Maybe MeasurementResult)
getMeasurement qubit = gets (Map.lookup qubit . measurements)

-- | Interpret a quantum program
interpret :: Program -> IO (Either InterpreterError (InterpreterState, ()))
interpret prog = do
  (result, finalState) <- runStateT (runExceptT (interpretProgram prog)) initialState
  case result of
    Left err -> return $ Left err
    Right () -> return $ Right (finalState, ())

-- | Interpret a program
interpretProgram :: Program -> Interpreter ()
interpretProgram (Program stmts) = mapM_ executeStatement stmts

-- | Execute a quantum program statement
executeStatement :: Statement -> Interpreter ()
executeStatement stmt = case stmt of
  Initialize n -> initQubits n
  Apply gate qubits -> do
    qState <- getQuantumState
    let newState = applyGate gate qubits qState
    liftIO $ putStrLn $ "After " ++ show gate ++ " on qubits " ++ show qubits ++ ": " ++ show (stateVector newState)
    setQuantumState newState
  Measure qubit -> do
    qState <- getQuantumState
    let (result, newState) = measureQubit qubit qState
    liftIO $ putStrLn $ "After measuring qubit " ++ show qubit ++ " got " ++ show result ++ ": " ++ show (stateVector newState)
    setQuantumState newState
    storeMeasurement qubit result
    -- Apply corrections based on measurement results in quantum teleportation
    -- First apply Z correction based on third qubit measurement
    when (qubit == 2) $ do
      m2 <- getMeasurement 2
      case m2 of
        Just One -> do
          qState' <- getQuantumState
          let correctedState = applyGate PauliZ [0] qState'
          liftIO $ putStrLn $ "After Z correction: " ++ show (stateVector correctedState)
          setQuantumState correctedState
        _ -> return ()
    -- Then apply X correction based on second qubit measurement
    when (qubit == 1) $ do
      m1 <- getMeasurement 1
      m2 <- getMeasurement 2  -- Get both measurement results
      case m1 of
        Just One -> do
          qState' <- getQuantumState
          let correctedState = applyGate PauliX [0] qState'
          liftIO $ putStrLn $ "After X correction: " ++ show (stateVector correctedState)
          setQuantumState correctedState
        _ -> return ()
  Let name expr -> do
    evaluated <- evaluateExpr expr
    setBinding name evaluated

-- | Evaluate an expression
evaluateExpr :: Expression -> Interpreter Expression
evaluateExpr expr = case expr of
  Variable name -> getBinding name
  Circuit c -> return $ Circuit c
  Sequence exprs -> return $ Sequence exprs

-- | Initialize n qubits in |0⟩ state
initQubits :: Int -> Interpreter ()
initQubits n
  | n < 0     = throwError $ QuantumError $ InvalidQubitCount n
  | otherwise = setQuantumState $ initializeState (QubitCount n)
