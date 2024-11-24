# Quantum DSL

A Domain-Specific Language for Quantum Computing Algorithms

## Overview

This project implements a domain-specific language (DSL) designed specifically for expressing quantum computing algorithms. The DSL aims to simplify quantum software development while enabling formal verification of quantum programs.

## Features

- Type-safe quantum circuit construction
- Built-in support for common quantum gates (Hadamard, CNOT, etc.)
- Quantum state manipulation and measurement
- Circuit composition and parallel execution
- Error handling and validation
- Implementation of fundamental quantum algorithms:
  - Quantum Teleportation
  - GHZ State preparation
- Variable binding support for circuit reuse
- Deterministic measurement for testing and verification

## Project Structure

```
quantum-dsl/
├── src/
│   └── QuantumDSL/
│       ├── Core.hs       # Core quantum operations and state management
│       ├── Types.hs      # Type definitions
│       ├── Syntax.hs     # DSL syntax and compilation
│       └── Interpreter.hs # DSL interpreter with quantum circuit execution
├── test/                 # Test suite with quantum algorithm verification
└── examples/            # Example quantum algorithms
```

## Getting Started

### Prerequisites

- GHC (Glasgow Haskell Compiler)
- Cabal (build system)

### Installation

```bash
git clone <repository-url>
cd quantum-dsl
cabal build
```

### Running Tests

```bash
cabal test
```

## Example Usage

Here's an example of quantum teleportation using our DSL:

```haskell
-- Initialize a quantum state to teleport
let circuit = do
    -- Create Bell pair between qubits 1 and 2
    hadamard 1
    cnot 1 2
    
    -- Prepare qubit 0 in state to teleport
    hadamard 0
    
    -- Perform teleportation protocol
    cnot 0 1
    hadamard 0
    
    -- Measure control qubits
    m1 <- measure 0
    m2 <- measure 1
    
    -- Apply corrections based on measurements
    when m2 $ pauliZ 2
    when m1 $ pauliX 2
```

## Current Features and Status

The DSL currently supports:
- Quantum state initialization and manipulation
- Basic quantum gates (X, Y, Z, H, CNOT)
- Measurement operations with deterministic outcomes for testing
- Variable binding for circuit reuse
- Implementation of fundamental quantum protocols
- Comprehensive test suite for quantum operations

### Testing
The project includes tests for:
- Basic quantum gates and state preparations
- GHZ state creation
- Quantum teleportation protocol
- Nested variable bindings
- State vector manipulations
