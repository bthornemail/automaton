# Self-Referencing JSONL Automaton

A meta-circular evaluator that reads, executes, and modifies its own JSONL definition while progressing through 8 dimensional levels from Church encoding to quantum computing.

## 🚀 Quick Start

### Option 1: Built-in Intelligence (No Ollama Required)
```bash
# Run for 50 iterations with 2-second intervals
npx tsx continuous-automaton.ts --max 50

# Run continuously with 1-second intervals
npx tsx continuous-automaton.ts 1000

# Run for 100 iterations with 500ms intervals
npx tsx continuous-automaton.ts 500 --max 100
```

### Option 2: With Ollama AI Control
```bash
# Setup Ollama (one-time)
./setup-ollama.sh

# Run with Ollama (default: llama3.2)
npx tsx ollama-automaton.ts

# Run with specific model and interval
npx tsx ollama-automaton.ts qwen2.5:3b 3000

# Run for limited iterations
npx tsx ollama-automaton.ts llama3.2 2000 50
```

## 🧠 Architecture

### Dimensional Progression
- **0D**: Identity (λx.x) - Self-reference foundation
- **1D**: Successor (λn.λf.λx.f(nfx)) - Temporal evolution
- **2D**: Pair (λx.λy.λf.fxy) - Pattern matching
- **3D**: Addition (λm.λn.λf.λx.mf(nfx)) - Algebraic composition
- **4D**: Network (localhost:8080) - File I/O operations
- **5D**: Consensus (blockchain) - Self-validation
- **6D**: Intelligence (neural_network) - Self-learning
- **7D**: Quantum (|ψ⟩ = α|0⟩ + β|1⟩) - Self-observation

### Self-Reference Pattern
Each automaton state contains:
```json
{
  "id": "0D-automaton",
  "selfReference": {
    "file": "automaton.jsonl",
    "line": 1,
    "pattern": "identity"
  }
}
```

### Actions
- **evolve**: Progress to next dimension
- **self-reference**: Execute self-reference pattern
- **self-modify**: Add new self-referential object
- **self-io**: Read/write own JSONL file
- **validate-self**: Check SHACL compliance
- **self-train**: Learn from execution history
- **self-observe**: Quantum observation and collapse
- **compose**: Compose multiple states

## 📁 Files

- `automaton.jsonl` - Self-referencing automaton definition
- `continuous-automaton.ts` - Built-in intelligence runner
- `ollama-automaton.ts` - Ollama-powered runner
- `advanced-automaton.ts` - Core automaton implementation
- `setup-ollama.sh` - Ollama installation script

## 🔄 Continuous Execution Features

### Built-in Intelligence Mode
- Periodic self-modification (every 20 iterations)
- Dimension-specific action selection
- Intelligent action sequencing
- Automatic state saving and analysis

### Ollama AI Mode
- AI-driven action selection
- Context-aware decision making
- Learning from execution history
- Adaptive behavior patterns

## 📊 Output Analysis

The automaton provides:
- Real-time dimensional progression tracking
- Self-reference integrity monitoring
- Dynamic object creation logging
- Execution history analysis
- SHACL validation reports

## 🛠️ Requirements

- Node.js with TypeScript
- (Optional) Ollama for AI control
- (Optional) Linux/macOS for setup script

## 🎯 Usage Examples

```bash
# Quick test run
npx tsx continuous-automaton.ts --max 10

# Production run with Ollama
./setup-ollama.sh
npx tsx ollama-automaton.ts llama3.2 5000

# Resource-efficient run
npx tsx continuous-automaton.ts 1000 --max 1000

# Development mode with fast iteration
npx tsx continuous-automaton.ts 100 --max 100
```

## 🔬 Self-Modification Demonstration

The automaton continuously:
1. Reads its own JSONL file structure
2. Executes dimensional transitions based on self-contained rules
3. Modifies its own file during execution
4. Maintains mathematical consistency through Church encoding
5. Implements quantum observation collapse back to 0D
6. Creates dynamic self-referential objects
7. Tracks execution history and learns from patterns

This creates a living computational topology that evolves and modifies itself according to dimensional progression specified in the AGENTS.md framework.
