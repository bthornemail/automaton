# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.0.0] - 2025-01-07

### Added

#### Core Execution Engines
- **AdvancedSelfReferencingAutomaton**: Core automaton engine with self-modification capabilities
  - Integrated with `meta-log-db` for CanvasL/JSONL parsing
  - Async initialization via `init()` method
  - Self-modification execution with dimensional progression
  - Object pooling for memory optimization

- **ContinuousAutomatonRunner**: Built-in intelligence automaton runner
  - Continuous execution with configurable intervals
  - Optional Ollama integration for AI-powered decisions
  - Execution history tracking

- **OllamaAutomatonRunner**: AI-powered automaton using Ollama
  - Ollama API integration for decision-making
  - OpenCode model integration
  - Intelligent action selection

- **MemoryOptimizedAutomaton**: Memory-aware automaton
  - Automatic object trimming
  - Execution history limits
  - Periodic garbage collection
  - Memory pressure monitoring

- **EvolvedAutomaton**: Extended automaton with evolution tracking
- **ScalableAutomaton**: Scalable automaton for large-scale operations
- **LearningAutomaton**: Learning automaton with pattern tracking
- **OptimizedBootstrap**: Optimized bootstrap process

#### Vector Clock Systems
- **VectorClock**: Distributed causality tracking
  - Tick, merge, happens-before, concurrent detection
  - Causal chain tracking
  - Clone and serialization support

- **VectorClockAutomaton**: Base class for vector clock automata
  - Vector clock state tracking
  - Message passing with causality
  - Meta-Log integration for fact storage

- **MLVectorClockAutomaton**: ML-enhanced vector clock automaton
  - Semantic conflict resolution
  - HNSW indexing integration (placeholder)
  - WASM ML engine integration (placeholder)
  - ProLog + HNSW query support

- **Dimension Automata**: Dimensional automata (0D-7D)
  - 0D-Topology Automaton: Foundation automaton for topology operations

#### Memory Management
- **ObjectPool**: Generic object pool implementation
  - Object reuse for memory optimization
  - Configurable pool size
  - Reset functions for object reuse

- **Memory Utilities**: Memory monitoring and management
  - Memory state assessment
  - Pressure level detection (low/medium/high/critical)
  - Garbage collection helpers
  - Memory formatting utilities

#### Server Components
- **AutomatonController**: Server-side automaton controller
  - Optional Socket.IO integration
  - Start/stop/pause/resume controls
  - Real-time state updates

- **Automaton Analysis**: Performance analysis utilities
  - Action frequency calculation
  - Performance metrics

#### Type Definitions
- **AutomatonState**: Core automaton state interface
- **Transition**: Transition interface
- **VerticalTransition**: Vertical transition interface
- **CanvasObject**: Canvas object interface
- **MetaLog**: Meta-Log interface for vector clock automata
- **AutomatonMessage**: Message interface
- **SwarmContext**: Swarm context interface
- **MemoryState**: Memory state interface
- **MemoryPressure**: Memory pressure type

### Changed

- Extracted automaton engines from `evolutions/` directory
- Refactored to use `meta-log-db` for all CanvasL/JSONL operations
- Removed OpenCode dependencies (now uses `meta-log-db` exclusively)
- Made Socket.IO optional in server components
- Converted all JavaScript files to TypeScript with full type coverage

### Fixed

- Async initialization pattern (constructors no longer call async `load()`)
- Import paths updated for new package structure
- Type safety improvements throughout

### Documentation

- Complete README.md with usage examples
- API reference documentation
- Architecture overview
- Browser usage guide

### Infrastructure

- Package structure with Node.js and browser exports
- TypeScript configuration for Node.js and browser
- Rollup configuration for browser bundle
- Jest configuration for testing
- Build scripts for development and production

## [Unreleased]

### Planned

- Additional dimension automata (1D-7D)
- Complete ML engine integration
- HNSW index implementation
- Performance optimizations
- Extended test coverage
- Documentation improvements

