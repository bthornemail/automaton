# Automata Metaverse

Automaton execution engines for self-referential CanvasL/JSONL systems with dimensional progression (0D-7D), Church encoding, and distributed causality tracking.

## Overview

`automata-metaverse` provides a complete suite of automaton execution engines for self-referential computational systems. It integrates with `meta-log-db` for CanvasL/JSONL parsing, ProLog/DataLog queries, and R5RS function execution.

## Features

### Core Execution Engines

- **AdvancedSelfReferencingAutomaton**: Core automaton engine with self-modification capabilities
- **ContinuousAutomatonRunner**: Built-in intelligence automaton with continuous execution
- **OllamaAutomatonRunner**: AI-powered automaton using Ollama for decision-making
- **MemoryOptimizedAutomaton**: Memory-aware automaton with automatic trimming and GC
- **EvolvedAutomaton**: Extended automaton with evolution tracking
- **ScalableAutomaton**: Scalable automaton for large-scale operations
- **LearningAutomaton**: Learning automaton with pattern tracking
- **OptimizedBootstrap**: Optimized bootstrap process for automaton initialization

### Vector Clock Systems

- **VectorClock**: Distributed causality tracking using vector clocks
- **VectorClockAutomaton**: Base class for automata with vector clock state tracking
- **MLVectorClockAutomaton**: ML-enhanced vector clock automaton with semantic conflict resolution
- **Dimension Automata**: Dimensional automata (0D-7D) for topology operations

### Memory Management

- **ObjectPool**: Generic object pool for memory optimization
- **Memory Utilities**: Memory state monitoring, pressure assessment, and GC helpers

### Server Components

- **AutomatonController**: Server-side automaton controller with Socket.IO integration (optional)
- **Automaton Analysis**: Action frequency calculation and performance metrics

## Installation

```bash
npm install automata-metaverse
```

### Dependencies

- **`meta-log-db`**: Required for CanvasL/JSONL parsing and R5RS function execution
- **`automaton-evolutions`**: Recommended for canonical automaton CanvasL files (A₀-A₁₁)

## Usage

### Using Canonical Automaton Files

The `automaton-evolutions` package provides canonical CanvasL files for all A₀-A₁₁ automata:

```typescript
import { AdvancedSelfReferencingAutomaton } from 'automata-metaverse';
import { MetaLogDb } from 'meta-log-db';
import { AUTOMATON_FILES } from 'automaton-evolutions';

const db = new MetaLogDb();
const automaton = new AdvancedSelfReferencingAutomaton(
  AUTOMATON_FILES.a0Unified,  // Use canonical unified automaton
  db
);
```

### Basic Usage

```typescript
import { AdvancedSelfReferencingAutomaton } from 'automata-metaverse';
import { MetaLogDb } from 'meta-log-db';

// Create MetaLogDb instance
const db = new MetaLogDb({ enableProlog: true, enableDatalog: true });

// Create automaton
const automaton = new AdvancedSelfReferencingAutomaton('./automaton.jsonl', db);

// Initialize (loads the file)
await automaton.init();

// Execute self-modification
await automaton.executeSelfIO();
```

### Continuous Execution

```typescript
import { ContinuousAutomatonRunner } from 'automata-metaverse';

const runner = new ContinuousAutomatonRunner('./automaton.jsonl', false, 'llama3.2');

// Start continuous execution
await runner.startContinuous(2000, 100); // 2s interval, 100 iterations max
```

### Memory-Optimized Execution

```typescript
import { MemoryOptimizedAutomaton } from 'automata-metaverse';

const automaton = new MemoryOptimizedAutomaton('./automaton.jsonl', {
  maxObjects: 2000,
  maxExecutionHistory: 500,
  gcInterval: 5000,
  trimInterval: 10000,
  memoryPressureThreshold: 200, // MB
  enableGC: true,
});

await automaton.init();
await automaton.executeSelfIO();
```

### Vector Clock Automata

```typescript
import { VectorClockAutomaton, type MetaLog } from 'automata-metaverse';

class MyAutomaton extends VectorClockAutomaton {
  protected async executeTick(swarm: SwarmContext | null): Promise<void> {
    // Your tick logic here
    console.log(`Tick ${this.vectorClock.getTick()}`);
  }
}

const automaton = new MyAutomaton('automaton-1', metaLog);
await automaton.tick();
```

### Server-Side Control

```typescript
import { AutomatonController } from 'automata-metaverse/server';
import { Server } from 'socket.io';

const io = new Server(server);
const automaton = new AdvancedSelfReferencingAutomaton('./automaton.jsonl', db);
await automaton.init();

const controller = new AutomatonController({
  automaton,
  io // Optional Socket.IO server
});

// Start automaton
controller.start(2000); // 2s interval

// Stop automaton
controller.stop();
```

## Browser Usage

For browser environments, use the browser-specific exports:

```typescript
import { AdvancedSelfReferencingAutomaton } from 'automata-metaverse/browser';
import { CanvasLMetaverseBrowser } from 'meta-log-db/browser';

const db = new CanvasLMetaverseBrowser();
const automaton = new AdvancedSelfReferencingAutomaton('./automaton.jsonl', db);
await automaton.init();
```

## Architecture

### Dimensional Progression (0D-7D)

The automaton system implements dimensional progression:

- **0D**: Topology (quantum vacuum, identity processes)
- **1D**: Temporal (successor operations, temporal evolution)
- **2D**: Structural (pairing, pattern encoding, bipartite graphs)
- **3D**: Algebraic (addition, multiplication, exponentiation)
- **4D**: Network (spacetime, network operations, CI/CD)
- **5D**: Consensus (distributed consensus, blockchain operations)
- **6D**: Intelligence (AI operations, neural networks, test analysis)
- **7D**: Quantum (quantum superposition, entanglement)

### Vector Clock Causality

Vector clocks track distributed causality:

- **Tick**: Increment own clock
- **Merge**: Element-wise max with other clocks
- **Happens-Before**: Causal ordering detection
- **Concurrent**: Detect concurrent events

### Memory Management

Automatic memory management:

- **Object Pooling**: Reuse objects to reduce allocation
- **Automatic Trimming**: Trim objects and execution history
- **GC Integration**: Force garbage collection at thresholds
- **Pressure Monitoring**: Monitor memory pressure levels

## API Reference

### Core Engines

#### `AdvancedSelfReferencingAutomaton`

Core automaton engine with self-modification.

```typescript
class AdvancedSelfReferencingAutomaton {
  constructor(filePath: string, db?: MetaLogDb);
  async init(): Promise<void>;
  async executeSelfIO(): Promise<void>;
  async executeSelfModification(): Promise<void>;
  save(): void;
}
```

#### `ContinuousAutomatonRunner`

Continuous execution runner.

```typescript
class ContinuousAutomatonRunner {
  constructor(automatonFile: string, useOllama: boolean, ollamaModel: string, db?: MetaLogDb);
  async init(): Promise<void>;
  async startContinuous(intervalMs: number, maxIterations?: number): Promise<void>;
  stop(): void;
}
```

### Vector Clock Systems

#### `VectorClock`

Distributed causality tracking.

```typescript
class VectorClock {
  constructor(automatonId: string | number, initialClock?: Map<string | number, number>);
  tick(): number;
  getTick(): number;
  merge(otherClock: Map<string | number, number> | VectorClock): VectorClock;
  happensBefore(otherClock: Map<string | number, number> | VectorClock): boolean;
  isConcurrent(otherClock: Map<string | number, number> | VectorClock): boolean;
}
```

### Memory Management

#### `ObjectPool<T>`

Generic object pool.

```typescript
class ObjectPool<T> {
  constructor(createFn: () => T, resetFn: (obj: T) => void, maxSize?: number);
  acquire(): T;
  release(obj: T): void;
  clear(): void;
  get size(): number;
}
```

#### Memory Utilities

```typescript
function getMemoryState(): MemoryState;
function assessMemoryPressure(heapUsed: number): MemoryPressure;
function forceGarbageCollection(): boolean;
function formatMemory(bytes: number): string;
```

## Dependencies

- **meta-log-db**: Core database and logic programming engine
- **socket.io** (peer): Optional Socket.IO server for real-time control

## Requirements

- Node.js >= 18.0.0
- TypeScript >= 5.0.0

## Development

```bash
# Install dependencies
npm install

# Build
npm run build:all

# Test
npm test

# Watch mode
npm run watch
```

## License

MIT

## Related Packages

- [meta-log-db](https://www.npmjs.com/package/meta-log-db): Core database and logic programming engine
- [@meta-log/plugin](https://www.npmjs.com/package/@meta-log/plugin): Plugin infrastructure

## Contributing

Contributions welcome! Please open an issue or submit a pull request.

## Author

Brian Thorne - [GitHub](https://github.com/bthornemail)

