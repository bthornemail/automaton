/**
 * Automata Metaverse - Browser Entry Point
 * 
 * Browser-compatible exports (excludes Node.js-specific features)
 */

// Core execution engines (browser-compatible)
export { AdvancedSelfReferencingAutomaton } from '../engines/advanced-automaton.js';
export { ContinuousAutomatonRunner } from '../engines/continuous-automaton.js';
export { OllamaAutomatonRunner } from '../engines/ollama-automaton.js';
export { MemoryOptimizedAutomaton } from '../engines/automaton-memory-optimized.js';
export { EvolvedAutomaton } from '../engines/automaton-evolved.js';
export { ScalableAutomaton } from '../engines/automaton-scalable.js';
export { LearningAutomaton } from '../engines/learning-automaton.js';

// Vector clock systems
export { VectorClock } from '../vector-clock/vector-clock.js';
export { VectorClockAutomaton, type MetaLog, type AutomatonState, type AutomatonMessage, type SwarmContext } from '../vector-clock/vector-clock-automaton.js';
export { MLVectorClockAutomaton } from '../vector-clock/ml-vector-clock-automaton.js';
export { A0_TopologyAutomaton } from '../vector-clock/dimension-automata/0d-topology-automaton.js';

// Memory management (browser-compatible)
export { ObjectPool } from '../memory/object-pool.js';
export { 
  assessMemoryPressure, 
  formatMemory,
  type MemoryPressure
} from '../memory/memory-utils.js';

// Types
export type { AutomatonState, Transition, VerticalTransition, CanvasObject } from '../types/automaton-state.js';

