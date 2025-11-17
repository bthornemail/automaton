/**
 * ML-Enhanced Vector Clock Automaton
 * 
 * Integrates WASM ML embeddings and HNSW indexing with vector clock state
 */

import { VectorClockAutomaton } from './vector-clock-automaton.js';
import type { MetaLog, SwarmContext, AutomatonMessage, AutomatonState } from './vector-clock-automaton.js';

// Re-export types for convenience
export type { MetaLog, SwarmContext, AutomatonMessage, AutomatonState };
import type { HNSWAutomatonIndex } from '../ml/hnsw-automaton-index.js';
import type { WASMMLEngine } from '../ml/wasm-ml-engine.js';

/**
 * ML-Enhanced Vector Clock Automaton
 * 
 * Base class for automata with ML-enhanced state tracking
 */
export class MLVectorClockAutomaton extends VectorClockAutomaton {
  protected mlEngine: WASMMLEngine;
  protected hnswIndex: HNSWAutomatonIndex | null;

  constructor(id: string | number, metaLog: MetaLog | null = null) {
    super(id, metaLog);
    
    // Note: These imports may not exist yet - they're placeholders for ML functionality
    // In a real implementation, these would be imported from '../ml/' directory
    // For now, we'll use 'any' type to allow compilation
    this.mlEngine = null as any; // new WASMMLEngine();
    this.hnswIndex = null; // Will be initialized when blackboard is available
    
    // Initialize ML engine
    // this.mlEngine.initialize().catch(error => {
    //   console.warn(`[Automaton ${id}] ML engine initialization warning:`, error);
    // });
  }

  /**
   * Set HNSW index (called by blackboard)
   * 
   * @param {HNSWAutomatonIndex} index - HNSW index instance
   */
  setHNSWIndex(index: HNSWAutomatonIndex): void {
    this.hnswIndex = index;
  }

  /**
   * Tick with ML-enhanced state tracking
   * 
   * @param {SwarmContext | null} swarm - Swarm context
   * @returns {Promise<void>}
   */
  async tick(swarm: SwarmContext | null = null): Promise<void> {
    // 1. Standard vector clock tick
    await super.tick(swarm);

    // 2. Generate embedding for current state
    const state = this.getState();
    
    try {
      if (this.mlEngine && typeof (this.mlEngine as any).embedAutomatonState === 'function') {
        const embedding = await (this.mlEngine as any).embedAutomatonState(state);

        // 3. Update HNSW index if available
        if (this.hnswIndex && typeof (this.hnswIndex as any).addAutomatonState === 'function') {
          await (this.hnswIndex as any).addAutomatonState(this.id, state);
        }

        // 4. Find similar automata using semantic search
        if (this.hnswIndex && typeof (this.hnswIndex as any).semanticSearch === 'function') {
          const similarAutomata = await (this.hnswIndex as any).semanticSearch(state, 5);

          // 5. Coordinate with similar automata
          await this.coordinateWithSimilar(similarAutomata, swarm);
        }
      }
    } catch (error) {
      console.warn(`[Automaton ${this.id}] ML tick error:`, error);
    }

    // 6. Execute automaton-specific logic
    await this.executeTick(swarm);
  }

  /**
   * Coordinate with semantically similar automata
   * 
   * @param {Array<{automatonId: string | number, similarity: number}>} similarAutomata - Array of similar automata with similarity scores
   * @param {SwarmContext | null} swarm - Swarm context
   * @returns {Promise<void>}
   */
  async coordinateWithSimilar(similarAutomata: Array<{automatonId: string | number; similarity: number}>, swarm: SwarmContext | null): Promise<void> {
    for (const similar of similarAutomata) {
      if (similar.similarity > 0.8 && similar.automatonId !== this.id) {
        // High similarity - coordinate
        const message: AutomatonMessage = {
          type: 'coordinate',
          from: this.id,
          similarity: similar.similarity,
          vectorClock: this.vectorClock.toMap()
        };

        await this.send(similar.automatonId, message);
      }
    }
  }

  /**
   * Receive with ML-enhanced conflict resolution
   * 
   * @param {string | number} from - Sender automaton ID
   * @param {AutomatonMessage} message - Received message
   * @returns {Promise<void>}
   */
  async receive(from: string | number, message: AutomatonMessage): Promise<void> {
    // 1. Standard vector clock merge
    await super.receive(from, message);

    // 2. Check for conflicts using semantic similarity
    if (message.type === 'state_update' && (message as any).state) {
      const remoteState = (message as any).state as AutomatonState;
      const localState = this.getState();

      try {
        if (this.mlEngine && typeof (this.mlEngine as any).embedAutomatonState === 'function') {
          // Check if states are semantically similar but causally concurrent
          const localEmbedding = await (this.mlEngine as any).embedAutomatonState(localState);
          const remoteEmbedding = await (this.mlEngine as any).embedAutomatonState(remoteState);

          const similarity = (this.mlEngine as any).cosineSimilarity(localEmbedding, remoteEmbedding);

          if (similarity > 0.9) {
            // Very similar states - resolve conflict
            await this.resolveSemanticConflict(localState, remoteState, from);
          }
        }
      } catch (error) {
        console.warn(`[Automaton ${this.id}] ML conflict resolution error:`, error);
      }
    }

    // 3. Execute automaton-specific receive
    await this.executeReceive(from, message);
  }

  /**
   * Resolve semantic conflicts using ML + Meta-Log
   * 
   * @param {AutomatonState} localState - Local automaton state
   * @param {AutomatonState} remoteState - Remote automaton state
   * @param {string | number} from - Sender automaton ID
   * @returns {Promise<void>}
   */
  async resolveSemanticConflict(localState: AutomatonState, remoteState: AutomatonState, from: string | number): Promise<void> {
    if (!this.metaLog) return;

    try {
      // Query Meta-Log for conflict resolution strategy
      const dimension = this.state.dimension || 0;
      
      // Determine strategy based on dimension
      let strategy: 'vector_clock_lww' | 'semantic_similarity' | 'homology_preservation' = 'vector_clock_lww'; // Default: Last-write-wins
      
      if (dimension === 0 || dimension === 1 || dimension === 6 || dimension === 7) {
        strategy = 'semantic_similarity';
      } else if (dimension === 2 || dimension === 5) {
        strategy = 'homology_preservation';
      }

      switch (strategy) {
        case 'semantic_similarity':
          // Use ML similarity for resolution
          if (this.mlEngine && typeof (this.mlEngine as any).embedAutomatonState === 'function') {
            const localEmbedding = await (this.mlEngine as any).embedAutomatonState(localState);
            const remoteEmbedding = await (this.mlEngine as any).embedAutomatonState(remoteState);
            
            // For now, keep local state if similarity is very high (simplified)
            // In production, would use query context to determine which is better
          }
          break;

        case 'vector_clock_lww':
          // Last-write-wins based on vector clock
          const localMax = Math.max(...Array.from(localState.vectorClock.values()));
          const remoteMax = Math.max(...Array.from(remoteState.vectorClock.values()));

          if (remoteMax > localMax) {
            // Remote is newer, merge
            await this.mergeState(remoteState);
          }
          break;

        case 'homology_preservation':
          // Use topological validation
          const localValid = await this.validateHomology(localState);
          const remoteValid = await this.validateHomology(remoteState);

          if (remoteValid && !localValid) {
            await this.mergeState(remoteState);
          }
          break;
      }
    } catch (error) {
      console.warn(`[Automaton ${this.id}] Conflict resolution error:`, error);
    }
  }

  /**
   * Query similar automata using ProLog + HNSW
   * 
   * @param {string} prologConstraints - ProLog query constraints
   * @param {number} k - Number of results
   * @returns {Promise<Array<AutomatonState>>} Similar automaton states
   */
  async querySimilarAutomata(prologConstraints: string, k: number = 10): Promise<Array<AutomatonState>> {
    if (!this.metaLog || !this.hnswIndex) {
      return [];
    }

    try {
      // 1. Query Meta-Log for automata matching ProLog constraints
      const matchingResults = await (this.metaLog as any).prologQuery(prologConstraints);

      // 2. Get states for matching automata
      const states: AutomatonState[] = [];
      for (const result of (matchingResults.bindings || []) as any[]) {
        const automatonId = result.AutomatonId || result.id || result.A;
        if (automatonId !== undefined) {
          try {
            if (this.hnswIndex && typeof (this.hnswIndex as any).getStateFromMetaLog === 'function') {
              const state = await (this.hnswIndex as any).getStateFromMetaLog(automatonId);
              states.push(state);
            }
          } catch (error) {
            // Skip if state not found
          }
        }
      }

      // 3. Rank by semantic similarity to current state
      if (this.mlEngine && typeof (this.mlEngine as any).embedAutomatonState === 'function') {
        const currentState = this.getState();
        const currentEmbedding = await (this.mlEngine as any).embedAutomatonState(currentState);

        const ranked = await Promise.all(
          states.map(async (state) => {
            const embedding = await (this.mlEngine as any).embedAutomatonState(state);
            const similarity = (this.mlEngine as any).cosineSimilarity(currentEmbedding, embedding);
            return { state, similarity };
          })
        );

        // 4. Sort by similarity and return top k
        return ranked
          .sort((a, b) => b.similarity - a.similarity)
          .slice(0, k)
          .map(r => r.state);
      }

      return states.slice(0, k);
    } catch (error) {
      console.error(`[Automaton ${this.id}] Query similar automata error:`, error);
      return [];
    }
  }

  /**
   * Merge state (override in subclasses)
   * 
   * @param {AutomatonState} state - State to merge
   * @returns {Promise<void>}
   */
  async mergeState(state: AutomatonState): Promise<void> {
    // Override in subclasses for specific merge logic
    const { VectorClock } = await import('./vector-clock.js');
    this.state = { ...this.state, ...state };
    const clockObj = Object.fromEntries(state.vectorClock);
    this.vectorClock = VectorClock.fromObject(this.id, clockObj);
  }

  /**
   * Validate homology (override in subclasses)
   * 
   * @param {AutomatonState} state - State to validate
   * @returns {Promise<boolean>} True if valid
   */
  async validateHomology(state: AutomatonState): Promise<boolean> {
    // Override in subclasses for homology validation
    // For now, return true (no validation)
    return true;
  }
}

