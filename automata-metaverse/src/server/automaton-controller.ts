/**
 * Automaton Controller
 * 
 * Handles automaton control functions (start, stop, execute actions, etc.)
 */

import { AdvancedSelfReferencingAutomaton } from '../engines/advanced-automaton.js';
import type { Server as SocketIOServer } from 'socket.io';

export interface AutomatonState {
  automaton: AdvancedSelfReferencingAutomaton;
  isRunning: boolean;
  intervalId: NodeJS.Timeout | null;
}

export interface AutomatonControllerConfig {
  automaton: AdvancedSelfReferencingAutomaton;
  io?: SocketIOServer;  // Optional Socket.IO server
}

export class AutomatonController {
  private state: AutomatonState;
  private io?: SocketIOServer;

  constructor(config: AutomatonControllerConfig) {
    this.state = {
      automaton: config.automaton,
      isRunning: false,
      intervalId: null,
    };
    this.io = config.io;
  }

  /**
   * Emit event to Socket.IO if available
   */
  private emit(event: string, data: any): void {
    if (this.io) {
      this.io.emit(event, data);
    }
  }

  /**
   * Start automaton execution
   */
  start(intervalMs: number, maxIterations: number): void {
    if (this.state.isRunning) return;

    this.state.isRunning = true;
    let iterationCount = 0;

    console.log(`🚀 Starting automaton with ${intervalMs}ms interval`);

    this.state.intervalId = setInterval(() => {
      if (iterationCount >= maxIterations) {
        this.stop();
        return;
      }

      // Get smart action based on current state
      const currentDimension = (this.state.automaton as any).currentDimension;
      const action = this.getSmartAction(currentDimension, iterationCount);
      
      this.executeAction(action);
      
      // Emit status update
      this.emit('status', {
        isRunning: true,
        currentDimension: (this.state.automaton as any).currentDimension,
        iterationCount: (this.state.automaton as any).executionHistory.length,
        selfModificationCount: (this.state.automaton as any).selfModificationCount,
        totalObjects: (this.state.automaton as any).objects.length,
        executionMode: 'builtin',
        status: 'running',
        lastAction: action,
      });

      iterationCount++;
    }, intervalMs);
  }

  /**
   * Stop automaton execution
   */
  stop(): void {
    if (!this.state.isRunning) return;

    this.state.isRunning = false;
    
    if (this.state.intervalId) {
      clearInterval(this.state.intervalId);
      this.state.intervalId = null;
    }

    console.log('🛑 Automaton stopped');

    // Emit status update
    if (this.io) {
      this.io.emit('status', {
        isRunning: false,
        currentDimension: (this.state.automaton as any).currentDimension || '0D',
        iterationCount: (this.state.automaton as any).executionHistory?.length || 0,
        selfModificationCount: (this.state.automaton as any).selfModificationCount || 0,
        totalObjects: (this.state.automaton as any).objects?.length || 0,
        executionMode: 'builtin',
        status: 'idle',
      });
    }
  }

  /**
   * Execute a single action (exponential, forward propagation)
   * Actions are exponential transformations: Affine → Projective
   * Effect: Forward propagation, exponential growth
   */
  executeAction(action: string): void {
    try {
      console.log(`🎯 Executing ACTION (exponential): ${action}`);
      
      const currentDim = (this.state.automaton as any).currentDimension;
      const iterationCount = (this.state.automaton as any).executionHistory?.length || 0;
      let fromDim = currentDim;
      let toDim = currentDim;

      // Actions are exponential (forward, projective)
      switch (action) {
        case 'evolve':
          (this.state.automaton as any).executeEvolution();
          this.progressDimension();
          toDim = (this.state.automaton as any).currentDimension;
          break;
        case 'self-modify':
          (this.state.automaton as any).executeSelfModification();
          if (this.io) {
            this.io.emit('modification', { type: 'self-modify', timestamp: Date.now() });
          }
          break;
        case 'self-io':
          (this.state.automaton as any).executeSelfIO();
          break;
        case 'self-train':
          (this.state.automaton as any).executeSelfTraining();
          break;
        case 'compose':
          (this.state.automaton as any).executeComposition();
          break;
        default:
          console.warn(`Unknown action: ${action}`);
      }

      // Add to execution history
      const historyEntry = {
        iteration: typeof iterationCount === 'number' ? iterationCount : 0,
        action: typeof action === 'string' ? action : 'unknown',
        from: typeof fromDim === 'number' ? `${fromDim}D` : 'unknown',
        to: typeof toDim === 'number' ? `${toDim}D` : 'unknown',
        timestamp: Date.now()
      };
      
      if (historyEntry && typeof historyEntry === 'object' && !Array.isArray(historyEntry)) {
        if (!(this.state.automaton as any).executionHistory) {
          (this.state.automaton as any).executionHistory = [];
        }
        (this.state.automaton as any).executionHistory.push(historyEntry);
      }

      // Emit action execution (exponential transformation)
      const actionData = {
        action: typeof action === 'string' ? action : 'unknown',
        result: 'success',
        timestamp: Date.now(),
        from: typeof fromDim === 'number' ? `${fromDim}D` : 'unknown',
        to: typeof toDim === 'number' ? `${toDim}D` : 'unknown',
        iteration: typeof iterationCount === 'number' ? iterationCount : 0,
        type: 'action', // exponential
        transformation: 'exponential' // forward propagation
      };
      
      if (actionData && typeof actionData === 'object' && !Array.isArray(actionData) && this.io) {
        this.io.emit('action', actionData);
      }

    } catch (error) {
      console.error(`❌ Failed to execute action ${action}:`, error);
      let errorMessage: string;
      try {
        errorMessage = error instanceof Error ? error.message : String(error);
      } catch (e) {
        errorMessage = 'Unknown error occurred';
      }
      if (this.io) {
        this.io.emit('error', { action: action, error: errorMessage });
      }
    }
  }

  /**
   * Execute observation (linear, backward propagation)
   * Observations are linear transformations: Projective → Affine
   * Effect: Backward propagation, linear collapse
   */
  executeObservation(observation: string): void {
    try {
      console.log(`👁️ Executing OBSERVATION (linear): ${observation}`);
      
      const currentDim = (this.state.automaton as any).currentDimension;
      const iterationCount = (this.state.automaton as any).executionHistory?.length || 0;

      // Observations are linear (backward, affine)
      switch (observation) {
        case 'self-reference':
          (this.state.automaton as any).executeSelfReference();
          break;
        case 'validate-self':
          (this.state.automaton as any).executeSelfValidation();
          break;
        case 'self-observe':
          (this.state.automaton as any).executeSelfObservation();
          break;
        default:
          console.warn(`Unknown observation: ${observation}`);
      }

      // Add to execution history
      const historyEntry = {
        iteration: typeof iterationCount === 'number' ? iterationCount : 0,
        observation: typeof observation === 'string' ? observation : 'unknown',
        dimension: typeof currentDim === 'number' ? `${currentDim}D` : 'unknown',
        timestamp: Date.now(),
        type: 'observation', // linear
        transformation: 'linear' // backward propagation
      };
      
      if (historyEntry && typeof historyEntry === 'object' && !Array.isArray(historyEntry)) {
        if (!(this.state.automaton as any).executionHistory) {
          (this.state.automaton as any).executionHistory = [];
        }
        (this.state.automaton as any).executionHistory.push(historyEntry);
      }

      // Emit observation execution (linear transformation)
      const observationData = {
        observation: typeof observation === 'string' ? observation : 'unknown',
        result: 'success',
        timestamp: Date.now(),
        dimension: typeof currentDim === 'number' ? `${currentDim}D` : 'unknown',
        iteration: typeof iterationCount === 'number' ? iterationCount : 0,
        type: 'observation', // linear
        transformation: 'linear' // backward propagation
      };
      
      if (observationData && typeof observationData === 'object' && !Array.isArray(observationData) && this.io) {
        this.io.emit('observation', observationData);
      }

    } catch (error) {
      console.error(`❌ Failed to execute observation ${observation}:`, error);
      let errorMessage: string;
      try {
        errorMessage = error instanceof Error ? error.message : String(error);
      } catch (e) {
        errorMessage = 'Unknown error occurred';
      }
      if (this.io) {
        this.io.emit('error', { observation: observation, error: errorMessage });
      }
    }
  }

  /**
   * Progress to next dimension
   */
  private progressDimension(): void {
    const currentDim = (this.state.automaton as any).currentDimension;
    const nextDim = (currentDim + 1) % 8;
    (this.state.automaton as any).currentDimension = nextDim;
    
    if (this.io) {
      this.io.emit('dimension', { dimension: nextDim });
    }
  }

  /**
   * Get smart action based on current state
   */
  private getSmartAction(currentDimension: number, iterationCount: number): string {
    // Intelligent action selection based on context
    if (iterationCount % 20 === 0) {
      return 'self-modify';
    }
    
    if (iterationCount % 15 === 0) {
      return 'self-io';
    }
    
    if (iterationCount % 10 === 0) {
      return 'validate-self';
    }
    
    if (iterationCount % 8 === 0) {
      return 'self-train';
    }

    // Dimension-specific actions
    switch (currentDimension) {
      case 0:
        return Math.random() > 0.7 ? 'self-reference' : 'evolve';
      case 2:
        return Math.random() > 0.6 ? 'self-modify' : 'evolve';
      case 4:
        return Math.random() > 0.5 ? 'self-io' : 'evolve';
      case 6:
        return Math.random() > 0.4 ? 'self-train' : 'evolve';
      case 7:
        return Math.random() > 0.3 ? 'self-observe' : 'evolve';
      default:
        return 'evolve';
    }
  }

  /**
   * Get current state
   */
  getState(): AutomatonState {
    return { ...this.state };
  }

  /**
   * Get automaton instance
   */
  getAutomaton(): AdvancedSelfReferencingAutomaton {
    return this.state.automaton;
  }

  /**
   * Reset automaton
   */
  reset(filePath: string = './automaton.jsonl'): void {
    this.stop();
    try {
      this.state.automaton = new AdvancedSelfReferencingAutomaton(filePath);
      console.log('✅ Automaton reset successfully');
    } catch (error) {
      console.error('❌ Failed to reset automaton:', error);
      throw error;
    }
  }
}
