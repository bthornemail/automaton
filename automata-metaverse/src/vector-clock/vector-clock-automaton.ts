/**
 * Vector Clock Automaton Base
 * 
 * Base class for automata with vector clock state tracking
 */

import { VectorClock } from './vector-clock.js';

/**
 * Meta-Log interface for vector clock automata
 */
export interface MetaLog {
  datalog?: {
    addFacts?: (facts: Array<{ predicate: string; args: any[] }>) => void;
  };
}

/**
 * Automaton state interface
 */
export interface AutomatonState {
  id: string | number;
  dimension: number;
  running: boolean;
  vectorClock: Map<string | number, number>;
  cellCounts: {
    C0: number;
    C1: number;
    C2: number;
    C3: number;
    C4: number;
  };
}

/**
 * Message interface
 */
export interface AutomatonMessage {
  type?: string;
  vectorClock?: Map<string | number, number> | Record<string | number, number>;
  from?: string | number;
  timestamp?: number;
  [key: string]: any;
}

/**
 * Swarm context interface
 */
export interface SwarmContext {
  [key: string]: any;
}

/**
 * Base Vector Clock Automaton
 * 
 * Provides vector clock state tracking for distributed causality
 */
export class VectorClockAutomaton {
  protected id: string | number;
  protected metaLog: MetaLog | null;
  protected vectorClock: VectorClock;
  protected state: AutomatonState;

  constructor(id: string | number, metaLog: MetaLog | null = null) {
    this.id = id;
    this.metaLog = metaLog;
    this.vectorClock = new VectorClock(id);
    this.state = {
      id,
      dimension: 0,
      running: false,
      vectorClock: this.vectorClock.toMap(),
      cellCounts: {
        C0: 0,
        C1: 0,
        C2: 0,
        C3: 0,
        C4: 0
      }
    };
  }

  /**
   * Tick automaton (increment own vector clock)
   * 
   * @param {SwarmContext | null} swarm - Swarm context (optional)
   * @returns {Promise<void>}
   */
  async tick(swarm: SwarmContext | null = null): Promise<void> {
    // Increment own tick
    this.vectorClock.tick();
    
    // Update state
    this.state.vectorClock = this.vectorClock.toMap();
    
    // Store tick in Meta-Log
    if (this.metaLog) {
      await this.storeTickInMetaLog();
    }
    
    // Execute automaton-specific tick logic
    await this.executeTick(swarm);
  }

  /**
   * Receive message from another automaton
   * 
   * @param {string | number} from - Sender automaton ID
   * @param {AutomatonMessage} message - Message object
   * @returns {Promise<void>}
   */
  async receive(from: string | number, message: AutomatonMessage): Promise<void> {
    // Merge vector clock from message
    if (message.vectorClock) {
      const otherClock = message.vectorClock instanceof Map 
        ? message.vectorClock 
        : new Map(Object.entries(message.vectorClock)) as Map<string | number, number>;
      
      this.vectorClock = this.vectorClock.merge(otherClock);
      this.state.vectorClock = this.vectorClock.toMap();
    }
    
    // Store received message in Meta-Log
    if (this.metaLog) {
      await this.storeReceiveInMetaLog(from, message);
    }
    
    // Execute automaton-specific receive logic
    await this.executeReceive(from, message);
  }

  /**
   * Send message to another automaton
   * 
   * @param {string | number} to - Recipient automaton ID
   * @param {AutomatonMessage} message - Message object
   * @returns {Promise<void>}
   */
  async send(to: string | number, message: AutomatonMessage): Promise<void> {
    // Include current vector clock in message
    const messageWithClock: AutomatonMessage = {
      ...message,
      vectorClock: this.vectorClock.toMap(),
      from: this.id,
      timestamp: Date.now() // Optional metadata
    };
    
    // Store sent message in Meta-Log
    if (this.metaLog) {
      await this.storeSendInMetaLog(to, messageWithClock);
    }
    
    // Execute automaton-specific send logic
    await this.executeSend(to, messageWithClock);
  }

  /**
   * Get current state
   * 
   * @returns {AutomatonState} Current automaton state
   */
  getState(): AutomatonState {
    return {
      ...this.state,
      vectorClock: this.vectorClock.toMap()
    };
  }

  /**
   * Check if this automaton happens before another
   * 
   * @param {string | number} otherId - Other automaton ID
   * @param {Map<string | number, number>} otherClock - Other automaton's vector clock
   * @returns {boolean} True if this happens before other
   */
  happensBefore(otherId: string | number, otherClock: Map<string | number, number>): boolean {
    return this.vectorClock.happensBefore(otherClock);
  }

  /**
   * Check if this automaton is concurrent with another
   * 
   * @param {string | number} otherId - Other automaton ID
   * @param {Map<string | number, number>} otherClock - Other automaton's vector clock
   * @returns {boolean} True if concurrent
   */
  isConcurrent(otherId: string | number, otherClock: Map<string | number, number>): boolean {
    return this.vectorClock.isConcurrent(otherClock);
  }

  /**
   * Get causal chain
   * 
   * @returns {Array<{automatonId: string | number, tick: number}>} Causal chain
   */
  getCausalChain(): Array<{automatonId: string | number; tick: number}> {
    return this.vectorClock.getCausalChain();
  }

  /**
   * Store tick in Meta-Log
   * 
   * @returns {Promise<void>}
   */
  async storeTickInMetaLog() {
    if (!this.metaLog || !this.metaLog.datalog) return;

    try {
      const tick = this.vectorClock.getTick();
      const fact = {
        predicate: 'automaton_tick',
        args: [this.id, this.id, tick]
      };

      if (this.metaLog.datalog.addFacts) {
        this.metaLog.datalog.addFacts([fact]);
      }
    } catch (error) {
      console.warn(`[Automaton ${this.id}] Failed to store tick in Meta-Log:`, error);
    }
  }

  /**
   * Store receive event in Meta-Log
   * 
   * @param {string | number} from - Sender automaton ID
   * @param {AutomatonMessage} message - Received message
   * @returns {Promise<void>}
   */
  async storeReceiveInMetaLog(from: string | number, message: AutomatonMessage): Promise<void> {
    if (!this.metaLog || !this.metaLog.datalog) return;

    try {
      // Store vector clock ticks from received message
      if (message.vectorClock) {
        const clockMap = message.vectorClock instanceof Map 
          ? message.vectorClock 
          : new Map(Object.entries(message.vectorClock)) as Map<string | number, number>;
        const clockFacts = Array.from(clockMap.entries()).map(([peer, tick]) => ({
          predicate: 'automaton_tick',
          args: [this.id, peer, tick]
        }));

        if (this.metaLog.datalog.addFacts) {
          this.metaLog.datalog.addFacts(clockFacts);
        }
      }

      // Store receive event
      const receiveFact = {
        predicate: 'automaton_receive',
        args: [this.id, from, message.type || 'message']
      };

      if (this.metaLog.datalog.addFacts) {
        this.metaLog.datalog.addFacts([receiveFact]);
      }
    } catch (error) {
      console.warn(`[Automaton ${this.id}] Failed to store receive in Meta-Log:`, error);
    }
  }

  /**
   * Store send event in Meta-Log
   * 
   * @param {string | number} to - Recipient automaton ID
   * @param {AutomatonMessage} message - Sent message
   * @returns {Promise<void>}
   */
  async storeSendInMetaLog(to: string | number, message: AutomatonMessage): Promise<void> {
    if (!this.metaLog || !this.metaLog.datalog) return;

    try {
      const sendFact = {
        predicate: 'automaton_send',
        args: [this.id, to, message.type || 'message']
      };

      if (this.metaLog.datalog.addFacts) {
        this.metaLog.datalog.addFacts([sendFact]);
      }
    } catch (error) {
      console.warn(`[Automaton ${this.id}] Failed to store send in Meta-Log:`, error);
    }
  }

  /**
   * Execute automaton-specific tick logic
   * Override in subclasses
   * 
   * @param {SwarmContext | null} swarm - Swarm context
   * @returns {Promise<void>}
   */
  protected async executeTick(swarm: SwarmContext | null): Promise<void> {
    // Override in subclasses
  }

  /**
   * Execute automaton-specific receive logic
   * Override in subclasses
   * 
   * @param {string | number} from - Sender automaton ID
   * @param {AutomatonMessage} message - Received message
   * @returns {Promise<void>}
   */
  protected async executeReceive(from: string | number, message: AutomatonMessage): Promise<void> {
    // Override in subclasses
  }

  /**
   * Execute automaton-specific send logic
   * Override in subclasses
   * 
   * @param {string | number} to - Recipient automaton ID
   * @param {AutomatonMessage} message - Message to send
   * @returns {Promise<void>}
   */
  protected async executeSend(to: string | number, message: AutomatonMessage): Promise<void> {
    // Override in subclasses
  }
}

