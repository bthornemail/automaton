/**
 * A₀: Genesis Automaton
 * 
 * Role: C₀ keyword generation (JSON Canvas format)
 * Uses meta-log-db: r5rs:create-cell, r5rs:export-0d
 */

import { BaseAutomaton } from './types.js';
import type { AutomatonId, SwarmContext, AutomatonMessage } from './types.js';
import type { MetaLogDb } from 'meta-log-db';

export interface A0GenesisState {
  genesis: any | null;
  keywords: string[];
  initialized: boolean;
}

/**
 * A₀: Genesis Automaton
 * 
 * Creates the genesis MetaLogNode and manages C₀ keywords (0-cells)
 */
export class A0_Genesis extends BaseAutomaton {
  readonly id: 0 = 0;
  readonly name = 'A₀ Genesis';
  readonly role = 'C₀ keyword generation (JSON Canvas format)';

  state: A0GenesisState = {
    genesis: null,
    keywords: [],
    initialized: false
  };

  constructor(private db?: MetaLogDb) {
    super();
  }

  async tick(swarm: SwarmContext): Promise<void> {
    if (!this.state.initialized) {
      await this.initialize();
    }

    // Genesis is stable - no ongoing tick operations needed
  }

  private async initialize(): Promise<void> {
    if (!this.db) {
      console.warn('A₀: MetaLogDb not provided, using mock genesis');
      this.state.genesis = {
        parent: 'genesis',
        cid: 'genesis',
        uri: 'canvasl://genesis',
        keywords: []
      };
      this.state.initialized = true;
      return;
    }

    try {
      // Create genesis node using meta-log-db
      // Create C₀ cell (keyword/point)
      const genesisCell = await this.db.executeR5RS('r5rs:create-cell', [
        0, // dimension
        'genesis',
        [], // no boundary (0-cell)
        { type: 'genesis', label: 'Genesis' }
      ]);

      // Build chain complex with just genesis
      const complex = await this.db.executeR5RS('r5rs:build-chain-complex', [[genesisCell]]);

      // Export to 0D (JSON Canvas)
      const jsonCanvas = await this.db.executeR5RS('r5rs:export-0d', [complex]);
      const canvas = JSON.parse(jsonCanvas);

      this.state.genesis = {
        parent: 'genesis',
        cid: 'genesis',
        uri: 'canvasl://genesis',
        keywords: canvas.nodes.map((n: any) => n.id),
        canvas
      };

      this.state.keywords = canvas.nodes.map((n: any) => n.id);
      this.state.initialized = true;

      console.log('A₀: Genesis node created with', this.state.keywords.length, 'keywords');
    } catch (error) {
      console.error('A₀: Error initializing genesis:', error);
      throw error;
    }
  }

  async receive(from: AutomatonId, message: AutomatonMessage): Promise<void> {
    // Genesis doesn't typically receive messages
    // But could receive requests for genesis data
    if (message.type === 'request-genesis') {
      // Could send genesis data back
    }
  }

  /**
   * Get genesis node
   */
  getGenesis(): any {
    return this.state.genesis;
  }

  /**
   * Get keywords (C₀ cells)
   */
  getKeywords(): string[] {
    return this.state.keywords;
  }
}

