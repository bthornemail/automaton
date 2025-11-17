/**
 * A₁: Edge Weaver Automaton
 * 
 * Role: C₁ edge/connection management (JSONL format)
 * Uses meta-log-db: r5rs:create-cell, r5rs:export-1d, r5rs:build-chain-complex
 */

import { BaseAutomaton } from './types.js';
import type { AutomatonId, SwarmContext, AutomatonMessage } from './types.js';
import type { MetaLogDb } from 'meta-log-db';

export interface A1EdgeWeaverState {
  edges: Array<{ id: string; from: string; to: string; data?: any }>;
  initialized: boolean;
}

/**
 * A₁: Edge Weaver Automaton
 * 
 * Manages C₁ edges/connections (1-cells) and weaves edges between keywords
 */
export class A1_EdgeWeaver extends BaseAutomaton {
  readonly id: 1 = 1;
  readonly name = 'A₁ Edge Weaver';
  readonly role = 'C₁ edge/connection management (JSONL format)';

  state: A1EdgeWeaverState = {
    edges: [],
    initialized: false
  };

  constructor(private db?: MetaLogDb) {
    super();
  }

  async tick(swarm: SwarmContext): Promise<void> {
    if (!this.state.initialized) {
      await this.initialize(swarm);
    }

    // Edge weaving happens based on keywords from A₀
    const a0 = swarm.get(0);
    if (a0 && 'getKeywords' in a0 && typeof a0.getKeywords === 'function') {
      const keywords = (a0 as any).getKeywords();
      await this.weaveEdges(keywords);
    }
  }

  private async initialize(swarm: SwarmContext): Promise<void> {
    this.state.initialized = true;
    console.log('A₁: Edge Weaver initialized');
  }

  private async weaveEdges(keywords: string[]): Promise<void> {
    if (!this.db || keywords.length < 2) {
      return;
    }

    try {
      // Create edges between adjacent keywords
      const newEdges: Array<{ id: string; from: string; to: string }> = [];
      
      for (let i = 0; i < keywords.length - 1; i++) {
        const from = keywords[i];
        const to = keywords[i + 1];
        const edgeId = `edge-${from}-${to}`;

        // Check if edge already exists
        if (!this.state.edges.find(e => e.id === edgeId)) {
          newEdges.push({ id: edgeId, from, to });
        }
      }

      if (newEdges.length === 0) {
        return;
      }

      // Create C₁ cells (edges) using meta-log-db
      const cells = await Promise.all(
        newEdges.map(edge =>
          this.db!.executeR5RS('r5rs:create-cell', [
            1, // dimension (1-cell)
            edge.id,
            [edge.from, edge.to], // boundary: from and to keywords
            { from: edge.from, to: edge.to }
          ])
        )
      );

      // Build chain complex with edges
      const complex = await this.db.executeR5RS('r5rs:build-chain-complex', [cells]);

      // Export to 1D (JSONL)
      const jsonl = await this.db.executeR5RS('r5rs:export-1d', [complex]);

      // Parse JSONL and update state
      const lines = jsonl.split('\n').filter((l: string) => l.trim());
      const parsedEdges = lines.map((line: string) => JSON.parse(line));

      this.state.edges.push(...parsedEdges.map((e: any) => ({
        id: e.id,
        from: e.from,
        to: e.to,
        data: e
      })));

      console.log(`A₁: Wove ${newEdges.length} new edges`);
    } catch (error) {
      console.error('A₁: Error weaving edges:', error);
    }
  }

  async receive(from: AutomatonId, message: AutomatonMessage): Promise<void> {
    if (message.type === 'request-edges') {
      // Could send edges back to requester
    }
  }

  /**
   * Get all edges
   */
  getEdges(): Array<{ id: string; from: string; to: string; data?: any }> {
    return this.state.edges;
  }
}

