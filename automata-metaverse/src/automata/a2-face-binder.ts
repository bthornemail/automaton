/**
 * A₂: Face Binder Automaton
 * 
 * Role: C₂ document/face management (GeoJSON format)
 * Uses meta-log-db: r5rs:export-2d
 */

import { BaseAutomaton } from './types.js';
import type { AutomatonId, SwarmContext, AutomatonMessage } from './types.js';
import type { MetaLogDb } from 'meta-log-db';

export interface A2FaceBinderState {
  faces: Array<{ id: string; edges: string[]; data?: any }>;
  initialized: boolean;
}

/**
 * A₂: Face Binder Automaton
 * 
 * Manages C₂ documents/faces (2-cells) and binds faces from edges
 */
export class A2_FaceBinder extends BaseAutomaton {
  readonly id: 2 = 2;
  readonly name = 'A₂ Face Binder';
  readonly role = 'C₂ document/face management (GeoJSON format)';

  state: A2FaceBinderState = {
    faces: [],
    initialized: false
  };

  constructor(private db?: MetaLogDb) {
    super();
  }

  async tick(swarm: SwarmContext): Promise<void> {
    if (!this.state.initialized) {
      await this.initialize(swarm);
    }

    // Face binding happens based on edges from A₁
    const a1 = swarm.get(1);
    if (a1 && 'getEdges' in a1 && typeof a1.getEdges === 'function') {
      const edges = (a1 as any).getEdges();
      await this.bindFaces(edges);
    }
  }

  private async initialize(swarm: SwarmContext): Promise<void> {
    this.state.initialized = true;
    console.log('A₂: Face Binder initialized');
  }

  private async bindFaces(edges: Array<{ id: string; from: string; to: string }>): Promise<void> {
    if (!this.db || edges.length < 3) {
      return;
    }

    try {
      // Create faces from closed loops of edges
      // Simple implementation: create a face from every 3+ edge cycle
      const newFaces: Array<{ id: string; edges: string[] }> = [];

      // Find cycles (simplified: every 3 consecutive edges form a face)
      for (let i = 0; i < edges.length - 2; i++) {
        const edge1 = edges[i];
        const edge2 = edges[i + 1];
        const edge3 = edges[i + 2];

        // Check if edges form a cycle (edge1.to === edge2.from, edge2.to === edge3.from, edge3.to === edge1.from)
        if (edge1.to === edge2.from && edge2.to === edge3.from && edge3.to === edge1.from) {
          const faceId = `face-${edge1.id}-${edge2.id}-${edge3.id}`;
          
          if (!this.state.faces.find(f => f.id === faceId)) {
            newFaces.push({
              id: faceId,
              edges: [edge1.id, edge2.id, edge3.id]
            });
          }
        }
      }

      if (newFaces.length === 0) {
        return;
      }

      // Create C₂ cells (faces) using meta-log-db
      const cells = await Promise.all(
        newFaces.map(face =>
          this.db!.executeR5RS('r5rs:create-cell', [
            2, // dimension (2-cell)
            face.id,
            face.edges, // boundary: edges that form the face
            { edges: face.edges, type: 'face' }
          ])
        )
      );

      // Build chain complex with faces
      const complex = await this.db.executeR5RS('r5rs:build-chain-complex', [cells]);

      // Export to 2D (GeoJSON)
      const geojson = await this.db.executeR5RS('r5rs:export-2d', [complex]);
      const geo = JSON.parse(geojson);

      // Update state
      this.state.faces.push(...newFaces.map((f, i) => ({
        id: f.id,
        edges: f.edges,
        data: geo.features[i] || {}
      })));

      console.log(`A₂: Bound ${newFaces.length} new faces`);
    } catch (error) {
      console.error('A₂: Error binding faces:', error);
    }
  }

  async receive(from: AutomatonId, message: AutomatonMessage): Promise<void> {
    if (message.type === 'request-faces') {
      // Could send faces back to requester
    }
  }

  /**
   * Get all faces
   */
  getFaces(): Array<{ id: string; edges: string[]; data?: any }> {
    return this.state.faces;
  }
}

