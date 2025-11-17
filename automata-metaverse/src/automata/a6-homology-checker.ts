/**
 * A₆: Homology Checker Automaton
 * 
 * Role: ∂² = 0 validation
 * Uses meta-log-db: HomologyValidator, r5rs:validate-homology
 */

import { BaseAutomaton } from './types.js';
import type { AutomatonId, SwarmContext, AutomatonMessage } from './types.js';
import type { MetaLogDb } from 'meta-log-db';
// @ts-ignore - extensions not yet exported in meta-log-db package.json
import { HomologyValidator } from 'meta-log-db/src/extensions/homology';
// @ts-ignore - extensions not yet exported in meta-log-db package.json
import type { ChainComplex } from 'meta-log-db/src/extensions/homology';

export interface A6HomologyCheckerState {
  lastCheck: number;
  violations: string[];
  validationCount: number;
  initialized: boolean;
}

/**
 * A₆: Homology Checker Automaton
 * 
 * Validates ∂² = 0 property and checks homology violations
 */
export class A6_HomologyChecker extends BaseAutomaton {
  readonly id: 6 = 6;
  readonly name = 'A₆ Homology Checker';
  readonly role = '∂² = 0 validation';

  state: A6HomologyCheckerState = {
    lastCheck: 0,
    violations: [],
    validationCount: 0,
    initialized: false
  };

  private checkInterval: number = 5000; // Check every 5 seconds

  constructor(private db?: MetaLogDb) {
    super();
  }

  async tick(swarm: SwarmContext): Promise<void> {
    if (!this.state.initialized) {
      await this.initialize(swarm);
    }

    const now = Date.now();
    if (now - this.state.lastCheck > this.checkInterval) {
      await this.validateSwarm(swarm);
      this.state.lastCheck = now;
    }
  }

  private async initialize(swarm: SwarmContext): Promise<void> {
    this.state.initialized = true;
    console.log('A₆: Homology Checker initialized');
  }

  private async validateSwarm(swarm: SwarmContext): Promise<void> {
    if (!this.db) {
      return;
    }

    try {
      // Build chain complex from automata states
      const complex = await this.buildChainComplex(swarm);

      // Validate homology using meta-log-db
      const validator = new HomologyValidator(complex);
      const result = validator.validate();

      this.state.validationCount++;

      if (!result.valid) {
        this.state.violations.push(...(result.violations || []));
        console.error('A₆: Homology violation detected!', result.violations);

        // Report to master (A₁₁)
        swarm.sendMessage(this.id, 11, {
          type: 'homology-error',
          payload: {
            violations: result.violations,
            betti: result.betti,
            eulerCharacteristic: result.eulerCharacteristic
          }
        });
      } else {
        console.log(`A₆: Homology check passed (Betti: [${result.betti.join(', ')}], χ: ${result.eulerCharacteristic})`);
      }
    } catch (error) {
      console.error('A₆: Error validating homology:', error);
    }
  }

  private async buildChainComplex(swarm: SwarmContext): Promise<ChainComplex> {
    // Collect cells from automata A₀-A₄
    const cells: any[] = [];

    // A₀: Keywords (C₀)
    const a0 = swarm.get(0);
    if (a0 && 'getKeywords' in a0 && typeof a0.getKeywords === 'function') {
      const keywords = (a0 as any).getKeywords();
      for (const keyword of keywords) {
        const cell = await this.db!.executeR5RS('r5rs:create-cell', [0, keyword, [], { label: keyword }]);
        cells.push(cell);
      }
    }

    // A₁: Edges (C₁)
    const a1 = swarm.get(1);
    if (a1 && 'getEdges' in a1 && typeof a1.getEdges === 'function') {
      const edges = (a1 as any).getEdges();
      for (const edge of edges) {
        const cell = await this.db!.executeR5RS('r5rs:create-cell', [1, edge.id, [edge.from, edge.to], edge.data]);
        cells.push(cell);
      }
    }

    // A₂: Faces (C₂)
    const a2 = swarm.get(2);
    if (a2 && 'getFaces' in a2 && typeof a2.getFaces === 'function') {
      const faces = (a2 as any).getFaces();
      for (const face of faces) {
        const cell = await this.db!.executeR5RS('r5rs:create-cell', [2, face.id, face.edges, face.data]);
        cells.push(cell);
      }
    }

    // A₃: Volumes (C₃)
    const a3 = swarm.get(3);
    if (a3 && 'getVolumes' in a3 && typeof a3.getVolumes === 'function') {
      const volumes = (a3 as any).getVolumes();
      for (const volume of volumes) {
        const cell = await this.db!.executeR5RS('r5rs:create-cell', [3, volume.id, volume.faces, volume.data]);
        cells.push(cell);
      }
    }

    // A₄: Contexts (C₄)
    const a4 = swarm.get(4);
    if (a4 && 'getContexts' in a4 && typeof a4.getContexts === 'function') {
      const contexts = (a4 as any).getContexts();
      for (const context of contexts) {
        const cell = await this.db!.executeR5RS('r5rs:create-cell', [4, context.id, context.volumes, context.data]);
        cells.push(cell);
      }
    }

    // Build chain complex
    const complex = await this.db!.executeR5RS('r5rs:build-chain-complex', [cells]);
    
    // Convert to ChainComplex type
    return complex as ChainComplex;
  }

  async receive(from: AutomatonId, message: AutomatonMessage): Promise<void> {
    if (message.type === 'request-validation') {
      // Could send validation results back
    }
  }

  /**
   * Get validation violations
   */
  getViolations(): string[] {
    return this.state.violations;
  }
}

