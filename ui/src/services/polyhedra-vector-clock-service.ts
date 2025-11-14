/**
 * Polyhedra Vector Clock Service
 * 
 * Integrates vector clocks with polyhedra metadata for causal ordering
 * Tracks causal history using geometric structures
 * 
 * Source: docs/32-Regulay-Polyhedra-Geometry/04-COMPUTATIONAL-MAPPING.md
 */

import { VectorClockService } from './vector-clock-service';
import type { VectorClock } from '../types/vector-clock';
import type { BQF } from './bqf-transformation-service';

export interface PolyhedraVectorClock {
  vectorClock: VectorClock;
  polyhedronType: 'tetrahedron' | 'cube' | 'octahedron' | 'icosahedron' | 'dodecahedron';
  bqf: BQF;
  file: string;
  line: number;
  timestamp: number;
  pattern: string;
}

/**
 * Polyhedra Vector Clock Service
 * 
 * Extends vector clock service with polyhedra-specific operations
 */
export class PolyhedraVectorClockService {
  private vectorClockService: VectorClockService;

  constructor() {
    this.vectorClockService = new VectorClockService();
  }

  /**
   * Create vector clock from polyhedron metadata
   * 
   * @param file - File name
   * @param line - Line number
   * @param timestamp - Timestamp
   * @param pattern - Pattern name (e.g., 'cube-consensus')
   * @param polyhedronType - Type of polyhedron
   * @param bqf - BQF encoding
   * @returns Polyhedra vector clock
   */
  create(
    file: string,
    line: number,
    timestamp: number,
    pattern: string,
    polyhedronType: PolyhedraVectorClock['polyhedronType'],
    bqf: BQF
  ): PolyhedraVectorClock {
    const vectorClock: VectorClock = [
      file,
      line,
      timestamp,
      pattern,
      ...bqf, // Add BQF as additional components
    ];

    return {
      vectorClock,
      polyhedronType,
      bqf,
      file,
      line,
      timestamp,
      pattern,
    };
  }

  /**
   * Merge vector clocks for dual pairs
   * 
   * @param vc1 - First polyhedra vector clock
   * @param vc2 - Second polyhedra vector clock
   * @returns Merged vector clock
   */
  mergeDualPair(
    vc1: PolyhedraVectorClock,
    vc2: PolyhedraVectorClock
  ): PolyhedraVectorClock {
    const merged = this.vectorClockService.merge(vc1.vectorClock, vc2.vectorClock);

    // Determine merged polyhedron type (use the more complex one)
    const mergedType = this.getMoreComplexType(vc1.polyhedronType, vc2.polyhedronType);
    
    // Merge BQFs (component-wise maximum)
    const mergedBQF: BQF = [
      Math.max(vc1.bqf[0], vc2.bqf[0]),
      Math.max(vc1.bqf[1], vc2.bqf[1]),
      Math.max(vc1.bqf[2], vc2.bqf[2]),
    ];

    return {
      vectorClock: merged.merged,
      polyhedronType: mergedType,
      bqf: mergedBQF,
      file: vc1.file, // Use first file
      line: Math.max(vc1.line, vc2.line),
      timestamp: Math.max(vc1.timestamp, vc2.timestamp),
      pattern: `${vc1.pattern}-${vc2.pattern}`,
    };
  }

  /**
   * Check causal ordering for polyhedra
   * 
   * @param vc1 - First polyhedra vector clock
   * @param vc2 - Second polyhedra vector clock
   * @returns True if vc1 happens before vc2
   */
  happensBefore(vc1: PolyhedraVectorClock, vc2: PolyhedraVectorClock): boolean {
    return this.vectorClockService.happensBefore(vc1.vectorClock, vc2.vectorClock);
  }

  /**
   * Get geometric causality level
   * 
   * @param polyhedronType - Type of polyhedron
   * @returns Number of causal components
   */
  getCausalityLevel(polyhedronType: PolyhedraVectorClock['polyhedronType']): number {
    switch (polyhedronType) {
      case 'tetrahedron':
        return 4; // Minimal causality (4 components)
      case 'cube':
      case 'octahedron':
        return 8; // Federated causality (8/6 components)
      case 'icosahedron':
      case 'dodecahedron':
        return 12; // Global causality (12/20 components)
      default:
        return 4;
    }
  }

  /**
   * Create vector clock for cube consensus
   * 
   * @param file - File name
   * @param line - Line number
   * @param timestamp - Timestamp
   * @returns Cube polyhedra vector clock
   */
  createCubeConsensus(
    file: string,
    line: number,
    timestamp: number
  ): PolyhedraVectorClock {
    return this.create(
      file,
      line,
      timestamp,
      'cube-consensus',
      'cube',
      [8, 12, 6]
    );
  }

  /**
   * Create vector clock for octahedron consensus
   * 
   * @param file - File name
   * @param line - Line number
   * @param timestamp - Timestamp
   * @returns Octahedron polyhedra vector clock
   */
  createOctahedronConsensus(
    file: string,
    line: number,
    timestamp: number
  ): PolyhedraVectorClock {
    return this.create(
      file,
      line,
      timestamp,
      'octa-consensus',
      'octahedron',
      [6, 12, 8]
    );
  }

  /**
   * Create vector clock for tetrahedron consensus
   * 
   * @param file - File name
   * @param line - Line number
   * @param timestamp - Timestamp
   * @returns Tetrahedron polyhedra vector clock
   */
  createTetrahedronConsensus(
    file: string,
    line: number,
    timestamp: number
  ): PolyhedraVectorClock {
    return this.create(
      file,
      line,
      timestamp,
      'tetra-consensus',
      'tetrahedron',
      [4, 6, 4]
    );
  }

  /**
   * Get more complex polyhedron type
   */
  private getMoreComplexType(
    type1: PolyhedraVectorClock['polyhedronType'],
    type2: PolyhedraVectorClock['polyhedronType']
  ): PolyhedraVectorClock['polyhedronType'] {
    const complexity: Record<PolyhedraVectorClock['polyhedronType'], number> = {
      tetrahedron: 1,
      cube: 2,
      octahedron: 2,
      icosahedron: 3,
      dodecahedron: 4,
    };

    return complexity[type1] >= complexity[type2] ? type1 : type2;
  }

  /**
   * Extract BQF from vector clock
   * 
   * @param vc - Polyhedra vector clock
   * @returns BQF encoding
   */
  extractBQF(vc: PolyhedraVectorClock): BQF {
    return vc.bqf;
  }

  /**
   * Update BQF in vector clock
   * 
   * @param vc - Polyhedra vector clock
   * @param bqf - New BQF
   * @returns Updated polyhedra vector clock
   */
  updateBQF(vc: PolyhedraVectorClock, bqf: BQF): PolyhedraVectorClock {
    const updatedVectorClock: VectorClock = [
      vc.file,
      vc.line,
      vc.timestamp,
      vc.pattern,
      ...bqf,
    ];

    return {
      ...vc,
      vectorClock: updatedVectorClock,
      bqf,
    };
  }
}

// Singleton instance
export const polyhedraVectorClockService = new PolyhedraVectorClockService();

