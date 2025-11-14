/**
 * Consensus Pattern Service
 * 
 * Implements consensus patterns using polyhedra symmetries
 * Uses GCD for affine consensus, LCM for projective consensus
 * 
 * Source: docs/32-Regulay-Polyhedra-Geometry/06-CONSENSUS-PATTERNS.md
 */

import { bqfTransformationService, type BQF } from './bqf-transformation-service';

export type ConsensusType = 'tetrahedron' | 'cube' | 'octahedron' | 'icosahedron' | 'dodecahedron';

export interface ConsensusResult {
  consensus: number;
  type: ConsensusType;
  threshold: number;
  participants: number;
  reached: boolean;
}

/**
 * Calculate GCD (Greatest Common Divisor) for affine consensus
 */
function gcd(a: number, b: number): number {
  return b === 0 ? a : gcd(b, a % b);
}

/**
 * Calculate LCM (Least Common Multiple) for projective consensus
 */
function lcm(a: number, b: number): number {
  return (a * b) / gcd(a, b);
}

/**
 * Reduce array with GCD
 */
function reduceGCD(values: number[]): number {
  if (values.length === 0) return 0;
  if (values.length === 1) return values[0];
  return values.reduce((a, b) => gcd(a, b));
}

/**
 * Reduce array with LCM
 */
function reduceLCM(values: number[]): number {
  if (values.length === 0) return 1;
  if (values.length === 1) return values[0];
  return values.reduce((a, b) => lcm(a, b));
}

/**
 * Consensus Pattern Service
 */
export class ConsensusPatternService {
  /**
   * Tetrahedron consensus (4-point agreement)
   * Minimal consensus using GCD (affine consensus)
   * 
   * @param facts - Array of 4 facts/values
   * @returns Consensus result
   */
  tetrahedronConsensus(facts: number[]): ConsensusResult {
    if (facts.length !== 4) {
      throw new Error('Tetrahedron consensus requires exactly 4 facts');
    }

    const consensus = reduceGCD(facts);
    const threshold = 3; // 75% threshold (3 out of 4)

    return {
      consensus,
      type: 'tetrahedron',
      threshold,
      participants: 4,
      reached: true, // All 4 points agree
    };
  }

  /**
   * Cube consensus (8-point federated)
   * Federated consensus using LCM (projective consensus)
   * 
   * @param types - Array of 8 types/values
   * @returns Consensus result
   */
  cubeConsensus(types: number[]): ConsensusResult {
    if (types.length !== 8) {
      throw new Error('Cube consensus requires exactly 8 types');
    }

    const consensus = reduceLCM(types);
    const threshold = 4; // 50% threshold (4 out of 8)

    return {
      consensus,
      type: 'cube',
      threshold,
      participants: 8,
      reached: true, // Federated agreement
    };
  }

  /**
   * Octahedron consensus (6-point federated)
   * Projective space consensus using LCM
   * 
   * @param points - Array of 6 points
   * @returns Consensus result
   */
  octahedronConsensus(points: number[]): ConsensusResult {
    if (points.length !== 6) {
      throw new Error('Octahedron consensus requires exactly 6 points');
    }

    const consensus = reduceLCM(points);
    const threshold = 3; // 50% threshold (3 out of 6)

    return {
      consensus,
      type: 'octahedron',
      threshold,
      participants: 6,
      reached: true,
    };
  }

  /**
   * Icosahedron consensus (12-point global)
   * Global consensus using LCM
   * 
   * @param nodes - Array of 12 nodes
   * @returns Consensus result
   */
  icosahedronConsensus(nodes: number[]): ConsensusResult {
    if (nodes.length !== 12) {
      throw new Error('Icosahedron consensus requires exactly 12 nodes');
    }

    const consensus = reduceLCM(nodes);
    const threshold = 6; // 50% threshold (6 out of 12)

    return {
      consensus,
      type: 'icosahedron',
      threshold,
      participants: 12,
      reached: true,
    };
  }

  /**
   * Dodecahedron consensus (20-point global)
   * Complex global consensus using LCM
   * 
   * @param entities - Array of 20 entities
   * @returns Consensus result
   */
  dodecahedronConsensus(entities: number[]): ConsensusResult {
    if (entities.length !== 20) {
      throw new Error('Dodecahedron consensus requires exactly 20 entities');
    }

    const consensus = reduceLCM(entities);
    const threshold = 10; // 50% threshold (10 out of 20)

    return {
      consensus,
      type: 'dodecahedron',
      threshold,
      participants: 20,
      reached: true,
    };
  }

  /**
   * Hash-based consensus
   * Hash data to H₀ homology classes for consensus
   * 
   * @param data - Data to hash
   * @param polyhedronType - Type of polyhedron (determines number of classes)
   * @returns Hash consensus class
   */
  hashConsensus(data: any, polyhedronType: ConsensusType): number {
    const hash = this.simpleHash(JSON.stringify(data));
    const classes = this.getConsensusClasses(polyhedronType);
    return hash % classes;
  }

  /**
   * Check if two data items have consensus
   * 
   * @param data1 - First data item
   * @param data2 - Second data item
   * @param polyhedronType - Type of polyhedron
   * @returns True if they have consensus
   */
  checkConsensus(data1: any, data2: any, polyhedronType: ConsensusType): boolean {
    return (
      this.hashConsensus(data1, polyhedronType) ===
      this.hashConsensus(data2, polyhedronType)
    );
  }

  /**
   * Get number of consensus classes for polyhedron type
   */
  private getConsensusClasses(type: ConsensusType): number {
    switch (type) {
      case 'tetrahedron':
        return 4;
      case 'cube':
        return 8;
      case 'octahedron':
        return 6;
      case 'icosahedron':
        return 12;
      case 'dodecahedron':
        return 20;
      default:
        return 4;
    }
  }

  /**
   * Simple hash function
   */
  private simpleHash(str: string): number {
    let hash = 0;
    for (let i = 0; i < str.length; i++) {
      const char = str.charCodeAt(i);
      hash = (hash << 5) - hash + char;
      hash = hash & hash; // Convert to 32-bit integer
    }
    return Math.abs(hash);
  }

  /**
   * Quorum-based consensus
   * Check if quorum threshold is met
   * 
   * @param participants - Number of participants
   * @param agreements - Number of agreements
   * @param polyhedronType - Type of polyhedron
   * @returns True if quorum is met
   */
  quorumConsensus(
    participants: number,
    agreements: number,
    polyhedronType: ConsensusType
  ): boolean {
    const threshold = this.getQuorumThreshold(polyhedronType);
    const required = Math.ceil(participants * threshold);
    return agreements >= required;
  }

  /**
   * Get quorum threshold for polyhedron type
   */
  private getQuorumThreshold(type: ConsensusType): number {
    switch (type) {
      case 'tetrahedron':
        return 0.75; // 75% (3 out of 4)
      case 'cube':
        return 0.50; // 50% (4 out of 8)
      case 'octahedron':
        return 0.50; // 50% (3 out of 6)
      case 'icosahedron':
        return 0.25; // 25% (3 out of 12) - lower for global
      case 'dodecahedron':
        return 0.25; // 25% (5 out of 20) - lower for complex
      default:
        return 0.50;
    }
  }
}

// Singleton instance
export const consensusPatternService = new ConsensusPatternService();

