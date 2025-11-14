/**
 * Constraint Pointer Service
 * 
 * Implements constraint pointers via geometric asymmetries in dual pairs
 * Creates directional constraints from affine to projective space
 * 
 * Source: docs/32-Regulay-Polyhedra-Geometry/07-CONSTRAINT-POINTERS.md
 */

import { bqfTransformationService, type BQF } from './bqf-transformation-service';
import * as THREE from 'three';

export type DualPairType = 'cube-octahedron' | 'icosahedron-dodecahedron' | 'tetrahedron-self';

export interface ConstraintPointer {
  from: BQF;
  to: BQF;
  direction: THREE.Vector3;
  type: DualPairType;
  strength: number; // 0-1, based on asymmetry
}

/**
 * Constraint Pointer Service
 */
export class ConstraintPointerService {
  /**
   * Create constraint pointer from dual pair
   * 
   * @param fromBQF - Source BQF (e.g., Cube [8,12,6])
   * @param toBQF - Target BQF (e.g., Octahedron [6,12,8])
   * @returns Constraint pointer with direction
   */
  createConstraintPointer(fromBQF: BQF, toBQF: BQF): ConstraintPointer {
    // Calculate direction vector based on BQF difference
    const [a1, b1, c1] = fromBQF;
    const [a2, b2, c2] = toBQF;
    
    // Direction: from affine (a) to projective (c)
    const direction = new THREE.Vector3(
      a2 - a1,  // Affine component change
      b2 - b1,  // Interaction component change
      c2 - c1   // Projective component change
    ).normalize();
    
    // Calculate asymmetry strength (difference in a and c)
    const asymmetry = Math.abs(a1 - c1) + Math.abs(a2 - c2);
    const strength = Math.min(asymmetry / 20, 1.0); // Normalize to 0-1
    
    // Determine dual pair type
    const type = this.detectDualPairType(fromBQF, toBQF);
    
    return {
      from: fromBQF,
      to: toBQF,
      direction,
      type,
      strength,
    };
  }

  /**
   * Create constraint pointer via dual swap
   * 
   * @param bqf - Source BQF
   * @returns Constraint pointer to dual
   */
  createDualSwapConstraint(bqf: BQF): ConstraintPointer {
    const dualBQF = bqfTransformationService.dualSwap(bqf);
    return this.createConstraintPointer(bqf, dualBQF);
  }

  /**
   * Cube → Octahedron constraint
   * Affine to projective constraint
   * 
   * @param cubeBQF - Cube BQF [8,12,6]
   * @returns Constraint pointer
   */
  cubeToOctahedronConstraint(cubeBQF: BQF = [8, 12, 6]): ConstraintPointer {
    const octaBQF: BQF = [6, 12, 8];
    return this.createConstraintPointer(cubeBQF, octaBQF);
  }

  /**
   * Icosahedron → Dodecahedron constraint
   * Exponential constraint fan-out
   * 
   * @param icosaBQF - Icosahedron BQF [12,30,20]
   * @returns Constraint pointer
   */
  icosahedronToDodecahedronConstraint(icosaBQF: BQF = [12, 30, 20]): ConstraintPointer {
    const dodecaBQF: BQF = [20, 30, 12];
    return this.createConstraintPointer(icosaBQF, dodecaBQF);
  }

  /**
   * Apply constraint (forward transformation)
   * 
   * @param constraint - Constraint pointer
   * @returns Transformed BQF
   */
  applyConstraint(constraint: ConstraintPointer): BQF {
    return bqfTransformationService.apply(constraint.to);
  }

  /**
   * Abstract constraint (backward transformation)
   * 
   * @param constraint - Constraint pointer
   * @returns Abstracted BQF
   */
  abstractConstraint(constraint: ConstraintPointer): BQF {
    return bqfTransformationService.abstract(constraint.from);
  }

  /**
   * Get constraint direction as 3D vector
   * 
   * @param constraint - Constraint pointer
   * @returns Direction vector
   */
  getDirection(constraint: ConstraintPointer): THREE.Vector3 {
    return constraint.direction.clone();
  }

  /**
   * Check if constraint is valid (has asymmetry)
   * 
   * @param constraint - Constraint pointer
   * @returns True if constraint has asymmetry
   */
  isValidConstraint(constraint: ConstraintPointer): boolean {
    return constraint.strength > 0;
  }

  /**
   * Detect dual pair type from BQFs
   */
  private detectDualPairType(fromBQF: BQF, toBQF: BQF): DualPairType {
    const [a1, , c1] = fromBQF;
    const [a2, , c2] = toBQF;
    
    // Check if it's a cube-octahedron pair
    if ((a1 === 8 && c1 === 6 && a2 === 6 && c2 === 8) ||
        (a1 === 6 && c1 === 8 && a2 === 8 && c2 === 6)) {
      return 'cube-octahedron';
    }
    
    // Check if it's an icosahedron-dodecahedron pair
    if ((a1 === 12 && c1 === 20 && a2 === 20 && c2 === 12) ||
        (a1 === 20 && c1 === 12 && a2 === 12 && c2 === 20)) {
      return 'icosahedron-dodecahedron';
    }
    
    // Check if it's self-dual (tetrahedron)
    if (a1 === c1 && a2 === c2 && a1 === a2) {
      return 'tetrahedron-self';
    }
    
    return 'cube-octahedron'; // Default
  }

  /**
   * Create constraint pointer from action
   * Forward propagation (affine → projective)
   * 
   * @param action - Action BQF
   * @returns Constraint pointer
   */
  createActionConstraint(action: BQF): ConstraintPointer {
    const applied = bqfTransformationService.apply(action);
    return this.createConstraintPointer(action, applied);
  }

  /**
   * Create constraint pointer from observation
   * Backward propagation (projective → affine)
   * 
   * @param observation - Observation BQF
   * @returns Constraint pointer
   */
  createObservationConstraint(observation: BQF): ConstraintPointer {
    const abstracted = bqfTransformationService.abstract(observation);
    return this.createConstraintPointer(observation, abstracted);
  }

  /**
   * Compose multiple constraint pointers
   * 
   * @param constraints - Array of constraint pointers
   * @returns Composed constraint pointer
   */
  composeConstraints(constraints: ConstraintPointer[]): ConstraintPointer {
    if (constraints.length === 0) {
      throw new Error('Cannot compose empty constraint array');
    }
    
    if (constraints.length === 1) {
      return constraints[0];
    }
    
    // Compose BQFs
    let currentBQF = constraints[0].from;
    for (const constraint of constraints) {
      currentBQF = bqfTransformationService.compose(currentBQF, constraint.to);
    }
    
    const finalBQF = constraints[constraints.length - 1].to;
    return this.createConstraintPointer(constraints[0].from, finalBQF);
  }

  /**
   * Get constraint strength (asymmetry measure)
   * 
   * @param constraint - Constraint pointer
   * @returns Strength value (0-1)
   */
  getStrength(constraint: ConstraintPointer): number {
    return constraint.strength;
  }
}

// Singleton instance
export const constraintPointerService = new ConstraintPointerService();

