/**
 * Constraint Pointer Service Tests
 * 
 * Tests for constraint pointers via geometric asymmetries in dual pairs
 */

import { describe, it, expect } from 'vitest';
import { constraintPointerService, type BQF, type DualPairType } from '../constraint-pointer-service';
import * as THREE from 'three';

describe('ConstraintPointerService', () => {
  describe('createConstraintPointer', () => {
    it('should create constraint pointer from dual pair', () => {
      const fromBQF: BQF = [8, 12, 6];
      const toBQF: BQF = [6, 12, 8];
      const result = constraintPointerService.createConstraintPointer(fromBQF, toBQF);
      
      expect(result.from).toEqual([8, 12, 6]);
      expect(result.to).toEqual([6, 12, 8]);
      expect(result.direction).toBeInstanceOf(THREE.Vector3);
      expect(result.type).toBe('cube-octahedron');
      expect(result.strength).toBeGreaterThanOrEqual(0);
      expect(result.strength).toBeLessThanOrEqual(1);
    });

    it('should calculate direction vector correctly', () => {
      const fromBQF: BQF = [8, 12, 6];
      const toBQF: BQF = [6, 12, 8];
      const result = constraintPointerService.createConstraintPointer(fromBQF, toBQF);
      
      // Direction should be normalized
      expect(result.direction.length()).toBeCloseTo(1, 5);
      
      // Direction components: [a2-a1, b2-b1, c2-c1] = [-2, 0, 2]
      expect(result.direction.x).toBeCloseTo(-2 / Math.sqrt(8), 5);
      expect(result.direction.y).toBe(0);
      expect(result.direction.z).toBeCloseTo(2 / Math.sqrt(8), 5);
    });

    it('should calculate strength based on asymmetry', () => {
      const fromBQF: BQF = [8, 12, 6];
      const toBQF: BQF = [6, 12, 8];
      const result = constraintPointerService.createConstraintPointer(fromBQF, toBQF);
      
      // Asymmetry: |8-6| + |6-8| = 2 + 2 = 4
      // Strength: min(4/20, 1.0) = 0.2
      expect(result.strength).toBeCloseTo(0.2, 5);
    });

    it('should handle icosahedron-dodecahedron pair', () => {
      const fromBQF: BQF = [12, 30, 20];
      const toBQF: BQF = [20, 30, 12];
      const result = constraintPointerService.createConstraintPointer(fromBQF, toBQF);
      
      expect(result.type).toBe('icosahedron-dodecahedron');
      expect(result.strength).toBeGreaterThan(0);
    });
  });

  describe('createDualSwapConstraint', () => {
    it('should create constraint via dual swap', () => {
      const bqf: BQF = [8, 12, 6];
      const result = constraintPointerService.createDualSwapConstraint(bqf);
      
      expect(result.from).toEqual([8, 12, 6]);
      expect(result.to).toEqual([6, 12, 8]); // Dual swap
      expect(result.type).toBe('cube-octahedron');
    });

    it('should handle tetrahedron (self-dual)', () => {
      const bqf: BQF = [4, 6, 4];
      const result = constraintPointerService.createDualSwapConstraint(bqf);
      
      expect(result.from).toEqual([4, 6, 4]);
      expect(result.to).toEqual([4, 6, 4]); // Self-dual
      expect(result.type).toBe('tetrahedron-self');
      expect(result.strength).toBe(0); // No asymmetry for self-dual
    });
  });

  describe('cubeToOctahedronConstraint', () => {
    it('should create cube to octahedron constraint', () => {
      const result = constraintPointerService.cubeToOctahedronConstraint();
      
      expect(result.from).toEqual([8, 12, 6]);
      expect(result.to).toEqual([6, 12, 8]);
      expect(result.type).toBe('cube-octahedron');
    });

    it('should accept custom cube BQF', () => {
      const customCube: BQF = [8, 12, 6];
      const result = constraintPointerService.cubeToOctahedronConstraint(customCube);
      
      expect(result.from).toEqual([8, 12, 6]);
      expect(result.to).toEqual([6, 12, 8]);
    });
  });

  describe('icosahedronToDodecahedronConstraint', () => {
    it('should create icosahedron to dodecahedron constraint', () => {
      const result = constraintPointerService.icosahedronToDodecahedronConstraint();
      
      expect(result.from).toEqual([12, 30, 20]);
      expect(result.to).toEqual([20, 30, 12]);
      expect(result.type).toBe('icosahedron-dodecahedron');
    });

    it('should accept custom icosahedron BQF', () => {
      const customIcosa: BQF = [12, 30, 20];
      const result = constraintPointerService.icosahedronToDodecahedronConstraint(customIcosa);
      
      expect(result.from).toEqual([12, 30, 20]);
      expect(result.to).toEqual([20, 30, 12]);
    });
  });

  describe('applyConstraint', () => {
    it('should apply constraint (forward transformation)', () => {
      const constraint = constraintPointerService.cubeToOctahedronConstraint();
      const result = constraintPointerService.applyConstraint(constraint);
      
      // Should apply BQF transformation to target
      expect(result).toEqual([6, 12, 7]); // apply([6, 12, 8]) = [6, 12, 7]
    });
  });

  describe('abstractConstraint', () => {
    it('should abstract constraint (backward transformation)', () => {
      const constraint = constraintPointerService.cubeToOctahedronConstraint();
      const result = constraintPointerService.abstractConstraint(constraint);
      
      // Should abstract BQF transformation from source
      expect(result).toEqual([8, 12, 7]); // abstract([8, 12, 6]) = [8, 12, 7]
    });
  });

  describe('getDirection', () => {
    it('should get constraint direction as 3D vector', () => {
      const constraint = constraintPointerService.cubeToOctahedronConstraint();
      const direction = constraintPointerService.getDirection(constraint);
      
      expect(direction).toBeInstanceOf(THREE.Vector3);
      expect(direction.length()).toBeCloseTo(1, 5); // Normalized
    });

    it('should return a clone of the direction', () => {
      const constraint = constraintPointerService.cubeToOctahedronConstraint();
      const direction1 = constraintPointerService.getDirection(constraint);
      const direction2 = constraintPointerService.getDirection(constraint);
      
      // Should be equal but not the same object
      expect(direction1).not.toBe(direction2);
      expect(direction1.x).toBe(direction2.x);
      expect(direction1.y).toBe(direction2.y);
      expect(direction1.z).toBe(direction2.z);
    });
  });

  describe('isValidConstraint', () => {
    it('should return true for constraint with asymmetry', () => {
      const constraint = constraintPointerService.cubeToOctahedronConstraint();
      expect(constraintPointerService.isValidConstraint(constraint)).toBe(true);
    });

    it('should return false for self-dual constraint (no asymmetry)', () => {
      const constraint = constraintPointerService.createDualSwapConstraint([4, 6, 4]);
      expect(constraintPointerService.isValidConstraint(constraint)).toBe(false);
    });
  });

  describe('composeConstraints', () => {
    it('should compose multiple constraint pointers', () => {
      const constraint1 = constraintPointerService.cubeToOctahedronConstraint();
      const constraint2 = constraintPointerService.icosahedronToDodecahedronConstraint();
      
      const result = constraintPointerService.composeConstraints([constraint1, constraint2]);
      
      expect(result.from).toEqual([8, 12, 6]); // From first constraint
      expect(result.to).toEqual([20, 30, 12]); // To last constraint
      expect(result.direction).toBeInstanceOf(THREE.Vector3);
    });

    it('should handle single constraint', () => {
      const constraint = constraintPointerService.cubeToOctahedronConstraint();
      const result = constraintPointerService.composeConstraints([constraint]);
      
      expect(result.from).toEqual(constraint.from);
      expect(result.to).toEqual(constraint.to);
    });

    it('should throw error for empty constraint array', () => {
      expect(() => constraintPointerService.composeConstraints([])).toThrow('Cannot compose empty constraint array');
    });
  });

  describe('getStrength', () => {
    it('should get constraint strength', () => {
      const constraint = constraintPointerService.cubeToOctahedronConstraint();
      const strength = constraintPointerService.getStrength(constraint);
      
      expect(strength).toBeGreaterThanOrEqual(0);
      expect(strength).toBeLessThanOrEqual(1);
      expect(strength).toBe(constraint.strength);
    });

    it('should return 0 for self-dual constraint', () => {
      const constraint = constraintPointerService.createDualSwapConstraint([4, 6, 4]);
      const strength = constraintPointerService.getStrength(constraint);
      expect(strength).toBe(0);
    });
  });

  describe('createActionConstraint', () => {
    it('should create constraint pointer from action', () => {
      const action: BQF = [8, 12, 6];
      const result = constraintPointerService.createActionConstraint(action);
      
      expect(result.from).toEqual([8, 12, 6]);
      expect(result.to).toEqual([8, 12, 5]); // apply([8, 12, 6]) = [8, 12, 5]
      expect(result.direction).toBeInstanceOf(THREE.Vector3);
    });
  });

  describe('createObservationConstraint', () => {
    it('should create constraint pointer from observation', () => {
      const observation: BQF = [6, 12, 8];
      const result = constraintPointerService.createObservationConstraint(observation);
      
      expect(result.from).toEqual([6, 12, 8]);
      expect(result.to).toEqual([6, 12, 9]); // abstract([6, 12, 8]) = [6, 12, 9]
      expect(result.direction).toBeInstanceOf(THREE.Vector3);
    });
  });

  describe('integration tests', () => {
    it('should create and apply cube-octahedron constraint chain', () => {
      const constraint = constraintPointerService.cubeToOctahedronConstraint();
      const applied = constraintPointerService.applyConstraint(constraint);
      const abstracted = constraintPointerService.abstractConstraint(constraint);
      
      expect(applied).toEqual([6, 12, 7]);
      expect(abstracted).toEqual([8, 12, 7]);
    });

    it('should compose multiple constraints correctly', () => {
      const constraint1 = constraintPointerService.cubeToOctahedronConstraint();
      const constraint2 = constraintPointerService.createDualSwapConstraint([6, 12, 8]);
      
      const composed = constraintPointerService.composeConstraints([constraint1, constraint2]);
      expect(composed.from).toEqual([8, 12, 6]);
      expect(composed.to).toEqual([8, 12, 6]); // Dual swap of octahedron back to cube
    });
  });
});

