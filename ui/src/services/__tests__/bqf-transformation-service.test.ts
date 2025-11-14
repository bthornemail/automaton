/**
 * BQF Transformation Service Tests
 * 
 * Tests for Binary Quadratic Form transformations
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { bqfTransformationService, type BQF } from '../bqf-transformation-service';

describe('BQFTransformationService', () => {
  describe('apply', () => {
    it('should apply forward transformation [a,b,c] → [a,b,c-1]', () => {
      const bqf: BQF = [8, 12, 6];
      const result = bqfTransformationService.apply(bqf);
      expect(result).toEqual([8, 12, 5]);
    });

    it('should handle cube BQF transformation', () => {
      const cubeBQF: BQF = [8, 12, 6];
      const result = bqfTransformationService.apply(cubeBQF);
      expect(result).toEqual([8, 12, 5]);
    });

    it('should handle octahedron BQF transformation', () => {
      const octaBQF: BQF = [6, 12, 8];
      const result = bqfTransformationService.apply(octaBQF);
      expect(result).toEqual([6, 12, 7]);
    });

    it('should throw error when c=0 (no projective component)', () => {
      const bqf: BQF = [8, 12, 0];
      expect(() => bqfTransformationService.apply(bqf)).toThrow('Cannot apply: c=0 (no projective component)');
    });

    it('should throw error when c<0', () => {
      const bqf: BQF = [8, 12, -1];
      expect(() => bqfTransformationService.apply(bqf)).toThrow('Cannot apply: c=0 (no projective component)');
    });
  });

  describe('abstract', () => {
    it('should abstract backward transformation [a,b,c] → [a,b,c+1]', () => {
      const bqf: BQF = [8, 12, 6];
      const result = bqfTransformationService.abstract(bqf);
      expect(result).toEqual([8, 12, 7]);
    });

    it('should handle cube BQF abstraction', () => {
      const cubeBQF: BQF = [8, 12, 6];
      const result = bqfTransformationService.abstract(cubeBQF);
      expect(result).toEqual([8, 12, 7]);
    });

    it('should handle octahedron BQF abstraction', () => {
      const octaBQF: BQF = [6, 12, 8];
      const result = bqfTransformationService.abstract(octaBQF);
      expect(result).toEqual([6, 12, 9]);
    });

    it('should work with c=0', () => {
      const bqf: BQF = [8, 12, 0];
      const result = bqfTransformationService.abstract(bqf);
      expect(result).toEqual([8, 12, 1]);
    });
  });

  describe('dualSwap', () => {
    it('should swap affine and projective components [a,b,c] → [c,b,a]', () => {
      const bqf: BQF = [8, 12, 6];
      const result = bqfTransformationService.dualSwap(bqf);
      expect(result).toEqual([6, 12, 8]);
    });

    it('should transform cube to octahedron', () => {
      const cubeBQF: BQF = [8, 12, 6];
      const result = bqfTransformationService.dualSwap(cubeBQF);
      expect(result).toEqual([6, 12, 8]); // Octahedron
    });

    it('should transform octahedron to cube', () => {
      const octaBQF: BQF = [6, 12, 8];
      const result = bqfTransformationService.dualSwap(octaBQF);
      expect(result).toEqual([8, 12, 6]); // Cube
    });

    it('should transform icosahedron to dodecahedron', () => {
      const icosaBQF: BQF = [12, 30, 20];
      const result = bqfTransformationService.dualSwap(icosaBQF);
      expect(result).toEqual([20, 30, 12]); // Dodecahedron
    });

    it('should transform dodecahedron to icosahedron', () => {
      const dodecaBQF: BQF = [20, 30, 12];
      const result = bqfTransformationService.dualSwap(dodecaBQF);
      expect(result).toEqual([12, 30, 20]); // Icosahedron
    });

    it('should preserve tetrahedron (self-dual)', () => {
      const tetraBQF: BQF = [4, 6, 4];
      const result = bqfTransformationService.dualSwap(tetraBQF);
      expect(result).toEqual([4, 6, 4]); // Self-dual
    });
  });

  describe('compose', () => {
    it('should compose two BQF transformations', () => {
      const q1: BQF = [8, 12, 6];
      const q2: BQF = [6, 12, 8];
      const result = bqfTransformationService.compose(q1, q2);
      
      // Matrix multiplication: [a1*a2 + b1*a2, a1*b2 + b1*b2 + c1*a2, b1*c2 + c1*c2]
      // [8*6 + 12*6, 8*12 + 12*12 + 6*6, 12*8 + 6*8]
      // [48 + 72, 96 + 144 + 36, 96 + 48]
      // [120, 276, 144]
      expect(result[0]).toBe(120);
      expect(result[1]).toBe(276);
      expect(result[2]).toBe(144);
    });

    it('should compose cube and octahedron', () => {
      const cubeBQF: BQF = [8, 12, 6];
      const octaBQF: BQF = [6, 12, 8];
      const result = bqfTransformationService.compose(cubeBQF, octaBQF);
      expect(result).toHaveLength(3);
      expect(typeof result[0]).toBe('number');
      expect(typeof result[1]).toBe('number');
      expect(typeof result[2]).toBe('number');
    });

    it('should handle identity composition', () => {
      const bqf: BQF = [1, 0, 0];
      const identity: BQF = [1, 0, 0];
      const result = bqfTransformationService.compose(bqf, identity);
      expect(result[0]).toBe(1);
      expect(result[1]).toBe(0);
      expect(result[2]).toBe(0);
    });
  });

  describe('canApply', () => {
    it('should return true when c > 0', () => {
      const bqf: BQF = [8, 12, 6];
      expect(bqfTransformationService.canApply(bqf)).toBe(true);
    });

    it('should return false when c = 0', () => {
      const bqf: BQF = [8, 12, 0];
      expect(bqfTransformationService.canApply(bqf)).toBe(false);
    });

    it('should return false when c < 0', () => {
      const bqf: BQF = [8, 12, -1];
      expect(bqfTransformationService.canApply(bqf)).toBe(false);
    });
  });

  describe('isSelfDual', () => {
    it('should return true for tetrahedron (self-dual)', () => {
      const tetraBQF: BQF = [4, 6, 4];
      expect(bqfTransformationService.isSelfDual(tetraBQF)).toBe(true);
    });

    it('should return false for cube', () => {
      const cubeBQF: BQF = [8, 12, 6];
      expect(bqfTransformationService.isSelfDual(cubeBQF)).toBe(false);
    });

    it('should return false for octahedron', () => {
      const octaBQF: BQF = [6, 12, 8];
      expect(bqfTransformationService.isSelfDual(octaBQF)).toBe(false);
    });

    it('should return true when a === c', () => {
      const bqf: BQF = [5, 10, 5];
      expect(bqfTransformationService.isSelfDual(bqf)).toBe(true);
    });
  });

  describe('createResult', () => {
    it('should create result with metadata', () => {
      const bqf: BQF = [8, 12, 6];
      const result = bqfTransformationService.createResult(bqf, 'dual-swap');
      
      expect(result.bqf).toEqual([8, 12, 6]);
      expect(result.operation).toBe('dual-swap');
      expect(result.timestamp).toBeTypeOf('number');
      expect(result.timestamp).toBeGreaterThan(0);
    });

    it('should create result for apply operation', () => {
      const bqf: BQF = [8, 12, 6];
      const result = bqfTransformationService.createResult(bqf, 'apply');
      expect(result.operation).toBe('apply');
    });

    it('should create result for abstract operation', () => {
      const bqf: BQF = [8, 12, 6];
      const result = bqfTransformationService.createResult(bqf, 'abstract');
      expect(result.operation).toBe('abstract');
    });

    it('should create result for compose operation', () => {
      const bqf: BQF = [8, 12, 6];
      const result = bqfTransformationService.createResult(bqf, 'compose');
      expect(result.operation).toBe('compose');
    });
  });

  describe('isValid', () => {
    it('should return true for valid BQF array', () => {
      const bqf: BQF = [8, 12, 6];
      expect(bqfTransformationService.isValid(bqf)).toBe(true);
    });

    it('should return false for non-array', () => {
      expect(bqfTransformationService.isValid('not an array')).toBe(false);
      expect(bqfTransformationService.isValid(42)).toBe(false);
      expect(bqfTransformationService.isValid(null)).toBe(false);
    });

    it('should return false for array with wrong length', () => {
      expect(bqfTransformationService.isValid([8, 12])).toBe(false);
      expect(bqfTransformationService.isValid([8, 12, 6, 4])).toBe(false);
    });

    it('should return false for array with non-numbers', () => {
      expect(bqfTransformationService.isValid(['8', 12, 6])).toBe(false);
      expect(bqfTransformationService.isValid([8, '12', 6])).toBe(false);
      expect(bqfTransformationService.isValid([8, 12, '6'])).toBe(false);
    });
  });

  describe('toString', () => {
    it('should convert BQF to string representation', () => {
      const bqf: BQF = [8, 12, 6];
      const result = bqfTransformationService.toString(bqf);
      expect(result).toBe('[8, 12, 6]');
    });

    it('should handle zero values', () => {
      const bqf: BQF = [0, 0, 0];
      const result = bqfTransformationService.toString(bqf);
      expect(result).toBe('[0, 0, 0]');
    });

    it('should handle negative values', () => {
      const bqf: BQF = [-1, -2, -3];
      const result = bqfTransformationService.toString(bqf);
      expect(result).toBe('[-1, -2, -3]');
    });
  });

  describe('parse', () => {
    it('should parse valid BQF array', () => {
      const input: number[] = [8, 12, 6];
      const result = bqfTransformationService.parse(input);
      expect(result).toEqual([8, 12, 6]);
    });

    it('should parse valid BQF string', () => {
      const input = '[8, 12, 6]';
      const result = bqfTransformationService.parse(input);
      expect(result).toEqual([8, 12, 6]);
    });

    it('should parse string without spaces', () => {
      const input = '[8,12,6]';
      const result = bqfTransformationService.parse(input);
      expect(result).toEqual([8, 12, 6]);
    });

    it('should throw error for invalid array', () => {
      expect(() => bqfTransformationService.parse([8, 12])).toThrow('Invalid BQF array');
      expect(() => bqfTransformationService.parse([8, 12, 6, 4])).toThrow('Invalid BQF array');
    });

    it('should throw error for invalid string format', () => {
      expect(() => bqfTransformationService.parse('invalid')).toThrow('Invalid BQF string format');
      expect(() => bqfTransformationService.parse('8, 12, 6')).toThrow('Invalid BQF string format');
    });

    it('should throw error for array with non-numbers', () => {
      expect(() => bqfTransformationService.parse(['8', 12, 6])).toThrow('Invalid BQF array');
    });
  });

  describe('integration tests', () => {
    it('should apply then abstract return to original', () => {
      const original: BQF = [8, 12, 6];
      const applied = bqfTransformationService.apply(original);
      const abstracted = bqfTransformationService.abstract(applied);
      expect(abstracted).toEqual(original);
    });

    it('should dual swap twice return to original', () => {
      const original: BQF = [8, 12, 6];
      const swapped = bqfTransformationService.dualSwap(original);
      const swappedBack = bqfTransformationService.dualSwap(swapped);
      expect(swappedBack).toEqual(original);
    });

    it('should compose cube and octahedron transformations', () => {
      const cubeBQF: BQF = [8, 12, 6];
      const octaBQF: BQF = [6, 12, 8];
      const composed = bqfTransformationService.compose(cubeBQF, octaBQF);
      expect(composed).toHaveLength(3);
      expect(composed[0]).toBeGreaterThan(0);
      expect(composed[1]).toBeGreaterThan(0);
      expect(composed[2]).toBeGreaterThan(0);
    });
  });
});

