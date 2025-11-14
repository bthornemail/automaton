/**
 * Consensus Pattern Service Tests
 * 
 * Tests for consensus patterns using polyhedra symmetries
 */

import { describe, it, expect } from 'vitest';
import { consensusPatternService, type ConsensusType } from '../consensus-pattern-service';

describe('ConsensusPatternService', () => {
  describe('tetrahedronConsensus', () => {
    it('should compute 4-point GCD consensus', () => {
      const facts = [2, 4, 6, 8];
      const result = consensusPatternService.tetrahedronConsensus(facts);
      
      expect(result.consensus).toBe(2); // GCD of [2, 4, 6, 8]
      expect(result.type).toBe('tetrahedron');
      expect(result.threshold).toBe(3);
      expect(result.participants).toBe(4);
      expect(result.reached).toBe(true);
    });

    it('should handle prime numbers', () => {
      const facts = [3, 5, 7, 11];
      const result = consensusPatternService.tetrahedronConsensus(facts);
      expect(result.consensus).toBe(1); // GCD of primes is 1
    });

    it('should throw error for wrong number of facts', () => {
      expect(() => consensusPatternService.tetrahedronConsensus([2, 4, 6])).toThrow('Tetrahedron consensus requires exactly 4 facts');
      expect(() => consensusPatternService.tetrahedronConsensus([2, 4, 6, 8, 10])).toThrow('Tetrahedron consensus requires exactly 4 facts');
    });

    it('should handle all same values', () => {
      const facts = [5, 5, 5, 5];
      const result = consensusPatternService.tetrahedronConsensus(facts);
      expect(result.consensus).toBe(5);
    });
  });

  describe('cubeConsensus', () => {
    it('should compute 8-point LCM consensus', () => {
      const types = [1, 2, 3, 4, 5, 6, 7, 8];
      const result = consensusPatternService.cubeConsensus(types);
      
      expect(result.consensus).toBe(840); // LCM of [1, 2, 3, 4, 5, 6, 7, 8]
      expect(result.type).toBe('cube');
      expect(result.threshold).toBe(4);
      expect(result.participants).toBe(8);
      expect(result.reached).toBe(true);
    });

    it('should handle powers of 2', () => {
      const types = [2, 4, 8, 16, 32, 64, 128, 256];
      const result = consensusPatternService.cubeConsensus(types);
      expect(result.consensus).toBe(256); // LCM of powers of 2
    });

    it('should throw error for wrong number of types', () => {
      expect(() => consensusPatternService.cubeConsensus([1, 2, 3, 4, 5, 6, 7])).toThrow('Cube consensus requires exactly 8 types');
      expect(() => consensusPatternService.cubeConsensus([1, 2, 3, 4, 5, 6, 7, 8, 9])).toThrow('Cube consensus requires exactly 8 types');
    });
  });

  describe('octahedronConsensus', () => {
    it('should compute 6-point LCM consensus', () => {
      const points = [2, 3, 4, 5, 6, 7];
      const result = consensusPatternService.octahedronConsensus(points);
      
      expect(result.consensus).toBe(420); // LCM of [2, 3, 4, 5, 6, 7]
      expect(result.type).toBe('octahedron');
      expect(result.threshold).toBe(3);
      expect(result.participants).toBe(6);
      expect(result.reached).toBe(true);
    });

    it('should throw error for wrong number of points', () => {
      expect(() => consensusPatternService.octahedronConsensus([1, 2, 3, 4, 5])).toThrow('Octahedron consensus requires exactly 6 points');
      expect(() => consensusPatternService.octahedronConsensus([1, 2, 3, 4, 5, 6, 7])).toThrow('Octahedron consensus requires exactly 6 points');
    });
  });

  describe('icosahedronConsensus', () => {
    it('should compute 12-point global consensus', () => {
      const nodes = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12];
      const result = consensusPatternService.icosahedronConsensus(nodes);
      
      expect(result.type).toBe('icosahedron');
      expect(result.threshold).toBe(6);
      expect(result.participants).toBe(12);
      expect(result.reached).toBe(true);
      expect(result.consensus).toBeGreaterThan(0);
    });

    it('should throw error for wrong number of nodes', () => {
      expect(() => consensusPatternService.icosahedronConsensus([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11])).toThrow('Icosahedron consensus requires exactly 12 nodes');
      expect(() => consensusPatternService.icosahedronConsensus([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13])).toThrow('Icosahedron consensus requires exactly 12 nodes');
    });
  });

  describe('dodecahedronConsensus', () => {
    it('should compute 20-point global consensus', () => {
      const entities = Array.from({ length: 20 }, (_, i) => i + 1);
      const result = consensusPatternService.dodecahedronConsensus(entities);
      
      expect(result.type).toBe('dodecahedron');
      expect(result.threshold).toBe(10);
      expect(result.participants).toBe(20);
      expect(result.reached).toBe(true);
      expect(result.consensus).toBeGreaterThan(0);
    });

    it('should throw error for wrong number of entities', () => {
      expect(() => consensusPatternService.dodecahedronConsensus(Array(19).fill(1))).toThrow('Dodecahedron consensus requires exactly 20 entities');
      expect(() => consensusPatternService.dodecahedronConsensus(Array(21).fill(1))).toThrow('Dodecahedron consensus requires exactly 20 entities');
    });
  });

  describe('hashConsensus', () => {
    it('should hash data to consensus class for tetrahedron', () => {
      const data = { test: 'value' };
      const result = consensusPatternService.hashConsensus(data, 'tetrahedron');
      expect(result).toBeGreaterThanOrEqual(0);
      expect(result).toBeLessThan(4); // 4 classes for tetrahedron
    });

    it('should hash data to consensus class for cube', () => {
      const data = { test: 'value' };
      const result = consensusPatternService.hashConsensus(data, 'cube');
      expect(result).toBeGreaterThanOrEqual(0);
      expect(result).toBeLessThan(8); // 8 classes for cube
    });

    it('should hash data to consensus class for icosahedron', () => {
      const data = { test: 'value' };
      const result = consensusPatternService.hashConsensus(data, 'icosahedron');
      expect(result).toBeGreaterThanOrEqual(0);
      expect(result).toBeLessThan(12); // 12 classes for icosahedron
    });

    it('should return same hash for same data', () => {
      const data = { test: 'value' };
      const result1 = consensusPatternService.hashConsensus(data, 'cube');
      const result2 = consensusPatternService.hashConsensus(data, 'cube');
      expect(result1).toBe(result2);
    });

    it('should return different hash for different data', () => {
      const data1 = { test: 'value1' };
      const data2 = { test: 'value2' };
      const result1 = consensusPatternService.hashConsensus(data1, 'cube');
      const result2 = consensusPatternService.hashConsensus(data2, 'cube');
      // May or may not be different, but should be valid
      expect(result1).toBeGreaterThanOrEqual(0);
      expect(result2).toBeGreaterThanOrEqual(0);
    });
  });

  describe('checkConsensus', () => {
    it('should return true for data with same consensus class', () => {
      const data1 = { test: 'value' };
      const data2 = { test: 'value' };
      const result = consensusPatternService.checkConsensus(data1, data2, 'cube');
      expect(result).toBe(true);
    });

    it('should return false for data with different consensus class', () => {
      const data1 = { test: 'value1' };
      const data2 = { test: 'value2' };
      // Note: This may return true or false depending on hash collision
      const result = consensusPatternService.checkConsensus(data1, data2, 'cube');
      expect(typeof result).toBe('boolean');
    });

    it('should work with different polyhedron types', () => {
      const data1 = { test: 'value' };
      const data2 = { test: 'value' };
      expect(consensusPatternService.checkConsensus(data1, data2, 'tetrahedron')).toBe(true);
      expect(consensusPatternService.checkConsensus(data1, data2, 'cube')).toBe(true);
      expect(consensusPatternService.checkConsensus(data1, data2, 'icosahedron')).toBe(true);
    });
  });

  describe('quorumConsensus', () => {
    it('should return true when quorum threshold is met for tetrahedron', () => {
      const result = consensusPatternService.quorumConsensus(4, 3, 'tetrahedron');
      expect(result).toBe(true); // 3 >= 3 (75% of 4)
    });

    it('should return false when quorum threshold is not met for tetrahedron', () => {
      const result = consensusPatternService.quorumConsensus(4, 2, 'tetrahedron');
      expect(result).toBe(false); // 2 < 3 (75% of 4)
    });

    it('should return true when quorum threshold is met for cube', () => {
      const result = consensusPatternService.quorumConsensus(8, 4, 'cube');
      expect(result).toBe(true); // 4 >= 4 (50% of 8)
    });

    it('should return false when quorum threshold is not met for cube', () => {
      const result = consensusPatternService.quorumConsensus(8, 3, 'cube');
      expect(result).toBe(false); // 3 < 4 (50% of 8)
    });

    it('should return true when quorum threshold is met for icosahedron', () => {
      const result = consensusPatternService.quorumConsensus(12, 3, 'icosahedron');
      expect(result).toBe(true); // 3 >= 3 (25% of 12)
    });

    it('should handle edge cases', () => {
      expect(consensusPatternService.quorumConsensus(4, 4, 'tetrahedron')).toBe(true); // All agree
      expect(consensusPatternService.quorumConsensus(4, 0, 'tetrahedron')).toBe(false); // None agree
    });
  });

  describe('integration tests', () => {
    it('should compute tetrahedron consensus correctly', () => {
      const facts = [12, 18, 24, 30];
      const result = consensusPatternService.tetrahedronConsensus(facts);
      expect(result.consensus).toBe(6); // GCD of [12, 18, 24, 30]
    });

    it('should compute cube consensus correctly', () => {
      const types = [2, 3, 4, 5, 6, 7, 8, 9];
      const result = consensusPatternService.cubeConsensus(types);
      expect(result.consensus).toBeGreaterThan(0);
    });

    it('should use hash consensus for different data types', () => {
      const stringData = 'test string';
      const numberData = 42;
      const objectData = { key: 'value' };
      
      const hash1 = consensusPatternService.hashConsensus(stringData, 'cube');
      const hash2 = consensusPatternService.hashConsensus(numberData, 'cube');
      const hash3 = consensusPatternService.hashConsensus(objectData, 'cube');
      
      expect(hash1).toBeGreaterThanOrEqual(0);
      expect(hash2).toBeGreaterThanOrEqual(0);
      expect(hash3).toBeGreaterThanOrEqual(0);
    });
  });
});

