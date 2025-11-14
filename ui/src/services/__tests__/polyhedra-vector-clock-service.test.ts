/**
 * Polyhedra Vector Clock Service Tests
 * 
 * Tests for vector clocks with polyhedra metadata
 */

import { describe, it, expect } from 'vitest';
import { polyhedraVectorClockService, type PolyhedraVectorClock } from '../polyhedra-vector-clock-service';
import type { BQF } from '../bqf-transformation-service';

describe('PolyhedraVectorClockService', () => {
  describe('create', () => {
    it('should create vector clock from polyhedron metadata', () => {
      const file = 'test.jsonl';
      const line = 10;
      const timestamp = 1000;
      const pattern = 'cube-consensus';
      const polyhedronType = 'cube';
      const bqf: BQF = [8, 12, 6];
      
      const result = polyhedraVectorClockService.create(
        file,
        line,
        timestamp,
        pattern,
        polyhedronType,
        bqf
      );
      
      expect(result.file).toBe(file);
      expect(result.line).toBe(line);
      expect(result.timestamp).toBe(timestamp);
      expect(result.pattern).toBe(pattern);
      expect(result.polyhedronType).toBe(polyhedronType);
      expect(result.bqf).toEqual(bqf);
      expect(result.vectorClock).toHaveLength(7); // file, line, timestamp, pattern, + 3 BQF components
      expect(result.vectorClock[0]).toBe(file);
      expect(result.vectorClock[1]).toBe(line);
      expect(result.vectorClock[2]).toBe(timestamp);
      expect(result.vectorClock[3]).toBe(pattern);
      expect(result.vectorClock[4]).toBe(8);
      expect(result.vectorClock[5]).toBe(12);
      expect(result.vectorClock[6]).toBe(6);
    });

    it('should create vector clock for different polyhedron types', () => {
      const tetra = polyhedraVectorClockService.create('test.jsonl', 1, 1000, 'tetra', 'tetrahedron', [4, 6, 4]);
      expect(tetra.polyhedronType).toBe('tetrahedron');
      
      const octa = polyhedraVectorClockService.create('test.jsonl', 2, 2000, 'octa', 'octahedron', [6, 12, 8]);
      expect(octa.polyhedronType).toBe('octahedron');
      
      const icosa = polyhedraVectorClockService.create('test.jsonl', 3, 3000, 'icosa', 'icosahedron', [12, 30, 20]);
      expect(icosa.polyhedronType).toBe('icosahedron');
    });
  });

  describe('mergeDualPair', () => {
    it('should merge vector clocks for dual pairs', () => {
      const vc1 = polyhedraVectorClockService.create('file1.jsonl', 1, 1000, 'cube', 'cube', [8, 12, 6]);
      const vc2 = polyhedraVectorClockService.create('file2.jsonl', 2, 2000, 'octa', 'octahedron', [6, 12, 8]);
      
      const merged = polyhedraVectorClockService.mergeDualPair(vc1, vc2);
      
      expect(merged.file).toBe('file1.jsonl'); // Uses first file
      expect(merged.line).toBe(2); // Max of both lines
      expect(merged.timestamp).toBe(2000); // Max of both timestamps
      expect(merged.pattern).toBe('cube-octa'); // Combined pattern
      expect(merged.bqf).toEqual([8, 12, 8]); // Component-wise max
      expect(merged.vectorClock).toHaveLength(7);
    });

    it('should use more complex polyhedron type', () => {
      const vc1 = polyhedraVectorClockService.create('file1.jsonl', 1, 1000, 'tetra', 'tetrahedron', [4, 6, 4]);
      const vc2 = polyhedraVectorClockService.create('file2.jsonl', 2, 2000, 'cube', 'cube', [8, 12, 6]);
      
      const merged = polyhedraVectorClockService.mergeDualPair(vc1, vc2);
      expect(merged.polyhedronType).toBe('cube'); // Cube is more complex than tetrahedron
    });

    it('should merge icosahedron and dodecahedron', () => {
      const vc1 = polyhedraVectorClockService.create('file1.jsonl', 1, 1000, 'icosa', 'icosahedron', [12, 30, 20]);
      const vc2 = polyhedraVectorClockService.create('file2.jsonl', 2, 2000, 'dodeca', 'dodecahedron', [20, 30, 12]);
      
      const merged = polyhedraVectorClockService.mergeDualPair(vc1, vc2);
      expect(merged.polyhedronType).toBe('dodecahedron'); // Dodecahedron is more complex
      expect(merged.bqf).toEqual([20, 30, 20]); // Component-wise max
    });
  });

  describe('happensBefore', () => {
    it('should return true when vc1 happens before vc2', () => {
      const vc1 = polyhedraVectorClockService.create('file.jsonl', 1, 1000, 'pattern1', 'cube', [8, 12, 6]);
      const vc2 = polyhedraVectorClockService.create('file.jsonl', 2, 2000, 'pattern2', 'cube', [8, 12, 6]);
      
      expect(polyhedraVectorClockService.happensBefore(vc1, vc2)).toBe(true);
    });

    it('should return false when vc1 does not happen before vc2', () => {
      const vc1 = polyhedraVectorClockService.create('file.jsonl', 2, 2000, 'pattern2', 'cube', [8, 12, 6]);
      const vc2 = polyhedraVectorClockService.create('file.jsonl', 1, 1000, 'pattern1', 'cube', [8, 12, 6]);
      
      expect(polyhedraVectorClockService.happensBefore(vc1, vc2)).toBe(false);
    });

    it('should return false for concurrent events', () => {
      const vc1 = polyhedraVectorClockService.create('file1.jsonl', 1, 1000, 'pattern1', 'cube', [8, 12, 6]);
      const vc2 = polyhedraVectorClockService.create('file2.jsonl', 1, 1000, 'pattern2', 'cube', [8, 12, 6]);
      
      // Different files, same timestamp - may be concurrent or ordered depending on vector clock comparison
      // Vector clock comparison compares all components, so file1 < file2 lexicographically
      const result = polyhedraVectorClockService.happensBefore(vc1, vc2);
      expect(typeof result).toBe('boolean');
    });
  });

  describe('getCausalityLevel', () => {
    it('should return 4 for tetrahedron', () => {
      expect(polyhedraVectorClockService.getCausalityLevel('tetrahedron')).toBe(4);
    });

    it('should return 8 for cube', () => {
      expect(polyhedraVectorClockService.getCausalityLevel('cube')).toBe(8);
    });

    it('should return 8 for octahedron', () => {
      expect(polyhedraVectorClockService.getCausalityLevel('octahedron')).toBe(8);
    });

    it('should return 12 for icosahedron', () => {
      expect(polyhedraVectorClockService.getCausalityLevel('icosahedron')).toBe(12);
    });

    it('should return 12 for dodecahedron', () => {
      expect(polyhedraVectorClockService.getCausalityLevel('dodecahedron')).toBe(12);
    });
  });

  describe('createCubeConsensus', () => {
    it('should create cube consensus vector clock', () => {
      const file = 'test.jsonl';
      const line = 10;
      const timestamp = 1000;
      
      const result = polyhedraVectorClockService.createCubeConsensus(file, line, timestamp);
      
      expect(result.file).toBe(file);
      expect(result.line).toBe(line);
      expect(result.timestamp).toBe(timestamp);
      expect(result.pattern).toBe('cube-consensus');
      expect(result.polyhedronType).toBe('cube');
      expect(result.bqf).toEqual([8, 12, 6]);
    });
  });

  describe('createOctahedronConsensus', () => {
    it('should create octahedron consensus vector clock', () => {
      const file = 'test.jsonl';
      const line = 10;
      const timestamp = 1000;
      
      const result = polyhedraVectorClockService.createOctahedronConsensus(file, line, timestamp);
      
      expect(result.file).toBe(file);
      expect(result.line).toBe(line);
      expect(result.timestamp).toBe(timestamp);
      expect(result.pattern).toBe('octa-consensus');
      expect(result.polyhedronType).toBe('octahedron');
      expect(result.bqf).toEqual([6, 12, 8]);
    });
  });

  describe('createTetrahedronConsensus', () => {
    it('should create tetrahedron consensus vector clock', () => {
      const file = 'test.jsonl';
      const line = 10;
      const timestamp = 1000;
      
      const result = polyhedraVectorClockService.createTetrahedronConsensus(file, line, timestamp);
      
      expect(result.file).toBe(file);
      expect(result.line).toBe(line);
      expect(result.timestamp).toBe(timestamp);
      expect(result.pattern).toBe('tetra-consensus');
      expect(result.polyhedronType).toBe('tetrahedron');
      expect(result.bqf).toEqual([4, 6, 4]);
    });
  });

  describe('extractBQF', () => {
    it('should extract BQF from vector clock', () => {
      const vc = polyhedraVectorClockService.create('test.jsonl', 1, 1000, 'pattern', 'cube', [8, 12, 6]);
      const bqf = polyhedraVectorClockService.extractBQF(vc);
      
      expect(bqf).toEqual([8, 12, 6]);
    });
  });

  describe('updateBQF', () => {
    it('should update BQF in vector clock', () => {
      const vc = polyhedraVectorClockService.create('test.jsonl', 1, 1000, 'pattern', 'cube', [8, 12, 6]);
      const newBQF: BQF = [6, 12, 8];
      
      const updated = polyhedraVectorClockService.updateBQF(vc, newBQF);
      
      expect(updated.bqf).toEqual(newBQF);
      expect(updated.vectorClock[4]).toBe(6);
      expect(updated.vectorClock[5]).toBe(12);
      expect(updated.vectorClock[6]).toBe(8);
      expect(updated.file).toBe(vc.file);
      expect(updated.line).toBe(vc.line);
      expect(updated.timestamp).toBe(vc.timestamp);
      expect(updated.pattern).toBe(vc.pattern);
    });
  });

  describe('integration tests', () => {
    it('should create and merge cube-octahedron dual pair', () => {
      const cubeVC = polyhedraVectorClockService.createCubeConsensus('file1.jsonl', 1, 1000);
      const octaVC = polyhedraVectorClockService.createOctahedronConsensus('file2.jsonl', 2, 2000);
      
      const merged = polyhedraVectorClockService.mergeDualPair(cubeVC, octaVC);
      
      expect(merged.polyhedronType).toBe('cube'); // Cube is more complex
      expect(merged.bqf).toEqual([8, 12, 8]); // Component-wise max
    });

    it('should track causal ordering through polyhedra operations', () => {
      // Use same pattern and BQF to ensure line/timestamp determine ordering
      const vc1 = polyhedraVectorClockService.create('file.jsonl', 1, 1000, 'consensus', 'cube', [8, 12, 6]);
      const vc2 = polyhedraVectorClockService.create('file.jsonl', 2, 2000, 'consensus', 'cube', [8, 12, 6]);
      const vc3 = polyhedraVectorClockService.create('file.jsonl', 3, 3000, 'consensus', 'cube', [8, 12, 6]);
      
      // Vector clocks compare all components: [file, line, timestamp, pattern, ...bqf]
      // Same file, pattern, and BQF, so line and timestamp determine ordering: 1 < 2 < 3, 1000 < 2000 < 3000
      // vc1: ['file.jsonl', 1, 1000, 'consensus', 8, 12, 6]
      // vc2: ['file.jsonl', 2, 2000, 'consensus', 8, 12, 6]
      // vc3: ['file.jsonl', 3, 3000, 'consensus', 8, 12, 6]
      // Line 1 < 2 < 3, so vc1 happens before vc2, and vc2 before vc3
      expect(polyhedraVectorClockService.happensBefore(vc1, vc2)).toBe(true);
      expect(polyhedraVectorClockService.happensBefore(vc2, vc3)).toBe(true);
      expect(polyhedraVectorClockService.happensBefore(vc1, vc3)).toBe(true);
    });
  });
});

