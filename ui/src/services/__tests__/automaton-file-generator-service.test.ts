/**
 * Automaton File Generator Service Tests
 */

import { describe, test, expect, beforeEach } from 'vitest';
import { AutomatonFileGeneratorService } from '../automaton-file-generator-service';
import { generateMockAutomatonState, generateAllDimensionStates } from './utils/mockAutomatonState';

describe('AutomatonFileGeneratorService', () => {
  let service: AutomatonFileGeneratorService;

  beforeEach(() => {
    service = new AutomatonFileGeneratorService();
  });

  describe('generateKernelCanvasL', () => {
    test('should generate kernel CanvasL with sample automaton state', () => {
      const state = generateMockAutomatonState({
        id: 'test-automaton',
        dimension: '0D',
        kernelCount: 3
      });

      const result = service.generateKernelCanvasL(state);

      expect(result).toContain('@version 1.0.0');
      expect(result).toContain('@schema automaton-kernel');
      expect(result.split('\n').filter(line => line.trim() && !line.startsWith('@')).length).toBe(3);
    });

    test('should include version directive', () => {
      const state = generateMockAutomatonState({ dimension: '0D' });

      const result = service.generateKernelCanvasL(state);

      expect(result).toContain('@version 1.0.0');
    });

    test('should include schema directive', () => {
      const state = generateMockAutomatonState({ dimension: '0D' });

      const result = service.generateKernelCanvasL(state);

      expect(result).toContain('@schema automaton-kernel');
    });

    test('should serialize kernel entries', () => {
      const state = generateMockAutomatonState({
        dimension: '0D',
        kernelCount: 2
      });

      const result = service.generateKernelCanvasL(state);
      const lines = result.split('\n').filter(line => line.trim() && !line.startsWith('@'));

      expect(lines.length).toBe(2);
      lines.forEach(line => {
        const entry = JSON.parse(line);
        expect(entry.type).toBe('automaton');
        expect(entry.dimension).toBe('0D');
      });
    });

    test('should include dimension in entries', () => {
      const state = generateMockAutomatonState({
        dimension: '2D',
        kernelCount: 1
      });

      const result = service.generateKernelCanvasL(state);
      const lines = result.split('\n').filter(line => line.trim() && !line.startsWith('@'));
      const entry = JSON.parse(lines[0]);

      expect(entry.dimension).toBe('2D');
    });
  });

  describe('generateSeedCanvasL', () => {
    test('should generate seed CanvasL with versioning data', () => {
      const state = generateMockAutomatonState({
        id: 'test-automaton',
        dimension: '0D',
        seedCount: 1
      });

      const result = service.generateSeedCanvasL(state);

      expect(result).toContain('@version 1.0.0');
      expect(result).toContain('@schema automaton-seed');
    });

    test('should include seed entry structure', () => {
      const state = generateMockAutomatonState({
        id: 'test-automaton',
        dimension: '0D',
        seedCount: 1
      });

      const result = service.generateSeedCanvasL(state);
      const lines = result.split('\n').filter(line => line.trim() && !line.startsWith('@'));
      const entry = JSON.parse(lines[0]);

      expect(entry.id).toBe('test-automaton-seed');
      expect(entry.type).toBe('seed');
      expect(entry.dimension).toBe('0D');
      expect(entry.version).toBe('1.0.0');
    });

    test('should include regeneration function specification', () => {
      const state = generateMockAutomatonState({
        id: 'test-automaton',
        dimension: '0D'
      });

      const result = service.generateSeedCanvasL(state);
      const lines = result.split('\n').filter(line => line.trim() && !line.startsWith('@'));
      const entry = JSON.parse(lines[0]);

      expect(entry.regeneration).toBeDefined();
      expect(entry.regeneration.function).toBe('r5rs:parse-jsonl-canvas');
      expect(entry.regeneration.args).toEqual(['automaton.kernel.canvasl']);
    });

    test('should include provenance history', () => {
      const state = generateMockAutomatonState({
        id: 'test-automaton',
        dimension: '0D',
        seedCount: 2
      });

      const result = service.generateSeedCanvasL(state);
      const lines = result.split('\n').filter(line => line.trim() && !line.startsWith('@'));
      const entry = JSON.parse(lines[0]);

      expect(entry.provenanceHistory).toBeDefined();
      expect(Array.isArray(entry.provenanceHistory)).toBe(true);
    });

    test('should include kernel URL reference', () => {
      const state = generateMockAutomatonState({
        id: 'test-automaton',
        dimension: '0D'
      });

      const result = service.generateSeedCanvasL(state);
      const lines = result.split('\n').filter(line => line.trim() && !line.startsWith('@'));
      const entry = JSON.parse(lines[0]);

      expect(entry.kernelUrl).toBe('./automaton.kernel.canvasl');
    });
  });

  describe('generateTopologyCanvasL', () => {
    test('should generate topology CanvasL with topology partition data', () => {
      const state = generateMockAutomatonState({
        dimension: '0D',
        topologyCount: 2
      });

      const result = service.generateTopologyCanvasL(state);

      expect(result).toContain('@version 1.0.0');
      expect(result).toContain('@schema metaverse-topology');
    });

    test('should include Bipartite-BQF metadata', () => {
      const state = generateMockAutomatonState({
        dimension: '2D',
        topologyCount: 1
      });

      const result = service.generateTopologyCanvasL(state);
      const lines = result.split('\n').filter(line => line.trim() && !line.startsWith('@'));
      const entry = JSON.parse(lines[0]);

      expect(entry.frontmatter).toBeDefined();
      expect(entry.frontmatter.bipartite).toBeDefined();
      expect(entry.frontmatter.bipartite.partition).toBe('topology');
      expect(entry.frontmatter.bipartite.dimension).toBe('2D');
    });

    test('should include BQF coefficients for topology partition', () => {
      const dimensions = ['0D', '1D', '2D', '3D'];
      
      for (const dimension of dimensions) {
        const state = generateMockAutomatonState({
          dimension,
          topologyCount: 1
        });

        const result = service.generateTopologyCanvasL(state);
        const lines = result.split('\n').filter(line => line.trim() && !line.startsWith('@'));
        const entry = JSON.parse(lines[0]);

        const bqf = entry.frontmatter.bipartite.bqf;
        expect(bqf.coefficients).toEqual([1, 0, parseInt(dimension.replace('D', ''))]);
      }
    });

    test('should include signature generation for each dimension', () => {
      const signatures: Record<string, string> = {
        '0D': 'identity',
        '1D': 'successor',
        '2D': 'pairing',
        '3D': 'lorentz',
        '4D': 'lorentz',
        '5D': 'consensus',
        '6D': 'intelligence',
        '7D': 'quantum'
      };

      for (const [dimension, expectedSignature] of Object.entries(signatures)) {
        const state = generateMockAutomatonState({
          dimension,
          topologyCount: 1
        });

        const result = service.generateTopologyCanvasL(state);
        const lines = result.split('\n').filter(line => line.trim() && !line.startsWith('@'));
        const entry = JSON.parse(lines[0]);

        expect(entry.frontmatter.bipartite.bqf.signature).toBe(expectedSignature);
      }
    });

    test('should include frontmatter structure', () => {
      const state = generateMockAutomatonState({
        dimension: '0D',
        topologyCount: 1
      });

      const result = service.generateTopologyCanvasL(state);
      const lines = result.split('\n').filter(line => line.trim() && !line.startsWith('@'));
      const entry = JSON.parse(lines[0]);

      expect(entry.frontmatter).toBeDefined();
      expect(entry.frontmatter.bipartite).toBeDefined();
      expect(entry.frontmatter.bipartite.bqf).toBeDefined();
      expect(entry.frontmatter.bipartite.bqf.form).toBeDefined();
    });
  });

  describe('generateSystemCanvasL', () => {
    test('should generate system CanvasL with system partition data', () => {
      const state = generateMockAutomatonState({
        dimension: '0D',
        systemCount: 2
      });

      const result = service.generateSystemCanvasL(state);

      expect(result).toContain('@version 1.0.0');
      expect(result).toContain('@schema metaverse-system');
    });

    test('should include Bipartite-BQF metadata', () => {
      const state = generateMockAutomatonState({
        dimension: '2D',
        systemCount: 1
      });

      const result = service.generateSystemCanvasL(state);
      const lines = result.split('\n').filter(line => line.trim() && !line.startsWith('@'));
      const entry = JSON.parse(lines[0]);

      expect(entry.frontmatter).toBeDefined();
      expect(entry.frontmatter.bipartite).toBeDefined();
      expect(entry.frontmatter.bipartite.partition).toBe('system');
      expect(entry.frontmatter.bipartite.dimension).toBe('2D');
    });

    test('should include BQF coefficients for system partition', () => {
      const dimensions = ['0D', '1D', '2D', '3D'];
      
      for (const dimension of dimensions) {
        const state = generateMockAutomatonState({
          dimension,
          systemCount: 1
        });

        const result = service.generateSystemCanvasL(state);
        const lines = result.split('\n').filter(line => line.trim() && !line.startsWith('@'));
        const entry = JSON.parse(lines[0]);

        const dimNum = parseInt(dimension.replace('D', ''));
        const bqf = entry.frontmatter.bipartite.bqf;
        expect(bqf.coefficients).toEqual([dimNum, 1, 1]);
      }
    });

    test('should include signature generation for each dimension', () => {
      const signatures: Record<string, string> = {
        '0D': 'identity',
        '1D': 'successor',
        '2D': 'pairing',
        '3D': 'lorentz',
        '4D': 'lorentz',
        '5D': 'consensus',
        '6D': 'intelligence',
        '7D': 'quantum'
      };

      for (const [dimension, expectedSignature] of Object.entries(signatures)) {
        const state = generateMockAutomatonState({
          dimension,
          systemCount: 1
        });

        const result = service.generateSystemCanvasL(state);
        const lines = result.split('\n').filter(line => line.trim() && !line.startsWith('@'));
        const entry = JSON.parse(lines[0]);

        expect(entry.frontmatter.bipartite.bqf.signature).toBe(expectedSignature);
      }
    });

    test('should include frontmatter structure', () => {
      const state = generateMockAutomatonState({
        dimension: '0D',
        systemCount: 1
      });

      const result = service.generateSystemCanvasL(state);
      const lines = result.split('\n').filter(line => line.trim() && !line.startsWith('@'));
      const entry = JSON.parse(lines[0]);

      expect(entry.frontmatter).toBeDefined();
      expect(entry.frontmatter.bipartite).toBeDefined();
      expect(entry.frontmatter.bipartite.bqf).toBeDefined();
      expect(entry.frontmatter.bipartite.bqf.form).toBeDefined();
    });
  });

  describe('generateAllFiles', () => {
    test('should generate all four files', () => {
      const state = generateMockAutomatonState({
        id: 'test-automaton',
        dimension: '0D',
        kernelCount: 2,
        topologyCount: 2,
        systemCount: 2,
        seedCount: 1
      });

      const files = service.generateAllFiles(state);

      expect(files.kernel).toBeDefined();
      expect(files.seed).toBeDefined();
      expect(files.topology).toBeDefined();
      expect(files.system).toBeDefined();
    });

    test('should maintain consistency across generated files', () => {
      const state = generateMockAutomatonState({
        id: 'test-automaton',
        dimension: '2D'
      });

      const files = service.generateAllFiles(state);

      // All files should reference the same dimension
      const kernelLines = files.kernel.split('\n').filter(line => line.trim() && !line.startsWith('@'));
      const seedLines = files.seed.split('\n').filter(line => line.trim() && !line.startsWith('@'));
      const topologyLines = files.topology.split('\n').filter(line => line.trim() && !line.startsWith('@'));
      const systemLines = files.system.split('\n').filter(line => line.trim() && !line.startsWith('@'));

      if (kernelLines.length > 0) {
        const kernelEntry = JSON.parse(kernelLines[0]);
        expect(kernelEntry.dimension).toBe('2D');
      }

      if (seedLines.length > 0) {
        const seedEntry = JSON.parse(seedLines[0]);
        expect(seedEntry.dimension).toBe('2D');
      }

      if (topologyLines.length > 0) {
        const topologyEntry = JSON.parse(topologyLines[0]);
        expect(topologyEntry.dimension).toBe('2D');
        expect(topologyEntry.frontmatter.bipartite.dimension).toBe('2D');
      }

      if (systemLines.length > 0) {
        const systemEntry = JSON.parse(systemLines[0]);
        expect(systemEntry.dimension).toBe('2D');
        expect(systemEntry.frontmatter.bipartite.dimension).toBe('2D');
      }
    });
  });

  describe('Helper Methods', () => {
    test('calculateBQF - should calculate for topology partition', () => {
      const dimensions = ['0D', '1D', '2D', '3D', '4D', '5D', '6D', '7D'];
      
      for (const dimension of dimensions) {
        const state = generateMockAutomatonState({
          dimension,
          topologyCount: 1
        });

        const result = service.generateTopologyCanvasL(state);
        const lines = result.split('\n').filter(line => line.trim() && !line.startsWith('@'));
        const entry = JSON.parse(lines[0]);

        const dimNum = parseInt(dimension.replace('D', ''));
        const bqf = entry.frontmatter.bipartite.bqf.coefficients;
        expect(bqf).toEqual([1, 0, dimNum]);
      }
    });

    test('calculateBQF - should calculate for system partition', () => {
      const dimensions = ['0D', '1D', '2D', '3D', '4D', '5D', '6D', '7D'];
      
      for (const dimension of dimensions) {
        const state = generateMockAutomatonState({
          dimension,
          systemCount: 1
        });

        const result = service.generateSystemCanvasL(state);
        const lines = result.split('\n').filter(line => line.trim() && !line.startsWith('@'));
        const entry = JSON.parse(lines[0]);

        const dimNum = parseInt(dimension.replace('D', ''));
        const bqf = entry.frontmatter.bipartite.bqf.coefficients;
        expect(bqf).toEqual([dimNum, 1, 1]);
      }
    });

    test('getSignature - should map signatures for all dimensions', () => {
      const expectedSignatures: Record<string, string> = {
        '0D': 'identity',
        '1D': 'successor',
        '2D': 'pairing',
        '3D': 'lorentz',
        '4D': 'lorentz',
        '5D': 'consensus',
        '6D': 'intelligence',
        '7D': 'quantum'
      };

      for (const [dimension, expectedSignature] of Object.entries(expectedSignatures)) {
        const state = generateMockAutomatonState({
          dimension,
          topologyCount: 1
        });

        const result = service.generateTopologyCanvasL(state);
        const lines = result.split('\n').filter(line => line.trim() && !line.startsWith('@'));
        const entry = JSON.parse(lines[0]);

        expect(entry.frontmatter.bipartite.bqf.signature).toBe(expectedSignature);
      }
    });
  });

  describe('Edge Cases', () => {
    test('should handle empty arrays', () => {
      const state = generateMockAutomatonState({
        dimension: '0D',
        kernelCount: 0,
        topologyCount: 0,
        systemCount: 0,
        seedCount: 0
      });

      const kernel = service.generateKernelCanvasL(state);
      const seed = service.generateSeedCanvasL(state);
      const topology = service.generateTopologyCanvasL(state);
      const system = service.generateSystemCanvasL(state);

      // Should still generate valid files with directives
      expect(kernel).toContain('@version');
      expect(seed).toContain('@version');
      expect(topology).toContain('@version');
      expect(system).toContain('@version');
    });

    test('should handle missing data', () => {
      const state: any = {
        id: 'test',
        dimension: '0D',
        kernel: [],
        topology: [],
        system: [],
        seed: []
      };

      const kernel = service.generateKernelCanvasL(state);
      const seed = service.generateSeedCanvasL(state);

      expect(kernel).toBeDefined();
      expect(seed).toBeDefined();
    });

    test('should handle different dimensions (0D-7D)', () => {
      const allStates = generateAllDimensionStates();

      for (const state of allStates) {
        const files = service.generateAllFiles(state);

        expect(files.kernel).toBeDefined();
        expect(files.seed).toBeDefined();
        expect(files.topology).toBeDefined();
        expect(files.system).toBeDefined();
      }
    });
  });
});

