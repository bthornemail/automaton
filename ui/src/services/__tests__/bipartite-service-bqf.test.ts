/**
 * Extended Bipartite Service Tests - BQF Encoding Extensions
 */

import { describe, test, expect, beforeEach, vi } from 'vitest';
import { BipartiteService } from '../bipartite-service';
import { generateBipartiteCanvasLData, generateMockCanvasLEntries } from './utils/mockCanvasLData';
import { createMockDatabaseService, setupMockDatabaseService } from './utils/mockDatabaseService';

// Mock dependencies
vi.mock('../database-service', () => ({
  databaseService: createMockDatabaseService()
}));

vi.mock('../markdown-service', () => ({
  markdownService: {
    loadMarkdown: vi.fn().mockResolvedValue(null),
    saveMarkdown: vi.fn().mockResolvedValue(undefined),
    linkToJSONL: vi.fn().mockResolvedValue(undefined)
  }
}));

vi.mock('../utils/front-matter-parser', () => ({
  frontMatterParser: {
    parse: vi.fn(),
    stringify: vi.fn()
  }
}));

describe('BipartiteService - BQF Encoding Extensions', () => {
  let service: BipartiteService;
  let mockDatabaseService: ReturnType<typeof createMockDatabaseService>;

  beforeEach(async () => {
    vi.clearAllMocks();
    
    const { databaseService } = await import('../database-service');
    mockDatabaseService = databaseService as any;
    
    const { bipartiteService: serviceInstance } = await import('../bipartite-service');
    service = serviceInstance;
  });

  describe('encodeBQF', () => {
    test('should encode for topology partition (all dimensions)', () => {
      const dimensions = ['0D', '1D', '2D', '3D', '4D', '5D', '6D', '7D'];
      
      for (const dimension of dimensions) {
        const result = service.encodeBQF(dimension, 'topology');
        
        expect(result).toBeDefined();
        expect(result.a).toBe(1);
        expect(result.b).toBe(0);
        
        const dimNum = parseInt(dimension.replace('D', ''));
        expect(result.c).toBe(dimNum);
      }
    });

    test('should encode for system partition (all dimensions)', () => {
      const dimensions = ['0D', '1D', '2D', '3D', '4D', '5D', '6D', '7D'];
      
      for (const dimension of dimensions) {
        const result = service.encodeBQF(dimension, 'system');
        
        expect(result).toBeDefined();
        
        const dimNum = parseInt(dimension.replace('D', ''));
        expect(result.a).toBe(dimNum);
        expect(result.b).toBe(1);
        expect(result.c).toBe(1);
      }
    });

    test('should calculate coefficients correctly', () => {
      // Test specific dimensions
      const testCases = [
        { dimension: '0D', partition: 'topology' as const, expected: { a: 1, b: 0, c: 0 } },
        { dimension: '2D', partition: 'topology' as const, expected: { a: 1, b: 0, c: 2 } },
        { dimension: '7D', partition: 'topology' as const, expected: { a: 1, b: 0, c: 7 } },
        { dimension: '0D', partition: 'system' as const, expected: { a: 0, b: 1, c: 1 } },
        { dimension: '2D', partition: 'system' as const, expected: { a: 2, b: 1, c: 1 } },
        { dimension: '7D', partition: 'system' as const, expected: { a: 7, b: 1, c: 1 } }
      ];
      
      for (const testCase of testCases) {
        const result = service.encodeBQF(testCase.dimension, testCase.partition);
        expect(result).toEqual(testCase.expected);
      }
    });

    test('should handle invalid dimension strings', () => {
      const invalidDimensions = ['', 'D', 'XD', 'invalid'];
      
      for (const dimension of invalidDimensions) {
        const result = service.encodeBQF(dimension, 'topology');
        
        // Should default to 0D behavior (parseInt returns NaN, which becomes 0)
        expect(result).toBeDefined();
        expect(result.a).toBe(1);
        expect(result.b).toBe(0);
        expect(result.c).toBe(0);
      }
      
      // Test '10D' - this actually parses correctly as 10
      const result10D = service.encodeBQF('10D', 'topology');
      expect(result10D).toBeDefined();
      expect(result10D.a).toBe(1);
      expect(result10D.b).toBe(0);
      expect(result10D.c).toBe(10); // '10D' parses to 10, not 0
    });
  });

  describe('buildBipartiteGraphFromCanvasL', () => {
    test('should build graph from CanvasL file', async () => {
      const bipartiteData = generateBipartiteCanvasLData('0D', 3, 3);
      const allEntries = [...bipartiteData.topology, ...bipartiteData.system];
      
      setupMockDatabaseService(mockDatabaseService, {
        canvasLEntries: allEntries
      });

      const graph = await service.buildBipartiteGraphFromCanvasL('test.canvasl');

      expect(graph).toBeDefined();
      expect(graph.topology).toBeDefined();
      expect(graph.system).toBeDefined();
      expect(graph.horizontalEdges).toBeDefined();
      expect(graph.verticalEdges).toBeDefined();
    });

    test('should extract topology partition', async () => {
      const topologyEntries = generateMockCanvasLEntries(3, {
        dimension: '0D',
        partition: 'topology',
        includeBipartite: true
      });
      
      const systemEntries = generateMockCanvasLEntries(2, {
        dimension: '0D',
        partition: 'system',
        includeBipartite: true
      });
      
      const allEntries = [...topologyEntries, ...systemEntries];
      
      setupMockDatabaseService(mockDatabaseService, {
        canvasLEntries: allEntries
      });

      const graph = await service.buildBipartiteGraphFromCanvasL('test.canvasl');

      expect(graph.topology.nodes.length).toBe(3);
      graph.topology.nodes.forEach(node => {
        const partition = node.frontmatter?.bipartite?.partition || 
                         (node.type === 'topology' ? 'topology' : 'system');
        expect(partition).toBe('topology');
      });
    });

    test('should extract system partition', async () => {
      const topologyEntries = generateMockCanvasLEntries(2, {
        dimension: '0D',
        partition: 'topology',
        includeBipartite: true
      });
      
      const systemEntries = generateMockCanvasLEntries(3, {
        dimension: '0D',
        partition: 'system',
        includeBipartite: true
      });
      
      const allEntries = [...topologyEntries, ...systemEntries];
      
      setupMockDatabaseService(mockDatabaseService, {
        canvasLEntries: allEntries
      });

      const graph = await service.buildBipartiteGraphFromCanvasL('test.canvasl');

      expect(graph.system.nodes.length).toBe(3);
      graph.system.nodes.forEach(node => {
        const partition = node.frontmatter?.bipartite?.partition || 
                         (node.type === 'topology' ? 'topology' : 'system');
        expect(partition).toBe('system');
      });
    });

    test('should extract horizontal edges', async () => {
      const topologyEntries = generateMockCanvasLEntries(2, {
        dimension: '0D',
        partition: 'topology',
        includeBipartite: true
      });
      
      const systemEntries = generateMockCanvasLEntries(2, {
        dimension: '0D',
        partition: 'system',
        includeBipartite: true
      });
      
      // Add horizontal edge entries
      const horizontalEdge = {
        id: 'h-edge-1',
        type: 'horizontal',
        from: topologyEntries[0].id,
        to: systemEntries[0].id
      };
      
      const allEntries = [...topologyEntries, ...systemEntries, horizontalEdge];
      
      setupMockDatabaseService(mockDatabaseService, {
        canvasLEntries: allEntries
      });

      const graph = await service.buildBipartiteGraphFromCanvasL('test.canvasl');

      expect(graph.horizontalEdges.length).toBeGreaterThan(0);
    });

    test('should extract vertical edges', async () => {
      const entries = generateMockCanvasLEntries(3, {
        dimension: '0D',
        partition: 'topology',
        includeBipartite: true
      });
      
      // Add vertical edge entry
      const verticalEdge = {
        id: 'v-edge-1',
        type: 'vertical',
        from: entries[0].id,
        to: entries[1].id
      };
      
      const allEntries = [...entries, verticalEdge];
      
      setupMockDatabaseService(mockDatabaseService, {
        canvasLEntries: allEntries
      });

      const graph = await service.buildBipartiteGraphFromCanvasL('test.canvasl');

      expect(graph.verticalEdges.length).toBeGreaterThan(0);
    });

    test('should handle missing bipartite metadata', async () => {
      const entries = generateMockCanvasLEntries(3, {
        dimension: '0D',
        partition: 'topology',
        includeBipartite: false
      });
      
      // Set type to topology for fallback
      entries.forEach(entry => {
        entry.type = 'topology';
      });
      
      setupMockDatabaseService(mockDatabaseService, {
        canvasLEntries: entries
      });

      const graph = await service.buildBipartiteGraphFromCanvasL('test.canvasl');

      expect(graph).toBeDefined();
      expect(graph.topology.nodes.length).toBe(3);
    });
  });

  describe('validateBipartiteBQF', () => {
    test('should validate valid bipartite structure', async () => {
      const topologyEntries = generateMockCanvasLEntries(3, {
        dimension: '0D',
        partition: 'topology',
        includeBipartite: true
      });
      
      const systemEntries = generateMockCanvasLEntries(2, {
        dimension: '0D',
        partition: 'system',
        includeBipartite: true
      });
      
      const graph = {
        topology: { nodes: topologyEntries, edges: [] },
        system: { nodes: systemEntries, edges: [] },
        horizontalEdges: [],
        verticalEdges: []
      };

      const result = await service.validateBipartiteBQF(graph);

      expect(result.valid).toBe(true);
      expect(result.errors.length).toBe(0);
    });

    test('should validate with missing BQF metadata', async () => {
      const topologyEntries = generateMockCanvasLEntries(2, {
        dimension: '0D',
        partition: 'topology',
        includeBipartite: false
      });
      
      // Remove bipartite metadata
      topologyEntries[0].frontmatter = undefined;
      
      const graph = {
        topology: { nodes: topologyEntries, edges: [] },
        system: { nodes: [], edges: [] },
        horizontalEdges: [],
        verticalEdges: []
      };

      const result = await service.validateBipartiteBQF(graph);

      expect(result.valid).toBe(false);
      expect(result.errors.length).toBeGreaterThan(0);
      expect(result.errors.some(e => e.includes('missing bipartite partition'))).toBe(true);
    });

    test('should validate with invalid coefficients', async () => {
      const topologyEntries = generateMockCanvasLEntries(1, {
        dimension: '0D',
        partition: 'topology',
        includeBipartite: true
      });
      
      // Set invalid partition value
      topologyEntries[0].frontmatter!.bipartite!.partition = 'system';
      
      const graph = {
        topology: { nodes: topologyEntries, edges: [] },
        system: { nodes: [], edges: [] },
        horizontalEdges: [],
        verticalEdges: []
      };

      const result = await service.validateBipartiteBQF(graph);

      expect(result.valid).toBe(false);
      expect(result.errors.length).toBeGreaterThan(0);
      expect(result.errors.some(e => e.includes('incorrect partition value'))).toBe(true);
    });

    test('should validate with missing partitions', async () => {
      const graph = {
        topology: { nodes: [], edges: [] },
        system: { nodes: [], edges: [] },
        horizontalEdges: [],
        verticalEdges: []
      };

      const result = await service.validateBipartiteBQF(graph);

      // Empty partitions should still be valid
      expect(result.valid).toBe(true);
    });

    test('should generate error messages', async () => {
      const topologyEntries = generateMockCanvasLEntries(1, {
        dimension: '0D',
        partition: 'topology',
        includeBipartite: false
      });
      
      const graph = {
        topology: { nodes: topologyEntries, edges: [] },
        system: { nodes: [], edges: [] },
        horizontalEdges: [],
        verticalEdges: []
      };

      const result = await service.validateBipartiteBQF(graph);

      expect(result.errors.length).toBeGreaterThan(0);
      result.errors.forEach(error => {
        expect(typeof error).toBe('string');
        expect(error.length).toBeGreaterThan(0);
      });
    });

    test('should validate horizontal edges connect topology ↔ system', async () => {
      const topologyEntries = generateMockCanvasLEntries(2, {
        dimension: '0D',
        partition: 'topology',
        includeBipartite: true
      });
      
      const systemEntries = generateMockCanvasLEntries(2, {
        dimension: '0D',
        partition: 'system',
        includeBipartite: true
      });
      
      // Add horizontal edge connecting topology to system (valid)
      const validHorizontalEdge = {
        id: 'h-valid',
        from: topologyEntries[0].id,
        to: systemEntries[0].id,
        fromNode: topologyEntries[0].id,
        toNode: systemEntries[0].id
      };
      
      // Add horizontal edge connecting topology to topology (invalid)
      const invalidHorizontalEdge = {
        id: 'h-invalid',
        from: topologyEntries[0].id,
        to: topologyEntries[1].id,
        fromNode: topologyEntries[0].id,
        toNode: topologyEntries[1].id
      };
      
      const graph = {
        topology: { nodes: topologyEntries, edges: [] },
        system: { nodes: systemEntries, edges: [] },
        horizontalEdges: [validHorizontalEdge, invalidHorizontalEdge],
        verticalEdges: []
      };

      const result = await service.validateBipartiteBQF(graph);

      expect(result.valid).toBe(false);
      expect(result.errors.some(e => e.includes('same partition'))).toBe(true);
    });
  });

  describe('syncBipartiteFrontmatter', () => {
    test('should synchronize frontmatter', async () => {
      const topologyEntries = generateMockCanvasLEntries(2, {
        dimension: '0D',
        partition: 'topology',
        includeBipartite: true
      });
      
      const systemEntries = generateMockCanvasLEntries(2, {
        dimension: '0D',
        partition: 'system',
        includeBipartite: true
      });
      
      const graph = {
        topology: { nodes: topologyEntries, edges: [] },
        system: { nodes: systemEntries, edges: [] },
        horizontalEdges: [],
        verticalEdges: []
      };

      const { markdownService } = await import('../markdown-service');
      markdownService.loadMarkdown = vi.fn().mockResolvedValue({
        content: '',
        frontMatter: {},
        jsonlReferences: [],
        lastModified: Date.now()
      });
      markdownService.saveMarkdown = vi.fn().mockResolvedValue(undefined);

      const result = await service.syncBipartiteFrontmatter(graph, './docs');

      expect(result).toBeDefined();
      expect(result.updatedFiles).toBeDefined();
      expect(Array.isArray(result.updatedFiles)).toBe(true);
    });

    test('should track file updates', async () => {
      const entries = generateMockCanvasLEntries(1, {
        dimension: '0D',
        partition: 'topology',
        includeBipartite: true
      });
      
      // Add filePath to entries so the service can find them
      entries[0].filePath = './docs/test.md';
      
      const graph = {
        topology: { nodes: entries, edges: [] },
        system: { nodes: [], edges: [] },
        horizontalEdges: [],
        verticalEdges: []
      };

      const { markdownService } = await import('../markdown-service');
      markdownService.loadMarkdown = vi.fn().mockResolvedValue({
        content: '',
        frontMatter: {},
        jsonlReferences: [],
        lastModified: Date.now()
      });
      markdownService.saveMarkdown = vi.fn().mockResolvedValue(undefined);

      const result = await service.syncBipartiteFrontmatter(graph, './docs');

      expect(result).toBeDefined();
      expect(result.updatedFiles).toBeDefined();
      expect(Array.isArray(result.updatedFiles)).toBe(true);
    });

    test('should handle existing frontmatter', async () => {
      const entries = generateMockCanvasLEntries(1, {
        dimension: '0D',
        partition: 'topology',
        includeBipartite: true
      });
      
      const graph = {
        topology: { nodes: entries, edges: [] },
        system: { nodes: [], edges: [] },
        horizontalEdges: [],
        verticalEdges: []
      };

      const { markdownService } = await import('../markdown-service');
      markdownService.loadMarkdown = vi.fn().mockResolvedValue({
        content: '',
        frontMatter: {
          bipartite: {
            partition: 'topology',
            dimension: '0D'
          }
        },
        jsonlReferences: [],
        lastModified: Date.now()
      });
      markdownService.saveMarkdown = vi.fn().mockResolvedValue(undefined);

      const result = await service.syncBipartiteFrontmatter(graph, './docs');

      expect(result).toBeDefined();
    });

    test('should handle missing frontmatter', async () => {
      const entries = generateMockCanvasLEntries(1, {
        dimension: '0D',
        partition: 'topology',
        includeBipartite: true
      });
      
      const graph = {
        topology: { nodes: entries, edges: [] },
        system: { nodes: [], edges: [] },
        horizontalEdges: [],
        verticalEdges: []
      };

      const { markdownService } = await import('../markdown-service');
      markdownService.loadMarkdown = vi.fn().mockResolvedValue({
        content: '',
        frontMatter: {},
        jsonlReferences: [],
        lastModified: Date.now()
      });
      markdownService.saveMarkdown = vi.fn().mockResolvedValue(undefined);

      const result = await service.syncBipartiteFrontmatter(graph, './docs');

      expect(result).toBeDefined();
    });

    test('should handle error during synchronization', async () => {
      const entries = generateMockCanvasLEntries(1, {
        dimension: '0D',
        partition: 'topology',
        includeBipartite: true
      });
      
      const graph = {
        topology: { nodes: entries, edges: [] },
        system: { nodes: [], edges: [] },
        horizontalEdges: [],
        verticalEdges: []
      };

      const { markdownService } = await import('../markdown-service');
      markdownService.loadMarkdown = vi.fn().mockRejectedValue(new Error('File not found'));

      // Should not throw, but handle error gracefully
      const result = await service.syncBipartiteFrontmatter(graph, './docs');

      expect(result).toBeDefined();
      expect(result.updatedFiles).toBeDefined();
    });
  });
});

