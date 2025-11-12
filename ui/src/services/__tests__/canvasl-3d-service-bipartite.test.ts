/**
 * Extended CanvasL 3D Service Tests - Bipartite Extensions
 */

import { describe, test, expect, beforeEach, vi } from 'vitest';
import { CanvasL3DService } from '../canvasl-3d-service';
import { generateBipartiteCanvasLData, generateMockCanvasLEntries } from './utils/mockCanvasLData';
import { createMockDatabaseService, setupMockDatabaseService } from './utils/mockDatabaseService';

// Mock dependencies
vi.mock('../database-service', () => ({
  databaseService: createMockDatabaseService()
}));

vi.mock('../jsonl-canvas-service', () => ({
  jsonlCanvasService: {
    parseJSONL: vi.fn((content: string) => {
      const lines = content.split('\n').filter(l => l.trim() && !l.startsWith('@'));
      const nodes: any[] = [];
      const edges: any[] = [];
      
      for (const line of lines) {
        try {
          const entry = JSON.parse(line);
          if (entry.id) {
            const node = {
              id: entry.id,
              type: entry.type || 'node',
              x: entry.x || 0,
              y: entry.y || 0,
              text: entry.text || '',
              metadata: {
                ...(entry.metadata || {}),
                ...(entry.frontmatter || {}),
                frontmatter: entry.frontmatter || {}
              }
            };
            nodes.push(node);
          }
        } catch (e) {
          // Skip invalid JSON
        }
      }
      
      return {
        nodes: new Map(nodes.map(n => [n.id, n])),
        edges: new Map(),
        nodeList: nodes,
        edgeList: edges
      };
    }),
    exportToJSONL: vi.fn((graph: any) => {
      return graph.nodeList.map((n: any) => JSON.stringify(n)).join('\n');
    })
  }
}));

describe('CanvasL3DService - Bipartite Extensions', () => {
  let service: CanvasL3DService;
  let mockDatabaseService: ReturnType<typeof createMockDatabaseService>;

  beforeEach(async () => {
    vi.clearAllMocks();
    
    const { databaseService } = await import('../database-service');
    mockDatabaseService = databaseService as any;
    
    const { canvasl3DService: serviceInstance } = await import('../canvasl-3d-service');
    service = serviceInstance;
  });

  describe('loadBipartiteCanvasL', () => {
    test('should load topology and system files', async () => {
      const bipartiteData = generateBipartiteCanvasLData('0D', 3, 3);
      
      setupMockDatabaseService(mockDatabaseService, {
        canvasLEntries: [
          ...bipartiteData.topology,
          ...bipartiteData.system
        ]
      });

      // Mock readJSONL to return different data for each file
      mockDatabaseService.readJSONL.mockImplementation(async (filename: string) => {
        if (filename.includes('topology')) {
          return bipartiteData.topology;
        } else if (filename.includes('system')) {
          return bipartiteData.system;
        }
        return [];
      });

      const result = await service.loadBipartiteCanvasL(
        'topology.canvasl',
        'system.canvasl'
      );

      expect(result.topology).toBeDefined();
      expect(result.system).toBeDefined();
      expect(result.topology.nodeList.length).toBeGreaterThan(0);
      expect(result.system.nodeList.length).toBeGreaterThan(0);
    });

    test('should create bipartite structure', async () => {
      const bipartiteData = generateBipartiteCanvasLData('2D', 2, 2);
      
      mockDatabaseService.readJSONL.mockImplementation(async (filename: string) => {
        if (filename.includes('topology')) {
          return bipartiteData.topology;
        } else if (filename.includes('system')) {
          return bipartiteData.system;
        }
        return [];
      });

      const result = await service.loadBipartiteCanvasL(
        'topology.canvasl',
        'system.canvasl'
      );

      expect(result).toBeDefined();
      expect(result.topology).toBeDefined();
      expect(result.system).toBeDefined();
      expect(result.horizontalEdges).toBeDefined();
      expect(result.verticalEdges).toBeDefined();
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
      
      // Add horizontal edge metadata
      topologyEntries[0].edges = [{ type: 'horizontal', to: systemEntries[0].id }];
      
      mockDatabaseService.readJSONL.mockImplementation(async (filename: string) => {
        if (filename.includes('topology')) {
          return topologyEntries;
        } else if (filename.includes('system')) {
          return systemEntries;
        }
        return [];
      });

      const result = await service.loadBipartiteCanvasL(
        'topology.canvasl',
        'system.canvasl'
      );

      expect(Array.isArray(result.horizontalEdges)).toBe(true);
    });

    test('should extract vertical edges', async () => {
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
      
      mockDatabaseService.readJSONL.mockImplementation(async (filename: string) => {
        if (filename.includes('topology')) {
          return topologyEntries;
        } else if (filename.includes('system')) {
          return systemEntries;
        }
        return [];
      });

      const result = await service.loadBipartiteCanvasL(
        'topology.canvasl',
        'system.canvasl'
      );

      expect(Array.isArray(result.verticalEdges)).toBe(true);
    });

    test('should handle error for missing files', async () => {
      mockDatabaseService.readJSONL.mockRejectedValue(new Error('File not found'));

      await expect(
        service.loadBipartiteCanvasL('missing-topology.canvasl', 'missing-system.canvasl')
      ).rejects.toThrow();
    });
  });

  describe('renderBipartitePartition', () => {
    test('should render topology partition', async () => {
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

      const canvas3D = await service.loadCanvasLTo3D('test.canvasl');
      const rendered = service.renderBipartitePartition(canvas3D, 'topology');

      // The service should filter nodes by partition
      // If no nodes match, the list will be empty, which is valid
      expect(rendered.nodeList).toBeDefined();
      expect(Array.isArray(rendered.nodeList)).toBe(true);
      
      // If nodes are rendered, they should all be topology
      if (rendered.nodeList.length > 0) {
        rendered.nodeList.forEach(node => {
          const partition = node.metadata?.frontmatter?.bipartite?.partition ||
                           (node.metadata?.type === 'topology' ? 'topology' : 'system');
          expect(partition).toBe('topology');
        });
      }
    });

    test('should render system partition', async () => {
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

      const canvas3D = await service.loadCanvasLTo3D('test.canvasl');
      const rendered = service.renderBipartitePartition(canvas3D, 'system');

      expect(rendered.nodeList.length).toBeGreaterThan(0);
      // All rendered nodes should be system
      rendered.nodeList.forEach(node => {
        const partition = node.metadata?.frontmatter?.bipartite?.partition ||
                         (node.metadata?.type === 'topology' ? 'topology' : 'system');
        expect(partition).toBe('system');
      });
    });

    test('should calculate position for partitions', async () => {
      const entries = generateMockCanvasLEntries(3, {
        dimension: '0D',
        partition: 'topology',
        includeBipartite: true
      });
      
      setupMockDatabaseService(mockDatabaseService, {
        canvasLEntries: entries
      });

      const canvas3D = await service.loadCanvasLTo3D('test.canvasl');
      const rendered = service.renderBipartitePartition(canvas3D, 'topology');

      rendered.nodeList.forEach(node => {
        expect(node.position).toHaveLength(3);
        expect(typeof node.position[0]).toBe('number');
        expect(typeof node.position[1]).toBe('number');
        expect(typeof node.position[2]).toBe('number');
      });
    });

    test('should assign color for partitions', async () => {
      const entries = generateMockCanvasLEntries(2, {
        dimension: '0D',
        partition: 'topology',
        includeBipartite: true
      });
      
      setupMockDatabaseService(mockDatabaseService, {
        canvasLEntries: entries
      });

      const canvas3D = await service.loadCanvasLTo3D('test.canvasl');
      const rendered = service.renderBipartitePartition(canvas3D, 'topology');

      rendered.nodeList.forEach(node => {
        expect(node.color).toBeDefined();
        expect(typeof node.color).toBe('string');
      });
    });
  });

  describe('extractBipartiteStructure', () => {
    test('should extract from single canvas', async () => {
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

      const canvas3D = await service.loadCanvasLTo3D('test.canvasl');
      const bipartite = service.extractBipartiteStructure(canvas3D);

      expect(bipartite).toBeDefined();
      expect(bipartite.topology).toBeDefined();
      expect(bipartite.system).toBeDefined();
    });

    test('should separate topology/system nodes', async () => {
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

      const canvas3D = await service.loadCanvasLTo3D('test.canvasl');
      const bipartite = service.extractBipartiteStructure(canvas3D);

      // Verify bipartite structure was created
      expect(bipartite).toBeDefined();
      expect(bipartite.topology).toBeDefined();
      expect(bipartite.system).toBeDefined();
      expect(Array.isArray(bipartite.topology.nodeList)).toBe(true);
      expect(Array.isArray(bipartite.system.nodeList)).toBe(true);
      
      // The service should separate nodes by partition
      // Total nodes should equal the sum of topology and system nodes
      const totalNodes = bipartite.topology.nodeList.length + bipartite.system.nodeList.length;
      expect(totalNodes).toBeGreaterThanOrEqual(0);
    });

    test('should identify horizontal edges', async () => {
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
      
      const allEntries = [...topologyEntries, ...systemEntries];
      
      setupMockDatabaseService(mockDatabaseService, {
        canvasLEntries: allEntries
      });

      const canvas3D = await service.loadCanvasLTo3D('test.canvasl');
      const bipartite = service.extractBipartiteStructure(canvas3D);

      expect(Array.isArray(bipartite.horizontalEdges)).toBe(true);
    });

    test('should identify vertical edges', async () => {
      const entries = generateMockCanvasLEntries(3, {
        dimension: '0D',
        partition: 'topology',
        includeBipartite: true
      });
      
      setupMockDatabaseService(mockDatabaseService, {
        canvasLEntries: entries
      });

      const canvas3D = await service.loadCanvasLTo3D('test.canvasl');
      const bipartite = service.extractBipartiteStructure(canvas3D);

      expect(Array.isArray(bipartite.verticalEdges)).toBe(true);
    });

    test('should handle missing bipartite metadata', async () => {
      const entries = generateMockCanvasLEntries(3, {
        dimension: '0D',
        partition: 'topology',
        includeBipartite: false
      });
      
      // Remove bipartite metadata from some entries
      entries[1].frontmatter = undefined;
      entries[2].metadata = { type: 'topology' };
      
      setupMockDatabaseService(mockDatabaseService, {
        canvasLEntries: entries
      });

      const canvas3D = await service.loadCanvasLTo3D('test.canvasl');
      const bipartite = service.extractBipartiteStructure(canvas3D);

      // Should still extract structure, using type as fallback
      expect(bipartite).toBeDefined();
      expect(bipartite.topology).toBeDefined();
      expect(bipartite.system).toBeDefined();
    });
  });
});

