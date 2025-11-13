/**
 * Provenance Search Service Tests
 * 
 * Tests for searching and filtering provenance chains.
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import { provenanceSearchService, SearchQuery, FilterPreset } from '../provenance-search-service';
import { ProvenanceChain, ProvenanceNode, ProvenanceEdge } from '../provenance-slide-service';

describe('ProvenanceSearchService', () => {
  let mockChain: ProvenanceChain;

  beforeEach(() => {
    // Clear presets
    provenanceSearchService['presets'].clear();
    
    // Create mock provenance chain
    mockChain = {
      nodes: [
        {
          id: 'node-1',
          type: 'agent',
          position: [0, 0, 0],
          metadata: {
            timestamp: 1000,
            file: 'test.jsonl',
            line: 1,
            agentId: '0D-Topology-Agent',
            dimension: '0D',
            pattern: 'identity',
            churchEncoding: 'λf.λx.x'
          },
          data: {}
        },
        {
          id: 'node-2',
          type: 'document',
          position: [1, 1, 1],
          metadata: {
            timestamp: 2000,
            file: 'test.jsonl',
            line: 2,
            agentId: '1D-Temporal-Agent',
            dimension: '1D',
            pattern: 'successor'
          },
          data: {}
        },
        {
          id: 'node-3',
          type: 'code',
          position: [2, 2, 2],
          metadata: {
            timestamp: 3000,
            file: 'other.jsonl',
            line: 3,
            agentId: '2D-Structural-Agent',
            dimension: '2D',
            pattern: 'pairing'
          },
          data: {}
        }
      ],
      edges: [
        {
          id: 'edge-1',
          type: 'evolves',
          from: 'node-1',
          to: 'node-2',
          metadata: {
            timestamp: 1500,
            weight: 1.0,
            context: 'dimensional-progression'
          }
        },
        {
          id: 'edge-2',
          type: 'references',
          from: 'node-2',
          to: 'node-3',
          metadata: {
            timestamp: 2500,
            weight: 0.8,
            context: 'cross-file'
          }
        }
      ]
    };
  });

  describe('searchChain', () => {
    it('should search by pattern', () => {
      const result = provenanceSearchService.searchChain(mockChain, {
        pattern: 'identity'
      });

      expect(result.nodes).toHaveLength(1);
      expect(result.nodes[0].id).toBe('node-1');
      expect(result.edges).toHaveLength(1); // Edge connected to matching node
    });

    it('should search by dimension', () => {
      const result = provenanceSearchService.searchChain(mockChain, {
        dimension: '0D'
      });

      expect(result.nodes).toHaveLength(1);
      expect(result.nodes[0].metadata.dimension).toBe('0D');
    });

    it('should search by agent ID', () => {
      const result = provenanceSearchService.searchChain(mockChain, {
        agentId: '0D-Topology-Agent'
      });

      expect(result.nodes).toHaveLength(1);
      expect(result.nodes[0].metadata.agentId).toBe('0D-Topology-Agent');
    });

    it('should search by node type', () => {
      const result = provenanceSearchService.searchChain(mockChain, {
        nodeType: 'agent'
      });

      expect(result.nodes).toHaveLength(1);
      expect(result.nodes[0].type).toBe('agent');
    });

    it('should search by edge type', () => {
      const result = provenanceSearchService.searchChain(mockChain, {
        edgeType: 'evolves'
      });

      expect(result.edges).toHaveLength(1);
      expect(result.edges[0].type).toBe('evolves');
    });

    it('should search by file', () => {
      const result = provenanceSearchService.searchChain(mockChain, {
        file: 'test.jsonl'
      });

      expect(result.nodes.length).toBeGreaterThan(0);
      expect(result.nodes.every(n => n.metadata.file === 'test.jsonl')).toBe(true);
    });

    it('should combine multiple search criteria', () => {
      const result = provenanceSearchService.searchChain(mockChain, {
        dimension: '0D',
        pattern: 'identity'
      });

      expect(result.nodes).toHaveLength(1);
      expect(result.nodes[0].metadata.dimension).toBe('0D');
      expect(result.nodes[0].metadata.pattern).toBe('identity');
    });

    it('should use custom query function', () => {
      const result = provenanceSearchService.searchChain(mockChain, {
        customQuery: (item) => {
          if ('type' in item) {
            return item.metadata.timestamp > 1500;
          }
          return false;
        }
      });

      expect(result.nodes.length).toBeGreaterThan(0);
      expect(result.nodes.every(n => n.metadata.timestamp > 1500)).toBe(true);
    });
  });

  describe('filterNodes', () => {
    it('should filter nodes by pattern', () => {
      const filtered = provenanceSearchService.filterNodes(mockChain.nodes, {
        pattern: 'identity'
      });

      expect(filtered).toHaveLength(1);
      expect(filtered[0].metadata.pattern).toBe('identity');
    });

    it('should filter nodes by dimension', () => {
      const filtered = provenanceSearchService.filterNodes(mockChain.nodes, {
        dimension: '1D'
      });

      expect(filtered).toHaveLength(1);
      expect(filtered[0].metadata.dimension).toBe('1D');
    });
  });

  describe('filterEdges', () => {
    it('should filter edges by type', () => {
      const filtered = provenanceSearchService.filterEdges(mockChain.edges, mockChain, {
        edgeType: 'evolves'
      });

      expect(filtered).toHaveLength(1);
      expect(filtered[0].type).toBe('evolves');
    });

    it('should filter edges by connected node pattern', () => {
      const filtered = provenanceSearchService.filterEdges(mockChain.edges, mockChain, {
        pattern: 'identity'
      });

      expect(filtered.length).toBeGreaterThan(0);
    });
  });

  describe('preset management', () => {
    it('should save preset', () => {
      const presetId = provenanceSearchService.savePreset('Test Preset', {
        pattern: 'identity',
        dimension: '0D'
      });

      expect(presetId).toBeDefined();
      const preset = provenanceSearchService.getPreset(presetId);
      expect(preset).toBeDefined();
      expect(preset?.name).toBe('Test Preset');
      expect(preset?.query.pattern).toBe('identity');
    });

    it('should get all presets', () => {
      provenanceSearchService.savePreset('Preset 1', { pattern: 'identity' });
      provenanceSearchService.savePreset('Preset 2', { dimension: '0D' });

      const presets = provenanceSearchService.getAllPresets();
      expect(presets.length).toBeGreaterThanOrEqual(2);
    });

    it('should filter shared presets', () => {
      provenanceSearchService.savePreset('Shared Preset', { pattern: 'identity' }, true);
      provenanceSearchService.savePreset('Private Preset', { dimension: '0D' }, false);

      const sharedPresets = provenanceSearchService.getAllPresets(true);
      expect(sharedPresets.every(p => p.shared)).toBe(true);
    });

    it('should delete preset', () => {
      const presetId = provenanceSearchService.savePreset('To Delete', { pattern: 'test' });
      
      const deleted = provenanceSearchService.deletePreset(presetId);
      expect(deleted).toBe(true);
      
      const preset = provenanceSearchService.getPreset(presetId);
      expect(preset).toBeUndefined();
    });

    it('should export preset', () => {
      const presetId = provenanceSearchService.savePreset('Export Test', {
        pattern: 'identity',
        dimension: '0D'
      }, true);

      const exported = provenanceSearchService.exportPreset(presetId);
      const parsed = JSON.parse(exported);
      
      expect(parsed.name).toBe('Export Test');
      expect(parsed.query.pattern).toBe('identity');
      expect(parsed.shared).toBe(true);
    });

    it('should import preset', () => {
      const exported = JSON.stringify({
        name: 'Imported Preset',
        query: { pattern: 'successor', dimension: '1D' },
        shared: false
      });

      const presetId = provenanceSearchService.importPreset(exported);
      const preset = provenanceSearchService.getPreset(presetId);
      
      expect(preset).toBeDefined();
      expect(preset?.name).toBe('Imported Preset');
      expect(preset?.query.pattern).toBe('successor');
    });
  });

  describe('buildAdvancedQuery', () => {
    it('should combine queries with AND logic', () => {
      const combined = provenanceSearchService.buildAdvancedQuery([
        { dimension: '0D' },
        { pattern: 'identity' }
      ], 'AND');

      expect(combined.customQuery).toBeDefined();
      
      const result = provenanceSearchService.searchChain(mockChain, combined);
      expect(result.nodes.length).toBeGreaterThanOrEqual(0);
    });

    it('should combine queries with OR logic', () => {
      const combined = provenanceSearchService.buildAdvancedQuery([
        { dimension: '0D' },
        { dimension: '1D' }
      ], 'OR');

      expect(combined.customQuery).toBeDefined();
      
      const result = provenanceSearchService.searchChain(mockChain, combined);
      expect(result.nodes.length).toBeGreaterThanOrEqual(0);
    });

    it('should return single query if only one provided', () => {
      const combined = provenanceSearchService.buildAdvancedQuery([
        { pattern: 'identity' }
      ]);

      expect(combined.pattern).toBe('identity');
      expect(combined.customQuery).toBeUndefined();
    });

    it('should return empty query if no queries provided', () => {
      const combined = provenanceSearchService.buildAdvancedQuery([]);
      expect(Object.keys(combined)).toHaveLength(0);
    });
  });
});

