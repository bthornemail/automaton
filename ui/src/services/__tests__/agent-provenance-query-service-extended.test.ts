/**
 * Extended Agent Provenance Query Service Tests
 * Tests for new methods added in Phase 1
 */

import { describe, test, expect, beforeEach, vi } from 'vitest';
import { agentProvenanceQueryService, QueryType } from '../agent-provenance-query-service';
import { generateMockCanvasLEntries } from './utils/mockCanvasLData';
import { createMockDatabaseService, setupMockDatabaseService } from './utils/mockDatabaseService';
import { createMockMetaLogApiService, setupMockMetaLogApiService } from './utils/mockMetaLogApiService';

// Mock dependencies
vi.mock('../database-service', () => ({
  databaseService: createMockDatabaseService()
}));

vi.mock('../meta-log-api-service', () => ({
  metaLogApiService: createMockMetaLogApiService()
}));

vi.mock('../agent-history-logging-service', () => ({
  agentHistoryLoggingService: {
    getHistory: vi.fn().mockResolvedValue([]),
    getAllAgentIds: vi.fn().mockResolvedValue([]),
    getAgentLogFile: vi.fn().mockReturnValue('logs/agent.log')
  }
}));

describe('AgentProvenanceQueryService - Extended Methods', () => {
  let mockDatabaseService: ReturnType<typeof createMockDatabaseService>;
  let mockMetaLogApiService: ReturnType<typeof createMockMetaLogApiService>;

  beforeEach(async () => {
    vi.clearAllMocks();
    
    const { databaseService } = await import('../database-service');
    const { metaLogApiService } = await import('../meta-log-api-service');
    
    mockDatabaseService = databaseService as any;
    mockMetaLogApiService = metaLogApiService as any;
    
    setupMockDatabaseService(mockDatabaseService);
    setupMockMetaLogApiService(mockMetaLogApiService);
  });

  describe('queryCanvasLFile', () => {
    test('should query with Prolog query type', async () => {
      const canvasLEntries = generateMockCanvasLEntries(3, {
        includeSelfReference: true,
        includeProvenanceHistory: true
      });
      
      setupMockDatabaseService(mockDatabaseService, {
        canvasLEntries
      });
      
      setupMockMetaLogApiService(mockMetaLogApiService, {
        available: true,
        prologResults: {
          bindings: [
            { Agent: 'test-agent', Document: 'test.jsonl', Timestamp: Date.now() }
          ]
        }
      });

      const result = await agentProvenanceQueryService.queryCanvasLFile(
        'test.canvasl',
        'consumes(Agent, Document, Timestamp).',
        QueryType.PROLOG
      );

      expect(mockMetaLogApiService.loadCanvas).toHaveBeenCalledWith('test.canvasl');
      expect(mockMetaLogApiService.prologQuery).toHaveBeenCalled();
      expect(result).toBeDefined();
    });

    test('should query with DataLog query type', async () => {
      const canvasLEntries = generateMockCanvasLEntries(3, {
        includeSelfReference: true
      });
      
      setupMockDatabaseService(mockDatabaseService, {
        canvasLEntries
      });
      
      setupMockMetaLogApiService(mockMetaLogApiService, {
        available: true,
        datalogResults: {
          facts: [
            { predicate: 'consumes', args: ['agent', 'doc.jsonl', Date.now()] }
          ]
        }
      });

      const result = await agentProvenanceQueryService.queryCanvasLFile(
        'test.canvasl',
        'consumes(Agent, Document, Timestamp)',
        QueryType.DATALOG
      );

      expect(mockMetaLogApiService.loadCanvas).toHaveBeenCalledWith('test.canvasl');
      expect(mockMetaLogApiService.datalogQuery).toHaveBeenCalled();
      expect(result).toBeDefined();
    });

    test('should query with SPARQL query type', async () => {
      const canvasLEntries = generateMockCanvasLEntries(3, {
        includeSelfReference: true
      });
      
      setupMockDatabaseService(mockDatabaseService, {
        canvasLEntries
      });
      
      setupMockMetaLogApiService(mockMetaLogApiService, {
        available: true,
        sparqlResults: {
          results: {
            bindings: [
              {
                action: { value: 'http://example.org/action1', type: 'uri' },
                target: { value: 'http://example.org/target1', type: 'uri' },
                timestamp: { value: Date.now().toString(), type: 'literal' }
              }
            ]
          }
        }
      });

      const query = `
        PREFIX prov: <http://www.w3.org/ns/prov#>
        SELECT ?action ?target ?timestamp
        WHERE {
          ?action prov:used ?target .
          ?action prov:atTime ?timestamp .
        }
      `;

      const result = await agentProvenanceQueryService.queryCanvasLFile(
        'test.canvasl',
        query,
        QueryType.SPARQL
      );

      expect(mockMetaLogApiService.loadCanvas).toHaveBeenCalledWith('test.canvasl');
      expect(mockMetaLogApiService.sparqlQuery).toHaveBeenCalled();
      expect(result).toBeDefined();
    });

    test('should fallback to provenance extraction when Meta-Log API unavailable', async () => {
      const canvasLEntries = generateMockCanvasLEntries(3, {
        includeSelfReference: true,
        includeProvenanceHistory: true
      });
      
      setupMockDatabaseService(mockDatabaseService, {
        canvasLEntries
      });
      
      setupMockMetaLogApiService(mockMetaLogApiService, {
        available: false
      });

      const result = await agentProvenanceQueryService.queryCanvasLFile(
        'test.canvasl',
        'test query',
        QueryType.PROLOG
      );

      expect(result).toBeDefined();
      expect(result.provenance).toBeDefined();
      expect(Array.isArray(result.provenance)).toBe(true);
    });

    test('should handle error when Meta-Log API fails', async () => {
      const canvasLEntries = generateMockCanvasLEntries(3, {
        includeSelfReference: true,
        includeProvenanceHistory: true
      });
      
      setupMockDatabaseService(mockDatabaseService, {
        canvasLEntries
      });
      
      setupMockMetaLogApiService(mockMetaLogApiService, {
        available: true
      });
      
      mockMetaLogApiService.loadCanvas.mockRejectedValue(new Error('API Error'));

      const result = await agentProvenanceQueryService.queryCanvasLFile(
        'test.canvasl',
        'test query',
        QueryType.PROLOG
      );

      // Should fallback to provenance extraction
      expect(result).toBeDefined();
      expect(result.provenance).toBeDefined();
    });
  });

  describe('queryFederatedProvenance', () => {
    test('should query federated provenance across multiple files', async () => {
      const canvasLEntries1 = generateMockCanvasLEntries(2, {
        includeSelfReference: true,
        includeProvenanceHistory: true
      });
      
      const canvasLEntries2 = generateMockCanvasLEntries(2, {
        includeSelfReference: true,
        includeProvenanceHistory: true
      });
      
      setupMockDatabaseService(mockDatabaseService, {
        canvasLEntries: [...canvasLEntries1, ...canvasLEntries2]
      });
      
      setupMockMetaLogApiService(mockMetaLogApiService, {
        available: true,
        sparqlResults: {
          results: {
            bindings: [
              {
                provenance: { value: 'http://example.org/prov1', type: 'uri' }
              }
            ]
          }
        }
      });

      const query = `
        SELECT ?provenance WHERE {
          ?entry prov:wasDerivedFrom ?provenance .
        }
      `;

      const result = await agentProvenanceQueryService.queryFederatedProvenance({
        files: ['file1.canvasl', 'file2.canvasl'],
        query,
        queryType: QueryType.SPARQL
      });

      expect(mockMetaLogApiService.loadCanvas).toHaveBeenCalledTimes(2);
      expect(mockMetaLogApiService.sparqlQuery).toHaveBeenCalled();
      expect(result).toBeDefined();
    });

    test('should query with Prolog query type', async () => {
      const canvasLEntries = generateMockCanvasLEntries(2, {
        includeSelfReference: true
      });
      
      setupMockDatabaseService(mockDatabaseService, {
        canvasLEntries
      });
      
      setupMockMetaLogApiService(mockMetaLogApiService, {
        available: true,
        prologResults: {
          bindings: [
            { Agent: 'agent1', Document: 'doc1.jsonl' }
          ]
        }
      });

      const result = await agentProvenanceQueryService.queryFederatedProvenance({
        files: ['file1.canvasl'],
        query: 'consumes(Agent, Document).',
        queryType: QueryType.PROLOG
      });

      expect(mockMetaLogApiService.prologQuery).toHaveBeenCalled();
      expect(result).toBeDefined();
    });

    test('should query with DataLog query type', async () => {
      const canvasLEntries = generateMockCanvasLEntries(2, {
        includeSelfReference: true
      });
      
      setupMockDatabaseService(mockDatabaseService, {
        canvasLEntries
      });
      
      setupMockMetaLogApiService(mockMetaLogApiService, {
        available: true,
        datalogResults: {
          facts: [
            { predicate: 'consumes', args: ['agent', 'doc.jsonl'] }
          ]
        }
      });

      const result = await agentProvenanceQueryService.queryFederatedProvenance({
        files: ['file1.canvasl'],
        query: 'consumes(Agent, Document)',
        queryType: QueryType.DATALOG
      });

      expect(mockMetaLogApiService.datalogQuery).toHaveBeenCalled();
      expect(result).toBeDefined();
    });

    test('should query with SPARQL query type', async () => {
      const canvasLEntries = generateMockCanvasLEntries(2, {
        includeSelfReference: true
      });
      
      setupMockDatabaseService(mockDatabaseService, {
        canvasLEntries
      });
      
      setupMockMetaLogApiService(mockMetaLogApiService, {
        available: true,
        sparqlResults: {
          results: {
            bindings: []
          }
        }
      });

      const query = `
        PREFIX prov: <http://www.w3.org/ns/prov#>
        SELECT ?entry ?provenance
        WHERE {
          ?entry prov:wasDerivedFrom ?provenance .
        }
      `;

      const result = await agentProvenanceQueryService.queryFederatedProvenance({
        files: ['file1.canvasl', 'file2.canvasl'],
        query,
        queryType: QueryType.SPARQL
      });

      expect(mockMetaLogApiService.sparqlQuery).toHaveBeenCalled();
      expect(result).toBeDefined();
    });

    test('should fallback to individual file extraction', async () => {
      const canvasLEntries1 = generateMockCanvasLEntries(2, {
        includeSelfReference: true,
        includeProvenanceHistory: true
      });
      
      const canvasLEntries2 = generateMockCanvasLEntries(2, {
        includeSelfReference: true,
        includeProvenanceHistory: true
      });
      
      setupMockDatabaseService(mockDatabaseService, {
        canvasLEntries: [...canvasLEntries1, ...canvasLEntries2]
      });
      
      setupMockMetaLogApiService(mockMetaLogApiService, {
        available: false
      });

      const result = await agentProvenanceQueryService.queryFederatedProvenance({
        files: ['file1.canvasl', 'file2.canvasl'],
        query: 'test query',
        queryType: QueryType.SPARQL
      });

      expect(result).toBeDefined();
      expect(result.results).toBeDefined();
      expect(Array.isArray(result.results)).toBe(true);
      expect(result.results.length).toBeGreaterThan(0);
    });

    test('should handle error during federated query', async () => {
      const canvasLEntries = generateMockCanvasLEntries(2, {
        includeSelfReference: true,
        includeProvenanceHistory: true
      });
      
      setupMockDatabaseService(mockDatabaseService, {
        canvasLEntries
      });
      
      setupMockMetaLogApiService(mockMetaLogApiService, {
        available: true
      });
      
      mockMetaLogApiService.loadCanvas.mockRejectedValue(new Error('Load failed'));

      const result = await agentProvenanceQueryService.queryFederatedProvenance({
        files: ['file1.canvasl'],
        query: 'test query',
        queryType: QueryType.SPARQL
      });

      // Should fallback to extraction
      expect(result).toBeDefined();
      expect(result.results).toBeDefined();
    });
  });

  describe('extractProvenanceFromCanvasL', () => {
    test('should extract from self-reference metadata', async () => {
      const canvasLEntries = generateMockCanvasLEntries(3, {
        includeSelfReference: true
      });
      
      setupMockDatabaseService(mockDatabaseService, {
        canvasLEntries
      });

      const result = await agentProvenanceQueryService.extractProvenanceFromCanvasL('test.canvasl');

      expect(mockDatabaseService.readJSONL).toHaveBeenCalledWith('test.canvasl');
      expect(result.length).toBe(3);
      expect(result[0].file).toBeDefined();
      expect(result[0].line).toBeDefined();
      expect(result[0].pattern).toBeDefined();
    });

    test('should extract from provenance history arrays', async () => {
      const canvasLEntries = generateMockCanvasLEntries(2, {
        includeSelfReference: true,
        includeProvenanceHistory: true
      });
      
      setupMockDatabaseService(mockDatabaseService, {
        canvasLEntries
      });

      const result = await agentProvenanceQueryService.extractProvenanceFromCanvasL('test.canvasl');

      expect(result.length).toBeGreaterThan(2); // Should include both self-ref and history
      const historyEntries = result.filter(r => r.source === 'provenanceHistory');
      expect(historyEntries.length).toBeGreaterThan(0);
    });

    test('should handle multiple provenance entries', async () => {
      const canvasLEntries = generateMockCanvasLEntries(1, {
        includeSelfReference: true,
        includeProvenanceHistory: true
      });
      
      // Add multiple provenance history entries
      canvasLEntries[0].provenanceHistory = [
        { file: 'source1.jsonl', line: 1, pattern: 'pattern1', timestamp: Date.now() },
        { file: 'source2.jsonl', line: 2, pattern: 'pattern2', timestamp: Date.now() },
        { file: 'source3.jsonl', line: 3, pattern: 'pattern3', timestamp: Date.now() }
      ];
      
      setupMockDatabaseService(mockDatabaseService, {
        canvasLEntries
      });

      const result = await agentProvenanceQueryService.extractProvenanceFromCanvasL('test.canvasl');

      expect(result.length).toBe(4); // 1 self-ref + 3 history entries
    });

    test('should handle missing provenance data', async () => {
      const canvasLEntries = generateMockCanvasLEntries(2, {
        includeSelfReference: false,
        includeProvenanceHistory: false
      });
      
      setupMockDatabaseService(mockDatabaseService, {
        canvasLEntries
      });

      const result = await agentProvenanceQueryService.extractProvenanceFromCanvasL('test.canvasl');

      // Should return empty array if no provenance data
      expect(Array.isArray(result)).toBe(true);
    });

    test('should handle error for invalid files', async () => {
      mockDatabaseService.readJSONL.mockRejectedValue(new Error('File not found'));

      const result = await agentProvenanceQueryService.extractProvenanceFromCanvasL('invalid.canvasl');

      // Should return empty array on error
      expect(Array.isArray(result)).toBe(true);
      expect(result.length).toBe(0);
    });

    test('should extract timestamp from provenance entries', async () => {
      const canvasLEntries = generateMockCanvasLEntries(2, {
        includeSelfReference: true
      });
      
      // Add timestamps to self-reference
      canvasLEntries[0].selfReference!.timestamp = Date.now();
      canvasLEntries[1].selfReference!.timestamp = Date.now() + 1000;
      
      setupMockDatabaseService(mockDatabaseService, {
        canvasLEntries
      });

      const result = await agentProvenanceQueryService.extractProvenanceFromCanvasL('test.canvasl');

      expect(result[0].timestamp).toBeDefined();
      expect(result[1].timestamp).toBeDefined();
    });

    test('should handle entries with both selfReference and provenanceHistory', async () => {
      const canvasLEntries = generateMockCanvasLEntries(1, {
        includeSelfReference: true,
        includeProvenanceHistory: true
      });
      
      canvasLEntries[0].provenanceHistory = [
        { file: 'source.jsonl', line: 1, pattern: 'pattern', timestamp: Date.now() }
      ];
      
      setupMockDatabaseService(mockDatabaseService, {
        canvasLEntries
      });

      const result = await agentProvenanceQueryService.extractProvenanceFromCanvasL('test.canvasl');

      // Should include both self-reference and history
      expect(result.length).toBe(2);
      const selfRefEntry = result.find(r => !r.source);
      const historyEntry = result.find(r => r.source === 'provenanceHistory');
      expect(selfRefEntry).toBeDefined();
      expect(historyEntry).toBeDefined();
    });
  });
});

