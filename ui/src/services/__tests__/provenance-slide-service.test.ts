/**
 * Provenance Slide Service Tests
 */

import { describe, test, expect, beforeEach, vi } from 'vitest';
import { ProvenanceSlideService } from '../provenance-slide-service';
import { generateMockEvolutionEntries, generateMockDatabaseResults } from './utils/mockEvolutionData';
import { generateMockProvenanceNodes } from './utils/mockProvenanceChain';
import { createMockDatabaseService, setupMockDatabaseService } from './utils/mockDatabaseService';
import { createMockMetaLogApiService, setupMockMetaLogApiService } from './utils/mockMetaLogApiService';

// Mock dependencies - inline to avoid hoisting issues
vi.mock('../database-service', () => {
  return {
    databaseService: {
      readJSONL: vi.fn().mockResolvedValue([]),
      writeJSONL: vi.fn().mockResolvedValue(undefined),
      queryJSONL: vi.fn().mockResolvedValue([]),
      readCanvasL: vi.fn().mockResolvedValue([]),
      writeCanvasL: vi.fn().mockResolvedValue(undefined),
      queryCanvasL: vi.fn().mockResolvedValue([]),
      query: vi.fn().mockResolvedValue([]),
      appendJSONL: vi.fn().mockResolvedValue(undefined),
      getR5RSFunction: vi.fn().mockResolvedValue(null),
      listR5RSFunctions: vi.fn().mockResolvedValue([]),
      invokeR5RSFunction: vi.fn().mockResolvedValue(null),
      registerR5RSFunction: vi.fn().mockResolvedValue(undefined),
      create: vi.fn().mockResolvedValue(''),
      read: vi.fn().mockResolvedValue(null),
      update: vi.fn().mockResolvedValue(undefined),
      delete: vi.fn().mockResolvedValue(undefined)
    }
  };
});

vi.mock('../agent-provenance-query-service', () => {
  return {
    agentProvenanceQueryService: {
      queryFederatedProvenance: vi.fn().mockResolvedValue([]),
      queryProvenanceByFile: vi.fn().mockResolvedValue([]),
      queryProvenanceByPattern: vi.fn().mockResolvedValue([]),
      queryProvenanceByAgent: vi.fn().mockResolvedValue([]),
      extractProvenanceFromCanvasL: vi.fn().mockResolvedValue([]),
      queryProlog: vi.fn().mockResolvedValue({ bindings: [] }),
      queryDatalog: vi.fn().mockResolvedValue({ facts: [] }),
      querySparql: vi.fn().mockResolvedValue({ results: { bindings: [] } }),
      buildQuery: vi.fn().mockReturnValue(''),
      loadAgentHistory: vi.fn().mockResolvedValue(undefined),
      getQueryTemplates: vi.fn().mockReturnValue([]),
      executeQuery: vi.fn().mockResolvedValue({})
    },
    QueryType: {
      SPARQL: 'sparql',
      PROLOG: 'prolog',
      DATALOG: 'datalog'
    }
  };
});

vi.mock('../projector/Projector', () => {
  return {
    Projector: class {
      onInit = vi.fn().mockResolvedValue(undefined);
    }
  };
});

vi.mock('../agent-coordinator/AgentCoordinator', () => {
  return {
    AgentCoordinator: class {
      init = vi.fn().mockResolvedValue(undefined);
    }
  };
});

vi.mock('../projector/TopicSlideGenerator', () => {
  return {
    TopicSlideGenerator: class {
      constructor(coordinator: any, options: any) {}
    }
  };
});

describe('ProvenanceSlideService', () => {
  let service: ProvenanceSlideService;
  let mockDatabaseService: ReturnType<typeof createMockDatabaseService>;
  let mockAgentProvenanceService: ReturnType<typeof createMockMetaLogApiService>;

  beforeEach(async () => {
    vi.clearAllMocks();
    
    // Get mocked services
    const { databaseService } = await import('../database-service');
    const { agentProvenanceQueryService } = await import('../agent-provenance-query-service');
    
    mockDatabaseService = databaseService as any;
    mockAgentProvenanceService = agentProvenanceQueryService as any;
    
    service = new ProvenanceSlideService();
  });

  describe('buildProvenanceChain', () => {
    test('should build provenance chain with mock evolution data', async () => {
      const evolutionEntries = generateMockEvolutionEntries(5, {
        dimension: '0D',
        pattern: 'identity',
        includeProvenance: true
      });
      
      const dbResults = generateMockDatabaseResults(evolutionEntries);
      
      // Setup database service mock - loadEvolutionFiles calls query with SPARQL
      if (mockDatabaseService.query) {
        mockDatabaseService.query.mockImplementation(async (query: string, type?: string) => {
          if (type === 'sparql' && query.includes('evolution:EvolutionFile')) {
            // Return SPARQL results format - service expects result.content and result.file directly
            return dbResults.map((result: any) => ({
              file: result.file,
              content: result.content
            }));
          }
          return [];
        });
      }
      
      // Also mock readJSONL in case it's used as fallback
      if (mockDatabaseService.readJSONL) {
        mockDatabaseService.readJSONL.mockImplementation(async (file: string) => {
          if (file.includes('evolution')) {
            return evolutionEntries;
          }
          return [];
        });
      }
      
      // Setup agent provenance service mock directly
      if (mockAgentProvenanceService.queryFederatedProvenance) {
        mockAgentProvenanceService.queryFederatedProvenance.mockResolvedValue([]);
      }
      if (mockAgentProvenanceService.queryProvenanceByFile) {
        mockAgentProvenanceService.queryProvenanceByFile.mockResolvedValue([]);
      }
      // Don't use setupMockMetaLogApiService - it expects different method names
      // Mock setup is done directly above

      const chain = await service.buildProvenanceChain('evolution/test');

      expect(chain.nodes).toHaveLength(5);
      expect(chain.nodes[0].metadata.dimension).toBe('0D');
      expect(chain.nodes[0].metadata.pattern).toBeDefined();
      expect(chain.nodes[0].metadata.churchEncoding).toBeDefined();
    });

    test('should create dimensional progression edges', async () => {
      const entries0D = generateMockEvolutionEntries(2, { dimension: '0D', file: 'evolution/0d.jsonl' });
      const entries1D = generateMockEvolutionEntries(2, { dimension: '1D', file: 'evolution/1d.jsonl' });
      
      const dbResults = generateMockDatabaseResults([...entries0D, ...entries1D]);
      // Setup database service mock - loadEvolutionFiles calls query with SPARQL
      if (mockDatabaseService.query) {
        mockDatabaseService.query.mockImplementation(async (query: string, type?: string) => {
          if (type === 'sparql' && query.includes('evolution:EvolutionFile')) {
            return dbResults.map((result: any) => ({
              file: result.file,
              content: result.content
            }));
          }
          return [];
        });
      }
      
      // Also mock readJSONL in case it's used as fallback
      if (mockDatabaseService.readJSONL) {
        mockDatabaseService.readJSONL.mockImplementation(async (file: string) => {
          if (file.includes('evolution')) {
            return [...entries0D, ...entries1D];
          }
          return [];
        });
      }
      
      // Setup agent provenance service mock directly
      if (mockAgentProvenanceService.queryFederatedProvenance) {
        mockAgentProvenanceService.queryFederatedProvenance.mockResolvedValue([]);
      }
      if (mockAgentProvenanceService.queryProvenanceByFile) {
        mockAgentProvenanceService.queryProvenanceByFile.mockResolvedValue([]);
      }
      // Don't use setupMockMetaLogApiService - it expects different method names
      // Mock setup is done directly above

      const chain = await service.buildProvenanceChain('evolution/test');

      // Should have edges between 0D and 1D nodes
      const evolveEdges = chain.edges.filter(e => e.type === 'evolves');
      expect(evolveEdges.length).toBeGreaterThan(0);
    });

    test('should create cross-file reference edges', async () => {
      const entries1 = generateMockEvolutionEntries(2, { dimension: '0D', file: 'evolution/file1.jsonl' });
      const entries2 = generateMockEvolutionEntries(2, { dimension: '0D', file: 'evolution/file2.jsonl' });
      
      const dbResults = generateMockDatabaseResults([...entries1, ...entries2]);
      // Setup database service mock
      if (mockDatabaseService.query) {
        mockDatabaseService.query.mockImplementation(async (query: string, type?: string) => {
          if (type === 'sparql' && query.includes('evolution:EvolutionFile')) {
            return dbResults.map((result: any) => ({
              file: result.file,
              content: result.content
            }));
          }
          return [];
        });
      }
      
      // Setup agent provenance service mock directly
      if (mockAgentProvenanceService.queryFederatedProvenance) {
        mockAgentProvenanceService.queryFederatedProvenance.mockResolvedValue([]);
      }
      if (mockAgentProvenanceService.queryProvenanceByFile) {
        mockAgentProvenanceService.queryProvenanceByFile.mockResolvedValue([]);
      }
      // Don't use setupMockMetaLogApiService - it expects different method names
      // Mock setup is done directly above

      const chain = await service.buildProvenanceChain('evolution/test');

      const refEdges = chain.edges.filter(e => e.type === 'references');
      expect(refEdges.length).toBeGreaterThan(0);
    });

    test('should handle empty evolution directory', async () => {
      setupMockDatabaseService(mockDatabaseService, {
        evolutionFiles: []
      });

      const chain = await service.buildProvenanceChain('evolution/empty');

      expect(chain.nodes).toHaveLength(0);
      expect(chain.edges).toHaveLength(0);
    });

    test('should handle invalid/malformed evolution files', async () => {
      const dbResults = [
        {
          file: 'evolution/invalid.jsonl',
          content: 'invalid json\n{invalid}\n'
        }
      ];
      
      // Setup database service mock
      if (mockDatabaseService.query) {
        mockDatabaseService.query.mockImplementation(async (query: string, type?: string) => {
          if (type === 'sparql' && query.includes('evolution:EvolutionFile')) {
            return dbResults.map((result: any) => ({
              file: result.file,
              content: result.content
            }));
          }
          return [];
        });
      }

      const chain = await service.buildProvenanceChain('evolution/invalid');

      // Should skip invalid entries
      expect(chain.nodes.length).toBeLessThanOrEqual(0);
    });

    test('should track federated provenance', async () => {
      const entries = generateMockEvolutionEntries(3, {
        dimension: '0D',
        includeProvenance: true
      });
      
      const dbResults = generateMockDatabaseResults(entries);
      // Setup database service mock
      if (mockDatabaseService.query) {
        mockDatabaseService.query.mockImplementation(async (query: string, type?: string) => {
          if (type === 'sparql' && query.includes('evolution:EvolutionFile')) {
            return dbResults.map((result: any) => ({
              file: result.file,
              content: result.content
            }));
          }
          return [];
        });
      }
      
      const provenanceResults = [
        { file: 'source.jsonl', line: 1, pattern: 'identity', timestamp: Date.now() }
      ];
      
      // Setup agent provenance service mock directly
      if (mockAgentProvenanceService.queryFederatedProvenance) {
        mockAgentProvenanceService.queryFederatedProvenance.mockResolvedValue([]);
      }
      if (mockAgentProvenanceService.queryProvenanceByFile) {
        mockAgentProvenanceService.queryProvenanceByFile.mockResolvedValue([]);
      }
      // Don't use setupMockMetaLogApiService - it expects different method names
      // Mock setup is done directly above

      const chain = await service.buildProvenanceChain('evolution/test');

      expect(chain.nodes.length).toBeGreaterThan(0);
      // Check that provenance history is included in node data
      const nodeWithProvenance = chain.nodes.find(n => n.data?.provenanceHistory?.length > 0);
      expect(nodeWithProvenance).toBeDefined();
    });
  });

  describe('generateSlidesFromEvolution', () => {
    test('should generate slides for all dimensions', async () => {
      const entries = generateMockEvolutionEntries(3, { dimension: '0D' });
      const dbResults = generateMockDatabaseResults(entries);
      // Setup database service mock
      if (mockDatabaseService.query) {
        mockDatabaseService.query.mockImplementation(async (query: string, type?: string) => {
          if (type === 'sparql' && query.includes('evolution:EvolutionFile')) {
            return dbResults.map((result: any) => ({
              file: result.file,
              content: result.content
            }));
          }
          return [];
        });
      }
      
      // Setup agent provenance service mock directly
      if (mockAgentProvenanceService.queryFederatedProvenance) {
        mockAgentProvenanceService.queryFederatedProvenance.mockResolvedValue([]);
      }
      if (mockAgentProvenanceService.queryProvenanceByFile) {
        mockAgentProvenanceService.queryProvenanceByFile.mockResolvedValue([]);
      }
      // Don't use setupMockMetaLogApiService - it expects different method names
      // Mock setup is done directly above

      const slides = await service.generateSlidesFromEvolution('evolution/test');

      // Should generate slides for 0D→7D→0D (9 slides)
      expect(slides.length).toBe(9);
      expect(slides[0].dimension).toBe('0D');
      expect(slides[7].dimension).toBe('7D');
      expect(slides[8].dimension).toBe('0D');
    });

    test('should group nodes by dimension', async () => {
      const entries0D = generateMockEvolutionEntries(2, { dimension: '0D' });
      const entries1D = generateMockEvolutionEntries(2, { dimension: '1D' });
      
      const dbResults = generateMockDatabaseResults([...entries0D, ...entries1D]);
      // Setup database service mock - loadEvolutionFiles calls query with SPARQL
      if (mockDatabaseService.query) {
        mockDatabaseService.query.mockImplementation(async (query: string, type?: string) => {
          if (type === 'sparql' && query.includes('evolution:EvolutionFile')) {
            return dbResults.map((result: any) => ({
              file: result.file,
              content: result.content
            }));
          }
          return [];
        });
      }
      
      // Also mock readJSONL in case it's used as fallback
      if (mockDatabaseService.readJSONL) {
        mockDatabaseService.readJSONL.mockImplementation(async (file: string) => {
          if (file.includes('evolution')) {
            return [...entries0D, ...entries1D];
          }
          return [];
        });
      }
      
      // Setup agent provenance service mock directly
      if (mockAgentProvenanceService.queryFederatedProvenance) {
        mockAgentProvenanceService.queryFederatedProvenance.mockResolvedValue([]);
      }
      if (mockAgentProvenanceService.queryProvenanceByFile) {
        mockAgentProvenanceService.queryProvenanceByFile.mockResolvedValue([]);
      }
      // Don't use setupMockMetaLogApiService - it expects different method names
      // Mock setup is done directly above

      const slides = await service.generateSlidesFromEvolution('evolution/test');

      const slide0D = slides.find(s => s.dimension === '0D');
      const slide1D = slides.find(s => s.dimension === '1D');
      
      expect(slide0D?.provenanceChain?.nodes.length).toBeGreaterThan(0);
      expect(slide1D?.provenanceChain?.nodes.length).toBeGreaterThan(0);
    });

    test('should generate slide content with Church encoding', async () => {
      const entries = generateMockEvolutionEntries(2, { dimension: '0D' });
      const dbResults = generateMockDatabaseResults(entries);
      // Setup database service mock
      if (mockDatabaseService.query) {
        mockDatabaseService.query.mockImplementation(async (query: string, type?: string) => {
          if (type === 'sparql' && query.includes('evolution:EvolutionFile')) {
            return dbResults.map((result: any) => ({
              file: result.file,
              content: result.content
            }));
          }
          return [];
        });
      }
      
      // Setup agent provenance service mock directly
      if (mockAgentProvenanceService.queryFederatedProvenance) {
        mockAgentProvenanceService.queryFederatedProvenance.mockResolvedValue([]);
      }
      if (mockAgentProvenanceService.queryProvenanceByFile) {
        mockAgentProvenanceService.queryProvenanceByFile.mockResolvedValue([]);
      }
      // Don't use setupMockMetaLogApiService - it expects different method names
      // Mock setup is done directly above

      const slides = await service.generateSlidesFromEvolution('evolution/test');

      const slide0D = slides.find(s => s.dimension === '0D');
      expect(slide0D?.content).toContain('Church Encoding');
      expect(slide0D?.content).toContain('λf.λx.x');
    });

    test('should generate slide content with BQF forms', async () => {
      const entries = generateMockEvolutionEntries(2, { dimension: '2D' });
      const dbResults = generateMockDatabaseResults(entries);
      // Setup database service mock
      if (mockDatabaseService.query) {
        mockDatabaseService.query.mockImplementation(async (query: string, type?: string) => {
          if (type === 'sparql' && query.includes('evolution:EvolutionFile')) {
            return dbResults.map((result: any) => ({
              file: result.file,
              content: result.content
            }));
          }
          return [];
        });
      }
      
      // Setup agent provenance service mock directly
      if (mockAgentProvenanceService.queryFederatedProvenance) {
        mockAgentProvenanceService.queryFederatedProvenance.mockResolvedValue([]);
      }
      if (mockAgentProvenanceService.queryProvenanceByFile) {
        mockAgentProvenanceService.queryProvenanceByFile.mockResolvedValue([]);
      }
      // Don't use setupMockMetaLogApiService - it expects different method names
      // Mock setup is done directly above

      const slides = await service.generateSlidesFromEvolution('evolution/test');

      const slide2D = slides.find(s => s.dimension === '2D');
      expect(slide2D?.content).toContain('BQF Form');
      expect(slide2D?.content).toContain('x²');
    });

    test('should handle nodes missing dimension metadata', async () => {
      const entries = generateMockEvolutionEntries(2, { dimension: '0D' });
      // Remove dimension from some entries
      entries[1].metadata = entries[1].metadata || {};
      delete entries[1].metadata.dimension;
      
      const dbResults = generateMockDatabaseResults(entries);
      // Setup database service mock
      if (mockDatabaseService.query) {
        mockDatabaseService.query.mockImplementation(async (query: string, type?: string) => {
          if (type === 'sparql' && query.includes('evolution:EvolutionFile')) {
            return dbResults.map((result: any) => ({
              file: result.file,
              content: result.content
            }));
          }
          return [];
        });
      }
      
      // Setup agent provenance service mock directly
      if (mockAgentProvenanceService.queryFederatedProvenance) {
        mockAgentProvenanceService.queryFederatedProvenance.mockResolvedValue([]);
      }
      if (mockAgentProvenanceService.queryProvenanceByFile) {
        mockAgentProvenanceService.queryProvenanceByFile.mockResolvedValue([]);
      }
      // Don't use setupMockMetaLogApiService - it expects different method names
      // Mock setup is done directly above

      const slides = await service.generateSlidesFromEvolution('evolution/test');

      // Should still generate slides, defaulting to 0D
      expect(slides.length).toBe(9);
    });
  });

  describe('generateCardsForDimension', () => {
    test('should generate cards with sample nodes', async () => {
      const nodes = generateMockProvenanceNodes(5, {
        dimension: '0D',
        pattern: 'identity',
        includeProvenanceHistory: true
      });

      const cards = await service.generateCardsForDimension('0D', nodes);

      expect(cards.length).toBeGreaterThan(0);
      expect(cards[0].pattern).toBeDefined();
      expect(cards[0].jsonlLines.length).toBeGreaterThan(0);
    });

    test('should group nodes by pattern', async () => {
      const nodes = [
        ...generateMockProvenanceNodes(2, { dimension: '0D', pattern: 'identity' }),
        ...generateMockProvenanceNodes(2, { dimension: '0D', pattern: 'successor' })
      ];

      const cards = await service.generateCardsForDimension('0D', nodes);

      // Should have separate cards for each pattern
      const patterns = new Set(cards.map(c => c.pattern));
      expect(patterns.size).toBe(2);
    });

    test('should aggregate JSONL lines from node data', async () => {
      const nodes = generateMockProvenanceNodes(3, {
        dimension: '0D',
        pattern: 'identity'
      });
      
      // Add rawEntry to node data
      nodes.forEach((node, i) => {
        node.data.rawEntry = { id: `entry-${i}`, type: 'test' };
      });

      const cards = await service.generateCardsForDimension('0D', nodes);

      expect(cards[0].jsonlLines.length).toBe(3);
      expect(cards[0].jsonlLines[0].id).toBe('entry-0');
    });

    test('should extract Church encoding from nodes', async () => {
      const nodes = generateMockProvenanceNodes(2, {
        dimension: '0D',
        pattern: 'identity'
      });
      
      nodes[0].metadata.churchEncoding = 'λf.λx.x';

      const cards = await service.generateCardsForDimension('0D', nodes);

      expect(cards[0].metadata.churchEncoding).toBe('λf.λx.x');
    });

    test('should calculate BQF coefficients for each dimension', async () => {
      const dimensions = ['0D', '1D', '2D', '3D'];
      
      for (const dimension of dimensions) {
        const nodes = generateMockProvenanceNodes(2, { dimension, pattern: 'test' });
        const cards = await service.generateCardsForDimension(dimension, nodes);
        
        expect(cards[0].metadata.bqfCoefficients).toBeDefined();
        expect(cards[0].metadata.bqfCoefficients?.length).toBe(3);
        
        const dimNum = parseInt(dimension.replace('D', ''));
        expect(cards[0].metadata.bqfCoefficients?.[2]).toBe(dimNum);
      }
    });

    test('should aggregate provenance history', async () => {
      const nodes = generateMockProvenanceNodes(3, {
        dimension: '0D',
        pattern: 'identity',
        includeProvenanceHistory: true
      });

      const cards = await service.generateCardsForDimension('0D', nodes);

      expect(cards[0].metadata.provenanceHistory).toBeDefined();
      expect(cards[0].metadata.provenanceHistory?.length).toBeGreaterThan(0);
    });

    test('should handle nodes with different patterns', async () => {
      const nodes = [
        ...generateMockProvenanceNodes(2, { dimension: '0D', pattern: 'pattern1' }),
        ...generateMockProvenanceNodes(2, { dimension: '0D', pattern: 'pattern2' }),
        ...generateMockProvenanceNodes(2, { dimension: '0D', pattern: 'pattern3' })
      ];

      const cards = await service.generateCardsForDimension('0D', nodes);

      expect(cards.length).toBe(3);
      expect(cards.map(c => c.pattern).sort()).toEqual(['pattern1', 'pattern2', 'pattern3']);
    });
  });

  describe('Helper Methods', () => {
    // Note: These are private methods, but we can test them indirectly through public methods
    // or we can test the behavior they produce
    
    test('extractChurchEncoding - should extract from metadata', async () => {
      const entries = generateMockEvolutionEntries(1, {
        dimension: '0D'
      });
      entries[0].metadata!.churchEncoding = 'λf.λx.x';
      
      const dbResults = generateMockDatabaseResults(entries);
      // Setup database service mock
      if (mockDatabaseService.query) {
        mockDatabaseService.query.mockImplementation(async (query: string, type?: string) => {
          if (type === 'sparql' && query.includes('evolution:EvolutionFile')) {
            return dbResults.map((result: any) => ({
              file: result.file,
              content: result.content
            }));
          }
          return [];
        });
      }
      
      // Setup agent provenance service mock directly
      if (mockAgentProvenanceService.queryFederatedProvenance) {
        mockAgentProvenanceService.queryFederatedProvenance.mockResolvedValue([]);
      }
      if (mockAgentProvenanceService.queryProvenanceByFile) {
        mockAgentProvenanceService.queryProvenanceByFile.mockResolvedValue([]);
      }
      // Don't use setupMockMetaLogApiService - it expects different method names
      // Mock setup is done directly above

      const chain = await service.buildProvenanceChain('evolution/test');

      expect(chain.nodes[0].metadata.churchEncoding).toBe('λf.λx.x');
    });

    test('inferDimensionFromPattern - should infer from pattern name', async () => {
      const entries = generateMockEvolutionEntries(1, {
        dimension: '0D',
        pattern: 'identity'
      });
      
      const dbResults = generateMockDatabaseResults(entries);
      // Setup database service mock
      if (mockDatabaseService.query) {
        mockDatabaseService.query.mockImplementation(async (query: string, type?: string) => {
          if (type === 'sparql' && query.includes('evolution:EvolutionFile')) {
            return dbResults.map((result: any) => ({
              file: result.file,
              content: result.content
            }));
          }
          return [];
        });
      }
      
      // Setup agent provenance service mock directly
      if (mockAgentProvenanceService.queryFederatedProvenance) {
        mockAgentProvenanceService.queryFederatedProvenance.mockResolvedValue([]);
      }
      if (mockAgentProvenanceService.queryProvenanceByFile) {
        mockAgentProvenanceService.queryProvenanceByFile.mockResolvedValue([]);
      }
      // Don't use setupMockMetaLogApiService - it expects different method names
      // Mock setup is done directly above

      const chain = await service.buildProvenanceChain('evolution/test');

      expect(chain.nodes[0].metadata.dimension).toBe('0D');
    });

    test('inferAgentFromDimension - should infer agent ID', async () => {
      const entries = generateMockEvolutionEntries(1, {
        dimension: '2D',
        pattern: 'pairing'
      });
      
      const dbResults = generateMockDatabaseResults(entries);
      // Setup database service mock
      if (mockDatabaseService.query) {
        mockDatabaseService.query.mockImplementation(async (query: string, type?: string) => {
          if (type === 'sparql' && query.includes('evolution:EvolutionFile')) {
            return dbResults.map((result: any) => ({
              file: result.file,
              content: result.content
            }));
          }
          return [];
        });
      }
      
      // Setup agent provenance service mock directly
      if (mockAgentProvenanceService.queryFederatedProvenance) {
        mockAgentProvenanceService.queryFederatedProvenance.mockResolvedValue([]);
      }
      if (mockAgentProvenanceService.queryProvenanceByFile) {
        mockAgentProvenanceService.queryProvenanceByFile.mockResolvedValue([]);
      }
      // Don't use setupMockMetaLogApiService - it expects different method names
      // Mock setup is done directly above

      const chain = await service.buildProvenanceChain('evolution/test');

      expect(chain.nodes[0].metadata.agentId).toBe('2D-Structural-Agent');
    });

    test('calculateBQFCoefficients - should calculate for all dimensions', async () => {
      const dimensions = ['0D', '1D', '2D', '3D', '4D', '5D', '6D', '7D'];
      
      for (const dimension of dimensions) {
        const nodes = generateMockProvenanceNodes(1, { dimension, pattern: 'test' });
        const cards = await service.generateCardsForDimension(dimension, nodes);
        
        const coeffs = cards[0].metadata.bqfCoefficients;
        expect(coeffs).toBeDefined();
        expect(coeffs?.[0]).toBe(1);
        expect(coeffs?.[1]).toBe(0);
        
        const dimNum = parseInt(dimension.replace('D', ''));
        expect(coeffs?.[2]).toBe(dimNum);
      }
    });

    test('calculatePosition - should calculate 3D positions', async () => {
      const entries = generateMockEvolutionEntries(3, {
        dimension: '2D',
        pattern: 'pairing'
      });
      
      const dbResults = generateMockDatabaseResults(entries);
      // Setup database service mock
      if (mockDatabaseService.query) {
        mockDatabaseService.query.mockImplementation(async (query: string, type?: string) => {
          if (type === 'sparql' && query.includes('evolution:EvolutionFile')) {
            return dbResults.map((result: any) => ({
              file: result.file,
              content: result.content
            }));
          }
          return [];
        });
      }
      
      // Setup agent provenance service mock directly
      if (mockAgentProvenanceService.queryFederatedProvenance) {
        mockAgentProvenanceService.queryFederatedProvenance.mockResolvedValue([]);
      }
      if (mockAgentProvenanceService.queryProvenanceByFile) {
        mockAgentProvenanceService.queryProvenanceByFile.mockResolvedValue([]);
      }
      // Don't use setupMockMetaLogApiService - it expects different method names
      // Mock setup is done directly above

      const chain = await service.buildProvenanceChain('evolution/test');

      chain.nodes.forEach(node => {
        expect(node.position).toHaveLength(3);
        expect(typeof node.position[0]).toBe('number');
        expect(typeof node.position[1]).toBe('number');
        expect(typeof node.position[2]).toBe('number');
      });
    });

    test('getChurchEncodingForDimension - should return correct encodings', async () => {
      const encodings: Record<string, string> = {
        '0D': 'λf.λx.x',
        '1D': 'λn.λf.λx.f(nfx)',
        '2D': 'λx.λy.λf.fxy',
        '3D': 'λm.λn.λf.λx.mf(nfx)'
      };

      for (const [dimension, expectedEncoding] of Object.entries(encodings)) {
        const entries = generateMockEvolutionEntries(1, { dimension });
        const dbResults = generateMockDatabaseResults(entries);
        setupMockDatabaseService(mockDatabaseService, {
          evolutionFiles: dbResults
        });
        
        // Setup agent provenance service mock directly
      if (mockAgentProvenanceService.queryFederatedProvenance) {
        mockAgentProvenanceService.queryFederatedProvenance.mockResolvedValue([]);
      }
      if (mockAgentProvenanceService.queryProvenanceByFile) {
        mockAgentProvenanceService.queryProvenanceByFile.mockResolvedValue([]);
      }
      // Don't use setupMockMetaLogApiService - it expects different method names
        // Mock setup is done directly above

        const slides = await service.generateSlidesFromEvolution('evolution/test');
        const slide = slides.find(s => s.dimension === dimension);
        
        expect(slide?.content).toContain(expectedEncoding);
      }
    });

    test('getBQFFormForDimension - should generate BQF form strings', async () => {
      const dimensions = ['0D', '1D', '2D'];
      
      for (const dimension of dimensions) {
        const entries = generateMockEvolutionEntries(1, { dimension });
        const dbResults = generateMockDatabaseResults(entries);
        setupMockDatabaseService(mockDatabaseService, {
          evolutionFiles: dbResults
        });
        
        // Setup agent provenance service mock directly
      if (mockAgentProvenanceService.queryFederatedProvenance) {
        mockAgentProvenanceService.queryFederatedProvenance.mockResolvedValue([]);
      }
      if (mockAgentProvenanceService.queryProvenanceByFile) {
        mockAgentProvenanceService.queryProvenanceByFile.mockResolvedValue([]);
      }
      // Don't use setupMockMetaLogApiService - it expects different method names
        // Mock setup is done directly above

        const slides = await service.generateSlidesFromEvolution('evolution/test');
        const slide = slides.find(s => s.dimension === dimension);
        
        expect(slide?.content).toContain('BQF Form');
        expect(slide?.content).toContain('x²');
      }
    });
  });
});

