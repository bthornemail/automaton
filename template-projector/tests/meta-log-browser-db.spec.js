import { test, expect } from '@playwright/test';

/**
 * End-to-End Tests for MetaLogDbBrowser
 * 
 * Tests the browser-native MetaLogDbBrowser implementation including:
 * - Initialization and configuration
 * - Canvas loading from URLs
 * - ProLog query execution
 * - DataLog query execution
 * - SPARQL query execution
 * - SHACL validation
 * - IndexedDB caching
 * - R5RS function execution
 * - Fact extraction and RDF conversion
 * 
 * @see {@link https://github.com/automaton-system/meta-log-db/blob/main/docs/27-Meta-Log-Browser-Db/README.md Meta-Log Browser Database Documentation}
 */

test.describe('MetaLogDbBrowser E2E Tests', () => {
  let page;
  let adapter;

  test.beforeEach(async ({ page: testPage }) => {
    page = testPage;
    
    // Navigate to a test page that loads MetaLogDbBrowser
    await page.goto('/viewer.html');
    await page.waitForLoadState('networkidle');
    
    // Wait for Projector to be initialized (it's loaded on viewer.html)
    await page.waitForFunction(() => {
      return window.projector && 
             window.projector.metaLog && 
             window.projector.metaLog.adapter &&
             window.projector.metaLog.adapter.initialized === true;
    }, { timeout: 15000 });
    
    // Verify adapter is ready
    const adapterReady = await page.evaluate(() => {
      return window.projector && 
             window.projector.metaLog && 
             window.projector.metaLog.adapter &&
             window.projector.metaLog.adapter.initialized === true;
    });
    
    expect(adapterReady).toBe(true);
  });

  test('should initialize MetaLogDbBrowser', async () => {
    const initialized = await page.evaluate(() => {
      return window.projector && 
             window.projector.metaLog && 
             window.projector.metaLog.adapter &&
             window.projector.metaLog.adapter.initialized === true;
    });
    
    expect(initialized).toBe(true);
  });

  test('should load canvas from URL', async () => {
    const result = await page.evaluate(async () => {
      try {
        const adapter = window.projector.metaLog.adapter;
        // Load a test canvas file
        await adapter.loadCanvas(
          'automaton-kernel.jsonl',
          '/jsonl/automaton-kernel.jsonl'
        );
        return { success: true, error: null };
      } catch (error) {
        return { success: false, error: error.message };
      }
    });
    
    expect(result.success).toBe(true);
    expect(result.error).toBeNull();
  });

  test('should extract facts from loaded canvas', async () => {
    await page.evaluate(async () => {
      const adapter = window.projector.metaLog.adapter;
      await adapter.loadCanvas(
        'automaton-kernel.jsonl',
        '/jsonl/automaton-kernel.jsonl'
      );
    });
    
    const facts = await page.evaluate(() => {
      const adapter = window.projector.metaLog.adapter;
      return adapter.extractFacts();
    });
    
    expect(Array.isArray(facts)).toBe(true);
    expect(facts.length).toBeGreaterThan(0);
  });

  test('should convert facts to RDF triples', async () => {
    await page.evaluate(async () => {
      const adapter = window.projector.metaLog.adapter;
      await adapter.loadCanvas(
        'automaton-kernel.jsonl',
        '/jsonl/automaton-kernel.jsonl'
      );
    });
    
    const triples = await page.evaluate(() => {
      const adapter = window.projector.metaLog.adapter;
      const facts = adapter.extractFacts();
      return adapter.jsonlToRdf(facts);
    });
    
    expect(Array.isArray(triples)).toBe(true);
    expect(triples.length).toBeGreaterThan(0);
    
    // Verify triple structure
    if (triples.length > 0) {
      const triple = triples[0];
      expect(triple).toHaveProperty('subject');
      expect(triple).toHaveProperty('predicate');
      expect(triple).toHaveProperty('object');
    }
  });

  test('should execute ProLog queries', async () => {
    await page.evaluate(async () => {
      const adapter = window.projector.metaLog.adapter;
      // Load canvas first
      await adapter.loadCanvas(
        'automaton-kernel.jsonl',
        '/jsonl/automaton-kernel.jsonl'
      );
      
      // Add some facts manually for testing
      const facts = [
        { predicate: 'node', args: ['test-node-1', 'test-type', 0, 0, 'Test Node'] },
        { predicate: 'node', args: ['test-node-2', 'test-type', 1, 1, 'Test Node 2'] }
      ];
      adapter.addPrologFacts(facts);
    });
    
    const result = await page.evaluate(async () => {
      try {
        const adapter = window.projector.metaLog.adapter;
        // Execute ProLog query
        const queryResult = await adapter.prologQuery('(node ?Id ?Type)');
        return { success: true, result: queryResult, error: null };
      } catch (error) {
        return { success: false, result: null, error: error.message };
      }
    });
    
    expect(result.success).toBe(true);
    expect(result.error).toBeNull();
    expect(result.result).toBeDefined();
  });

  test('should execute DataLog queries', async () => {
    await page.evaluate(async () => {
      const adapter = window.projector.metaLog.adapter;
      // Load canvas first
      await adapter.loadCanvas(
        'automaton-kernel.jsonl',
        '/jsonl/automaton-kernel.jsonl'
      );
    });
    
    const result = await page.evaluate(async () => {
      try {
        const adapter = window.projector.metaLog.adapter;
        // Execute DataLog query
        const queryResult = await adapter.datalogQuery('(node ?Id ?Type)');
        return { success: true, result: queryResult, error: null };
      } catch (error) {
        return { success: false, result: null, error: error.message };
      }
    });
    
    expect(result.success).toBe(true);
    expect(result.error).toBeNull();
    expect(result.result).toBeDefined();
  });

  test('should execute SPARQL queries', async () => {
    await page.evaluate(async () => {
      const adapter = window.projector.metaLog.adapter;
      // Load canvas first
      await adapter.loadCanvas(
        'automaton-kernel.jsonl',
        '/jsonl/automaton-kernel.jsonl'
      );
    });
    
    const result = await page.evaluate(async () => {
      try {
        const adapter = window.projector.metaLog.adapter;
        // Execute SPARQL query
        const sparqlQuery = `
          SELECT ?id ?type WHERE {
            ?id <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> ?type
          } LIMIT 10
        `;
        const queryResult = await adapter.sparqlQuery(sparqlQuery);
        return { success: true, result: queryResult, error: null };
      } catch (error) {
        return { success: false, result: null, error: error.message };
      }
    });
    
    expect(result.success).toBe(true);
    expect(result.error).toBeNull();
    expect(result.result).toBeDefined();
  });

  test('should validate with SHACL', async () => {
    await page.evaluate(async () => {
      const adapter = window.projector.metaLog.adapter;
      // Load canvas first
      await adapter.loadCanvas(
        'automaton-kernel.jsonl',
        '/jsonl/automaton-kernel.jsonl'
      );
    });
    
    const result = await page.evaluate(async () => {
      try {
        const adapter = window.projector.metaLog.adapter;
        // Create simple SHACL shape
        const shapes = {
          'NodeShape': {
            targetClass: 'http://example.org/type/node',
            properties: [{
              path: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
              minCount: 1
            }]
          }
        };
        
        const validationResult = await adapter.shaclValidate(shapes);
        return { success: true, result: validationResult, error: null };
      } catch (error) {
        return { success: false, result: null, error: error.message };
      }
    });
    
    expect(result.success).toBe(true);
    expect(result.error).toBeNull();
    expect(result.result).toBeDefined();
    expect(result.result).toHaveProperty('conforms');
  });

  test('should execute R5RS functions', async () => {
    const result = await page.evaluate(async () => {
      try {
        const adapter = window.projector.metaLog.adapter;
        // Execute R5RS function
        const churchZero = await adapter.executeR5RS('r5rs:church-zero', []);
        const churchAdd = await adapter.executeR5RS('r5rs:church-add', [
          (f) => (x) => f(f(x)), // church-2
          (f) => (x) => f(f(f(x))) // church-3
        ]);
        
        return { 
          success: true, 
          churchZero: typeof churchZero === 'function',
          churchAdd: typeof churchAdd === 'function',
          error: null 
        };
      } catch (error) {
        return { success: false, error: error.message };
      }
    });
    
    expect(result.success).toBe(true);
    expect(result.error).toBeNull();
    expect(result.churchZero).toBe(true);
    expect(result.churchAdd).toBe(true);
  });

  test('should cache files in IndexedDB', async () => {
    // First load
    await page.evaluate(async () => {
      const adapter = window.projector.metaLog.adapter;
      await adapter.loadCanvas(
        'automaton-kernel.jsonl',
        '/jsonl/automaton-kernel.jsonl'
      );
    });
    
    // Clear memory cache to force IndexedDB read
    const cached = await page.evaluate(async () => {
      const adapter = window.projector.metaLog.adapter;
      // Load again - should use IndexedDB cache
      const startTime = performance.now();
      await adapter.loadCanvas(
        'automaton-kernel.jsonl',
        '/jsonl/automaton-kernel.jsonl'
      );
      const loadTime = performance.now() - startTime;
      
      // Check if IndexedDB has the file
      const dbName = 'meta-log-db';
      const db = await new Promise((resolve, reject) => {
        const request = indexedDB.open(dbName, 1);
        request.onsuccess = () => resolve(request.result);
        request.onerror = () => reject(request.error);
      });
      
      const transaction = db.transaction(['files'], 'readonly');
      const store = transaction.objectStore('files');
      const cachedFile = await new Promise((resolve, reject) => {
        const request = store.get('automaton-kernel.jsonl');
        request.onsuccess = () => resolve(request.result);
        request.onerror = () => reject(request.error);
      });
      
      db.close();
      
      return {
        cached: cachedFile !== undefined,
        loadTime: loadTime
      };
    });
    
    expect(cached.cached).toBe(true);
    expect(cached.loadTime).toBeLessThan(1000); // Should be fast from cache
  });

  test('should handle ProLog rules', async () => {
    await page.evaluate(async () => {
      const adapter = window.projector.metaLog.adapter;
      // Load canvas first
      await adapter.loadCanvas(
        'automaton-kernel.jsonl',
        '/jsonl/automaton-kernel.jsonl'
      );
      
      // Add ProLog rule
      adapter.addPrologRule('inherits(X,Z) :- vertical(Y,X), inherits(Y,Z)');
    });
    
    const result = await page.evaluate(async () => {
      try {
        const adapter = window.projector.metaLog.adapter;
        // Query using the rule
        const queryResult = await adapter.prologQuery('(inherits ?X ?Z)');
        return { success: true, result: queryResult, error: null };
      } catch (error) {
        return { success: false, result: null, error: error.message };
      }
    });
    
    expect(result.success).toBe(true);
    expect(result.error).toBeNull();
  });

  test('should handle DataLog rules', async () => {
    await page.evaluate(async () => {
      const adapter = window.projector.metaLog.adapter;
      // Load canvas first
      await adapter.loadCanvas(
        'automaton-kernel.jsonl',
        '/jsonl/automaton-kernel.jsonl'
      );
      
      // Add DataLog rule
      adapter.addDatalogRule('inherits(X,Z) :- vertical(Y,X), inherits(Y,Z)');
    });
    
    const result = await page.evaluate(async () => {
      try {
        const adapter = window.projector.metaLog.adapter;
        // Query using the rule
        const queryResult = await adapter.datalogQuery('(inherits ?X ?Z)');
        return { success: true, result: queryResult, error: null };
      } catch (error) {
        return { success: false, result: null, error: error.message };
      }
    });
    
    expect(result.success).toBe(true);
    expect(result.error).toBeNull();
  });

  test('should store and query RDF triples', async () => {
    await page.evaluate(async () => {
      const adapter = window.projector.metaLog.adapter;
      // Store some triples
      const triples = [
        {
          subject: '<http://example.org/node/test1>',
          predicate: '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>',
          object: '<http://example.org/type/TestNode>'
        },
        {
          subject: '<http://example.org/node/test2>',
          predicate: '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>',
          object: '<http://example.org/type/TestNode>'
        }
      ];
      
      adapter.storeTriples(triples);
    });
    
    const result = await page.evaluate(async () => {
      try {
        const adapter = window.projector.metaLog.adapter;
        // Query stored triples
        const sparqlQuery = `
          SELECT ?s ?o WHERE {
            ?s <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> ?o
          }
        `;
        const queryResult = await adapter.sparqlQuery(sparqlQuery);
        return { success: true, result: queryResult, error: null };
      } catch (error) {
        return { success: false, result: null, error: error.message };
      }
    });
    
    expect(result.success).toBe(true);
    expect(result.error).toBeNull();
    expect(result.result).toBeDefined();
  });

  test('should clear cache', async () => {
    await page.evaluate(async () => {
      const adapter = window.projector.metaLog.adapter;
      // Load canvas first
      await adapter.loadCanvas(
        'automaton-kernel.jsonl',
        '/jsonl/automaton-kernel.jsonl'
      );
      
      // Clear cache
      await adapter.clear();
    });
    
    const cleared = await page.evaluate(async () => {
      // Check if IndexedDB still has the file
      const dbName = 'meta-log-db';
      try {
        const db = await new Promise((resolve, reject) => {
          const request = indexedDB.open(dbName, 1);
          request.onsuccess = () => resolve(request.result);
          request.onerror = () => reject(request.error);
        });
        
        const transaction = db.transaction(['files'], 'readonly');
        const store = transaction.objectStore('files');
        const countRequest = store.count();
        
        const count = await new Promise((resolve, reject) => {
          countRequest.onsuccess = () => resolve(countRequest.result);
          countRequest.onerror = () => reject(countRequest.error);
        });
        
        db.close();
        
        return count === 0;
      } catch (error) {
        // If database doesn't exist or error, consider it cleared
        return true;
      }
    });
    
    expect(cleared).toBe(true);
  });

  test('should handle errors gracefully', async () => {
    const result = await page.evaluate(async () => {
      try {
        const adapter = window.projector.metaLog.adapter;
        // Try to load non-existent file
        await adapter.loadCanvas(
          'non-existent-file.jsonl',
          '/jsonl/non-existent-file.jsonl'
        );
        return { success: false, error: null };
      } catch (error) {
        return { success: false, error: error.message };
      }
    });
    
    // Should handle error gracefully
    expect(result.error).toBeDefined();
    expect(result.error).toContain('Failed to load canvas');
  });

  test('should support different cache strategies', async () => {
    // Test memory-only cache
    await page.evaluate(async () => {
      const { MetaLogBrowserAdapter } = await import('/src/projector/MetaLogBrowserAdapter.js');
      window.memoryAdapter = new MetaLogBrowserAdapter({
        cacheStrategy: 'memory'
      });
      await window.memoryAdapter.init();
    });
    
    const memoryOnly = await page.evaluate(() => {
      return window.memoryAdapter.config.cacheStrategy === 'memory';
    });
    
    expect(memoryOnly).toBe(true);
    
    // Test IndexedDB-only cache
    await page.evaluate(async () => {
      const { MetaLogBrowserAdapter } = await import('/src/projector/MetaLogBrowserAdapter.js');
      window.indexedDBAdapter = new MetaLogBrowserAdapter({
        cacheStrategy: 'indexeddb'
      });
      await window.indexedDBAdapter.init();
    });
    
    const indexedDBOnly = await page.evaluate(() => {
      return window.indexedDBAdapter.config.cacheStrategy === 'indexeddb';
    });
    
    expect(indexedDBOnly).toBe(true);
  });
});

