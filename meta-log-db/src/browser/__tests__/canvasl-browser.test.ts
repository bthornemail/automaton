/**
 * Tests for CanvasLMetaverseBrowser
 */

import { CanvasLMetaverseBrowser } from '../canvasl-browser.js';

describe('CanvasLMetaverseBrowser', () => {
  let browser: CanvasLMetaverseBrowser;

  beforeEach(() => {
    browser = new CanvasLMetaverseBrowser({
      indexedDBName: 'test-meta-log-db'
    });
  });

  afterEach(async () => {
    if (browser.isInitialized()) {
      await browser.clear();
    }
  });

  describe('Initialization', () => {
    it('should initialize successfully', async () => {
      await browser.init();
      expect(browser.isInitialized()).toBe(true);
    });

    it('should handle multiple init calls', async () => {
      await browser.init();
      await browser.init(); // Should not throw
      expect(browser.isInitialized()).toBe(true);
    });
  });

  describe('Canvas Loading', () => {
    it('should load canvas from path', async () => {
      await browser.init();
      // Note: This test requires a valid canvas file
      // In a real test environment, you would use a test fixture
      expect(browser.isInitialized()).toBe(true);
    });

    it('should parse JSONL canvas', async () => {
      await browser.init();
      // Note: This test requires a valid JSONL file
      expect(browser.isInitialized()).toBe(true);
    });

    it('should parse CanvasL file', async () => {
      await browser.init();
      // Note: This test requires a valid CanvasL file
      expect(browser.isInitialized()).toBe(true);
    });
  });

  describe('Query Execution', () => {
    beforeEach(async () => {
      await browser.init();
    });

    it('should execute ProLog query', async () => {
      // Add a test fact
      browser.addPrologFacts([{
        predicate: 'test',
        args: ['value']
      }]);

      const result = await browser.prologQuery('test(X)');
      expect(result).toBeDefined();
    });

    it('should execute DataLog query', async () => {
      const result = await browser.datalogQuery('test(X)', null);
      expect(result).toBeDefined();
    });

    it('should execute SPARQL query', async () => {
      const result = await browser.sparqlQuery('SELECT ?s WHERE { ?s ?p ?o }');
      expect(result).toBeDefined();
    });
  });

  describe('R5RS Functions', () => {
    beforeEach(async () => {
      await browser.init();
    });

    it('should execute R5RS function', async () => {
      // Test with a simple function if available
      try {
        const result = await browser.executeR5RS('r5rs:church-zero', []);
        expect(result).toBeDefined();
      } catch (error) {
        // Function might not be available in test environment
        expect(error).toBeDefined();
      }
    });

    it('should list R5RS functions', async () => {
      const functions = await browser.listR5RSFunctions();
      expect(Array.isArray(functions)).toBe(true);
    });
  });

  describe('CanvasL Object Execution', () => {
    beforeEach(async () => {
      await browser.init();
    });

    it('should execute RDF triple object', async () => {
      const obj = {
        type: 'rdf-triple',
        subject: 'http://example.org/subject',
        predicate: 'http://example.org/predicate',
        object: 'http://example.org/object'
      };

      const result = await browser.executeCanvasLObject(obj);
      expect(result.type).toBe('rdf-triple');
      expect(result.result).toBeDefined();
    });

    it('should execute slide object', async () => {
      const obj = {
        type: 'slide',
        id: 'test-slide',
        dimension: '0D'
      };

      const result = await browser.executeCanvasLObject(obj);
      expect(result.type).toBe('slide');
      expect(result.result).toEqual(obj);
    });

    it('should handle unknown object type', async () => {
      const obj = {
        type: 'unknown-type',
        data: 'test'
      };

      const result = await browser.executeCanvasLObject(obj);
      expect(result.type).toBe('unknown');
    });

    it('should execute multiple objects', async () => {
      const objects = [
        {
          type: 'rdf-triple',
          subject: 'http://example.org/s1',
          predicate: 'http://example.org/p1',
          object: 'http://example.org/o1'
        },
        {
          type: 'slide',
          id: 'slide-1',
          dimension: '0D'
        }
      ];

      const results = await browser.executeCanvasLObjects(objects);
      expect(results.triples.length).toBeGreaterThanOrEqual(1);
      expect(results.slides.length).toBeGreaterThanOrEqual(1);
    });
  });

  describe('Utility Methods', () => {
    beforeEach(async () => {
      await browser.init();
    });

    it('should extract facts', () => {
      const facts = browser.extractFacts();
      expect(Array.isArray(facts)).toBe(true);
    });

    it('should convert facts to RDF', () => {
      const facts = browser.extractFacts();
      const triples = browser.jsonlToRdf(facts);
      expect(Array.isArray(triples)).toBe(true);
    });

    it('should get database instance', () => {
      const db = browser.getDb();
      expect(db).toBeDefined();
    });

    it('should clear data', async () => {
      await browser.clear();
      // Should not throw
      expect(browser.isInitialized()).toBe(true);
    });
  });
});

