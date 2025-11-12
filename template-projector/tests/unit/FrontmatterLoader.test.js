/**
 * Unit tests for FrontmatterLoader
 */

import { jest, describe, test, beforeEach, expect } from '@jest/globals';
import { FrontmatterLoader } from '../../src/agents/FrontmatterLoader.js';

// Mock fetch
global.fetch = jest.fn();

describe('FrontmatterLoader', () => {
  let loader;

  beforeEach(() => {
    loader = new FrontmatterLoader();
    jest.clearAllMocks();
  });

  describe('loadContentIndex', () => {
    test('should load and parse JSONL content', async () => {
      const mockContent = `{"type":"document","id":"doc-1","title":"Test"}
{"type":"relationship","from":"doc-1","to":"doc-2","relType":"prerequisite"}
{"type":"rdf-triple","subject":"#doc-1","predicate":"rdfs:prerequisite","object":"#doc-2"}`;

      fetch.mockResolvedValue({
        ok: true,
        text: jest.fn().mockResolvedValue(mockContent)
      });

      await loader.loadContentIndex();

      expect(loader.loaded).toBe(true);
      expect(loader.documents.size).toBe(1);
      expect(loader.relationships.length).toBe(1);
      expect(loader.rdfTriples.length).toBe(1);
    });

    test('should handle fetch errors', async () => {
      fetch.mockResolvedValue({
        ok: false,
        statusText: 'Not Found'
      });

      await expect(loader.loadContentIndex()).rejects.toThrow('Failed to load content index');
    });

    test('should skip invalid JSON lines', async () => {
      const mockContent = `{"type":"document","id":"doc-1"}
invalid json line
{"type":"document","id":"doc-2"}`;

      fetch.mockResolvedValue({
        ok: true,
        text: jest.fn().mockResolvedValue(mockContent)
      });

      await loader.loadContentIndex();

      expect(loader.documents.size).toBe(2);
    });

    test('should not reload if already loaded', async () => {
      loader.loaded = true;
      loader.documents.set('doc-1', { id: 'doc-1' });

      await loader.loadContentIndex();

      expect(fetch).not.toHaveBeenCalled();
    });
  });

  describe('findByDimension', () => {
    beforeEach(async () => {
      loader.documents.set('doc-0D', { id: 'doc-0D', dimension: '0D' });
      loader.documents.set('doc-1D', { id: 'doc-1D', dimension: '1D' });
      loader.loaded = true;
    });

    test('should find documents by dimension', () => {
      const results = loader.findByDimension('0D');

      expect(results).toHaveLength(1);
      expect(results[0].id).toBe('doc-0D');
    });

    test('should normalize dimension format', () => {
      const results = loader.findByDimension('0d');

      expect(results).toHaveLength(1);
    });

    test('should throw if not loaded', () => {
      loader.loaded = false;

      expect(() => loader.findByDimension('0D')).toThrow('Content index not loaded');
    });
  });

  describe('findByTag', () => {
    beforeEach(() => {
      loader.documents.set('doc-1', { id: 'doc-1', tags: ['topology', '0D'] });
      loader.documents.set('doc-2', { id: 'doc-2', tags: ['system'] });
      loader.loaded = true;
    });

    test('should find documents by tag', () => {
      const results = loader.findByTag('topology');

      expect(results).toHaveLength(1);
      expect(results[0].id).toBe('doc-1');
    });

    test('should return empty array if no matches', () => {
      const results = loader.findByTag('nonexistent');

      expect(results).toHaveLength(0);
    });
  });

  describe('findByKeyword', () => {
    beforeEach(() => {
      loader.documents.set('doc-1', { id: 'doc-1', keywords: ['quantum', 'vacuum'] });
      loader.loaded = true;
    });

    test('should find documents by keyword', () => {
      const results = loader.findByKeyword('quantum');

      expect(results).toHaveLength(1);
      expect(results[0].id).toBe('doc-1');
    });
  });

  describe('getRelationships', () => {
    beforeEach(() => {
      loader.documents.set('doc-1', {
        id: 'doc-1',
        relationships: {
          prerequisites: ['doc-0'],
          enables: ['doc-2'],
          related: ['doc-3']
        }
      });
      loader.loaded = true;
    });

    test('should return relationships for document', () => {
      const rels = loader.getRelationships('doc-1');

      expect(rels.prerequisites).toEqual(['doc-0']);
      expect(rels.enables).toEqual(['doc-2']);
      expect(rels.related).toEqual(['doc-3']);
    });

    test('should return empty relationships if document not found', () => {
      const rels = loader.getRelationships('nonexistent');

      expect(rels.prerequisites).toEqual([]);
      expect(rels.enables).toEqual([]);
      expect(rels.related).toEqual([]);
    });
  });

  describe('getRDFTriples', () => {
    beforeEach(() => {
      loader.rdfTriples = [
        { subject: '#doc-1', predicate: 'rdfs:prerequisite', object: '#doc-2' },
        { subject: '#doc-2', predicate: 'rdfs:enables', object: '#doc-3' }
      ];
      loader.loaded = true;
    });

    test('should return RDF triples for document', () => {
      const triples = loader.getRDFTriples('doc-1');

      expect(triples).toHaveLength(1);
      expect(triples[0].subject).toBe('#doc-1');
    });

    test('should return triples where document is object', () => {
      const triples = loader.getRDFTriples('doc-3');

      expect(triples).toHaveLength(1);
      expect(triples[0].object).toBe('#doc-3');
    });
  });

  describe('findById', () => {
    beforeEach(() => {
      loader.documents.set('doc-1', { id: 'doc-1', title: 'Test' });
      loader.loaded = true;
    });

    test('should find document by ID', () => {
      const doc = loader.findById('doc-1');

      expect(doc).toBeDefined();
      expect(doc.title).toBe('Test');
    });

    test('should return null if not found', () => {
      const doc = loader.findById('nonexistent');

      expect(doc).toBeNull();
    });
  });

  describe('getKnowledgeGraph', () => {
    beforeEach(() => {
      loader.documents.set('doc-1', { id: 'doc-1' });
      loader.relationships = [{ from: 'doc-1', to: 'doc-2' }];
      loader.rdfTriples = [{ subject: '#doc-1', predicate: 'rdfs:seeAlso', object: '#doc-2' }];
      loader.loaded = true;
    });

    test('should return knowledge graph structure', () => {
      const graph = loader.getKnowledgeGraph();

      expect(graph.nodes).toHaveLength(1);
      expect(graph.edges).toHaveLength(1);
      expect(graph.triples).toHaveLength(1);
    });
  });

  describe('clearCache', () => {
    test('should clear all cached data', () => {
      loader.documents.set('doc-1', { id: 'doc-1' });
      loader.relationships = [{ from: 'doc-1', to: 'doc-2' }];
      loader.loaded = true;

      loader.clearCache();

      expect(loader.documents.size).toBe(0);
      expect(loader.relationships.length).toBe(0);
      expect(loader.loaded).toBe(false);
    });
  });
});

