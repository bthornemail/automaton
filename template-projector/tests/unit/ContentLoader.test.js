/**
 * Unit tests for ContentLoader
 */

import { jest } from '@jest/globals';
import { ContentLoader } from '../../src/agents/ContentLoader.js';
import { KernelLoader } from '../../src/agents/KernelLoader.js';
import { FrontmatterLoader } from '../../src/agents/FrontmatterLoader.js';

// Mock the loaders
jest.mock('../../src/agents/KernelLoader.js');
jest.mock('../../src/agents/FrontmatterLoader.js');

describe('ContentLoader', () => {
  let contentLoader;
  let mockKernelLoader;
  let mockFrontmatterLoader;

  beforeEach(() => {
    // Reset mocks
    jest.clearAllMocks();

    // Create mock instances
    mockKernelLoader = {
      loadKernel: jest.fn().mockResolvedValue([]),
      getAllEntries: jest.fn().mockReturnValue([]),
      findByDimension: jest.fn().mockReturnValue([]),
      getRelationships: jest.fn().mockReturnValue({ vertical: [], horizontal: [] }),
      clearCache: jest.fn()
    };

    mockFrontmatterLoader = {
      loadContentIndex: jest.fn().mockResolvedValue([]),
      getAllDocuments: jest.fn().mockReturnValue([]),
      findByDimension: jest.fn().mockReturnValue([]),
      findByTag: jest.fn().mockReturnValue([]),
      findByKeyword: jest.fn().mockReturnValue([]),
      getRelationships: jest.fn().mockReturnValue({ prerequisites: [], enables: [], related: [] }),
      getRDFTriples: jest.fn().mockReturnValue([]),
      findById: jest.fn().mockReturnValue(null),
      clearCache: jest.fn()
    };

    // Mock constructors
    KernelLoader.mockImplementation(() => mockKernelLoader);
    FrontmatterLoader.mockImplementation(() => mockFrontmatterLoader);

    contentLoader = new ContentLoader('/kernel.jsonl', '/content-index.jsonl');
  });

  describe('loadAll', () => {
    test('should load both kernel and content index', async () => {
      await contentLoader.loadAll();

      expect(mockKernelLoader.loadKernel).toHaveBeenCalled();
      expect(mockFrontmatterLoader.loadContentIndex).toHaveBeenCalled();
      expect(contentLoader.loaded).toBe(true);
    });

    test('should handle frontmatter loader failure gracefully', async () => {
      mockFrontmatterLoader.loadContentIndex.mockRejectedValue(new Error('Not found'));

      await expect(contentLoader.loadAll()).resolves.not.toThrow();
      expect(mockKernelLoader.loadKernel).toHaveBeenCalled();
    });
  });

  describe('findByDimension', () => {
    test('should query both kernel and frontmatter', async () => {
      await contentLoader.loadAll();

      const kernelEntries = [{ id: 'kernel-0D', dimension: '0D' }];
      const frontmatterDocs = [{ id: 'doc-0D', dimension: '0D' }];

      mockKernelLoader.findByDimension.mockReturnValue(kernelEntries);
      mockFrontmatterLoader.findByDimension.mockReturnValue(frontmatterDocs);

      const results = contentLoader.findByDimension('0D');

      expect(results).toHaveLength(2);
      expect(results).toContainEqual(kernelEntries[0]);
      expect(results).toContainEqual(frontmatterDocs[0]);
    });

    test('should throw if not loaded', () => {
      expect(() => contentLoader.findByDimension('0D')).toThrow('ContentLoader not loaded');
    });
  });

  describe('findByTag', () => {
    test('should query frontmatter loader', async () => {
      await contentLoader.loadAll();

      const docs = [{ id: 'doc-1', tags: ['topology'] }];
      mockFrontmatterLoader.findByTag.mockReturnValue(docs);

      const results = contentLoader.findByTag('topology');

      expect(mockFrontmatterLoader.findByTag).toHaveBeenCalledWith('topology');
      expect(results).toEqual(docs);
    });
  });

  describe('findByKeyword', () => {
    test('should query frontmatter loader', async () => {
      await contentLoader.loadAll();

      const docs = [{ id: 'doc-1', keywords: ['quantum'] }];
      mockFrontmatterLoader.findByKeyword.mockReturnValue(docs);

      const results = contentLoader.findByKeyword('quantum');

      expect(mockFrontmatterLoader.findByKeyword).toHaveBeenCalledWith('quantum');
      expect(results).toEqual(docs);
    });
  });

  describe('findByRelationship', () => {
    test('should find related documents', async () => {
      await contentLoader.loadAll();

      const relatedDoc = { id: 'doc-2', title: 'Related' };
      mockFrontmatterLoader.getRelationships.mockReturnValue({
        prerequisites: [],
        enables: ['doc-2'],
        related: []
      });
      mockFrontmatterLoader.findById.mockReturnValue(relatedDoc);

      const results = contentLoader.findByRelationship('enables', 'doc-1');

      expect(results).toContainEqual(relatedDoc);
    });
  });

  describe('getRelationships', () => {
    test('should return kernel relationships if available', async () => {
      await contentLoader.loadAll();

      const kernelRels = {
        vertical: [{ fromNode: '0D', toNode: '1D' }],
        horizontal: []
      };
      mockKernelLoader.getRelationships.mockReturnValue(kernelRels);

      const result = contentLoader.getRelationships('0D');

      expect(result).toEqual(kernelRels);
    });

    test('should return frontmatter relationships if kernel has none', async () => {
      await contentLoader.loadAll();

      mockKernelLoader.getRelationships.mockReturnValue({ vertical: [], horizontal: [] });
      mockFrontmatterLoader.getRelationships.mockReturnValue({
        prerequisites: ['doc-1'],
        enables: [],
        related: []
      });

      const result = contentLoader.getRelationships('doc-0');

      expect(result.prerequisites).toHaveLength(1);
    });
  });

  describe('getRDFTriples', () => {
    test('should return RDF triples from frontmatter', async () => {
      await contentLoader.loadAll();

      const triples = [
        { subject: '#doc-1', predicate: 'rdfs:prerequisite', object: '#doc-2' }
      ];
      mockFrontmatterLoader.getRDFTriples.mockReturnValue(triples);

      const results = contentLoader.getRDFTriples('doc-1');

      expect(results).toEqual(triples);
    });
  });

  describe('findById', () => {
    test('should check kernel first, then frontmatter', async () => {
      await contentLoader.loadAll();

      const kernelEntry = { id: 'entry-1', type: 'kernel' };
      mockKernelLoader.findById.mockReturnValue(kernelEntry);

      const result = contentLoader.findById('entry-1');

      expect(result).toEqual(kernelEntry);
      expect(mockFrontmatterLoader.findById).not.toHaveBeenCalled();
    });

    test('should check frontmatter if kernel returns null', async () => {
      await contentLoader.loadAll();

      const frontmatterDoc = { id: 'doc-1', type: 'document' };
      mockKernelLoader.findById.mockReturnValue(null);
      mockFrontmatterLoader.findById.mockReturnValue(frontmatterDoc);

      const result = contentLoader.findById('doc-1');

      expect(result).toEqual(frontmatterDoc);
    });
  });

  describe('getAllEntries', () => {
    test('should return entries from both sources', async () => {
      await contentLoader.loadAll();

      const kernelEntries = [{ id: 'kernel-1' }];
      const frontmatterDocs = [{ id: 'doc-1' }];

      mockKernelLoader.getAllEntries.mockReturnValue(kernelEntries);
      mockFrontmatterLoader.getAllDocuments.mockReturnValue(frontmatterDocs);

      const results = contentLoader.getAllEntries();

      expect(results).toHaveLength(2);
      expect(results).toContainEqual(kernelEntries[0]);
      expect(results).toContainEqual(frontmatterDocs[0]);
    });
  });

  describe('reload', () => {
    test('should clear cache and reload', async () => {
      await contentLoader.loadAll();
      contentLoader.loaded = true;

      await contentLoader.reload();

      expect(mockKernelLoader.clearCache).toHaveBeenCalled();
      expect(mockFrontmatterLoader.clearCache).toHaveBeenCalled();
      expect(contentLoader.loaded).toBe(true);
    });
  });
});

