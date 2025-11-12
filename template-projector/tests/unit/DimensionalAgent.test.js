/**
 * Unit tests for DimensionalAgent
 */

import { jest, describe, test, beforeEach, expect } from '@jest/globals';
import { DimensionalAgent } from '../../src/agents/DimensionalAgent.js';

// Mock ContentLoader
jest.mock('../../src/agents/ContentLoader.js', () => ({
  ContentLoader: jest.fn()
}));

describe('DimensionalAgent', () => {
  let agent;
  let mockContentLoader;

  beforeEach(() => {
    mockContentLoader = {
      loadAll: jest.fn().mockResolvedValue(),
      getAllEntries: jest.fn().mockReturnValue([]),
      findByDimension: jest.fn().mockReturnValue([]),
      findByTag: jest.fn().mockReturnValue([]),
      findByKeyword: jest.fn().mockReturnValue([]),
      getRelationships: jest.fn().mockReturnValue({
        vertical: [],
        horizontal: [],
        prerequisites: [],
        enables: [],
        related: []
      }),
      getRDFTriples: jest.fn().mockReturnValue([])
    };

    agent = new DimensionalAgent('0D', mockContentLoader);
    // Ensure contentLoader is set
    agent.contentLoader = mockContentLoader;
  });

  describe('constructor', () => {
    test('should set dimension and name', () => {
      expect(agent.dimension).toBe('0D');
      expect(agent.name).toBe('0D-Agent');
    });
  });

  describe('matchContentEntries', () => {
    beforeEach(() => {
      mockContentLoader.loadAll.mockResolvedValue();
      mockContentLoader.getAllEntries.mockReturnValue([]);
    });

    test('should find entries by dimension', async () => {
      const entries = [
        { id: '0D-topology', dimension: '0D' },
        { id: '0D-system', dimension: '0D' }
      ];
      mockContentLoader.findByDimension.mockReturnValue(entries);

      const matches = agent.matchContentEntries({ dimension: '0D' });

      expect(matches).toHaveLength(2);
    });

    test('should find entries by tag variations', async () => {
      const tagEntries = [{ id: 'doc-1', tags: ['0D-topology'] }];
      mockContentLoader.findByTag.mockReturnValue(tagEntries);

      const matches = agent.matchContentEntries({ dimension: '0D' });

      expect(mockContentLoader.findByTag).toHaveBeenCalled();
    });

    test('should find entries by keyword', async () => {
      const keywordEntries = [{ id: 'doc-1', keywords: ['0d'] }];
      mockContentLoader.findByKeyword.mockReturnValue(keywordEntries);

      const matches = agent.matchContentEntries({ dimension: '0D' });

      expect(mockContentLoader.findByKeyword).toHaveBeenCalled();
    });

    test('should remove duplicates', async () => {
      const entries = [
        { id: 'doc-1', dimension: '0D' },
        { id: 'doc-1', dimension: '0D' } // Duplicate
      ];
      mockContentLoader.findByDimension.mockReturnValue(entries);

      const matches = agent.matchContentEntries({ dimension: '0D' });

      expect(matches).toHaveLength(1);
    });
  });

  describe('populateSlide', () => {
    test('should populate slide with content', async () => {
      const slide = { id: 'slide-1', dimension: '0D' };
      const entries = [
        {
          id: 'doc-1',
          type: 'document',
          title: 'Test Document',
          description: 'Test description',
          body: 'Test content',
          frontmatter: { title: 'Test Document' }
        }
      ];

      mockContentLoader.getAllEntries.mockReturnValue(entries);
      mockContentLoader.findByDimension.mockReturnValue(entries);

      const populated = await agent.populateSlide(slide);

      expect(populated.title).toBe('Test Document');
      expect(populated.content).toContain('Test content');
      expect(populated._populated).toBe(true);
    });

    test('should handle empty entries', async () => {
      const slide = { id: 'slide-1', dimension: '0D' };
      mockContentLoader.findByDimension.mockReturnValue([]);

      const populated = await agent.populateSlide(slide);

      expect(populated).toEqual(slide);
      expect(populated._populated).toBeUndefined();
    });

    test('should merge existing content', async () => {
      const slide = {
        id: 'slide-1',
        dimension: '0D',
        title: 'Existing Title',
        content: 'Existing content'
      };
      const entries = [
        {
          id: 'doc-1',
          type: 'document',
          title: 'New Title',
          body: 'New content'
        }
      ];

      mockContentLoader.findByDimension.mockReturnValue(entries);

      const populated = await agent.populateSlide(slide);

      expect(populated.title).toBe('Existing Title');
      expect(populated.subtitle).toBe('New Title');
      expect(populated.content).toContain('Existing content');
      expect(populated.content).toContain('New content');
    });
  });

  describe('extractText', () => {
    test('should extract text from frontmatter documents', () => {
      const entries = [
        {
          type: 'document',
          title: 'Document Title',
          description: 'Document description',
          body: 'Document body content',
          frontmatter: { title: 'Document Title' }
        }
      ];

      const result = agent.extractText(entries);

      expect(result.title).toBe('Document Title');
      expect(result.description).toBe('Document description');
      expect(result.content).toContain('Document body content');
    });

    test('should extract text from kernel entries', () => {
      const entries = [
        {
          id: '0D-topology',
          text: '# Topology Title\n\nTopology content'
        }
      ];

      const result = agent.extractText(entries);

      expect(result.title).toBe('Topology Title');
      expect(result.content).toContain('Topology content');
    });

    test('should combine multiple entries', () => {
      const entries = [
        { type: 'document', title: 'Doc 1', body: 'Content 1' },
        { type: 'document', title: 'Doc 2', body: 'Content 2' }
      ];

      const result = agent.extractText(entries);

      expect(result.content).toContain('Content 1');
      expect(result.content).toContain('Content 2');
    });
  });

  describe('extractRDF', () => {
    test('should extract RDF triples from relationships', () => {
      const entries = [{ id: 'doc-1' }];
      mockContentLoader.getRelationships.mockReturnValue({
        prerequisites: [{ toNode: 'doc-2' }],
        enables: [{ toNode: 'doc-3' }]
      });

      const triples = agent.extractRDF(entries, {});

      expect(triples.length).toBeGreaterThan(0);
      expect(triples.some(t => t.predicate === 'rdfs:prerequisite')).toBe(true);
      expect(triples.some(t => t.predicate === 'rdfs:enables')).toBe(true);
    });

    test('should include direct RDF triples from ContentLoader', () => {
      const entries = [{ id: 'doc-1' }];
      const directTriples = [
        { subject: '#doc-1', predicate: 'rdfs:seeAlso', object: '#doc-2' }
      ];
      mockContentLoader.getRDFTriples.mockReturnValue(directTriples);

      const triples = agent.extractRDF(entries, {});

      expect(triples).toContainEqual(directTriples[0]);
    });
  });

  describe('extractUIComponents', () => {
    test('should extract images from markdown', () => {
      const entries = [
        {
          body: '![Alt text](image.png "Title")'
        }
      ];

      const components = agent.extractUIComponents(entries);

      expect(components.some(c => c.type === 'image')).toBe(true);
    });

    test('should extract code blocks', () => {
      const entries = [
        {
          body: '```javascript\nconst x = 1;\n```'
        }
      ];

      const components = agent.extractUIComponents(entries);

      expect(components.some(c => c.type === 'code-block')).toBe(true);
    });

    test('should extract diagrams', () => {
      const entries = [
        {
          body: '```mermaid\ngraph TD\nA --> B\n```'
        }
      ];

      const components = agent.extractUIComponents(entries);

      expect(components.some(c => c.type === 'diagram')).toBe(true);
    });

    test('should extract quotes', () => {
      const entries = [
        {
          body: '> This is a quote'
        }
      ];

      const components = agent.extractUIComponents(entries);

      expect(components.some(c => c.type === 'quote')).toBe(true);
    });

    test('should avoid duplicates', () => {
      const slide = { uiComponents: [{ type: 'image', url: 'image.png' }] };
      const entries = [
        {
          body: '![Alt](image.png)'
        }
      ];

      const components = agent.extractUIComponents(entries, slide);

      // Should not add duplicate image
      const images = components.filter(c => c.type === 'image' && c.url === 'image.png');
      expect(images.length).toBeLessThanOrEqual(1);
    });
  });
});

