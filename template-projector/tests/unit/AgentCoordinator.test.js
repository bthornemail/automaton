/**
 * Unit tests for AgentCoordinator
 */

import { jest, describe, test, beforeEach, expect } from '@jest/globals';
import { AgentCoordinator } from '../../src/agents/AgentCoordinator.js';
import { ContentLoader } from '../../src/agents/ContentLoader.js';
import { DimensionalAgent } from '../../src/agents/DimensionalAgent.js';

// Mock dependencies
jest.mock('../../src/agents/ContentLoader.js');
jest.mock('../../src/agents/DimensionalAgent.js');
jest.mock('../../src/agents/agents/0D-TopologyAgent.js', () => ({
  TopologyAgent0D: jest.fn()
}));

describe('AgentCoordinator', () => {
  let coordinator;
  let mockContentLoader;

  beforeEach(() => {
    jest.clearAllMocks();

    mockContentLoader = {
      loadAll: jest.fn().mockResolvedValue(),
      getAllEntries: jest.fn().mockReturnValue([]),
      findByDimension: jest.fn().mockReturnValue([]),
      kernelLoader: {
        setKernelUrl: jest.fn()
      },
      frontmatterLoader: {
        setContentIndexUrl: jest.fn()
      },
      reload: jest.fn().mockResolvedValue()
    };

    ContentLoader.mockImplementation(() => mockContentLoader);

    coordinator = new AgentCoordinator('/kernel.jsonl', '/content-index.jsonl');
    // Manually set contentLoader since mocks don't work with constructor assignment
    coordinator.contentLoader = mockContentLoader;
  });

  describe('constructor', () => {
    test('should initialize with ContentLoader', () => {
      expect(coordinator.contentLoader).toBeDefined();
      expect(coordinator.agents).toBeDefined();
      expect(coordinator.initialized).toBe(false);
    });
  });

  describe('init', () => {
    test('should load content and initialize agents', async () => {
      const mockAgent = {
        populateSlide: jest.fn().mockResolvedValue({})
      };
      DimensionalAgent.mockImplementation(() => mockAgent);

      await coordinator.init();

      expect(mockContentLoader.loadAll).toHaveBeenCalled();
      expect(coordinator.initialized).toBe(true);
      expect(coordinator.agents.size).toBe(8); // 0D-7D
    });

    test('should not reinitialize if already initialized', async () => {
      coordinator.initialized = true;
      await coordinator.init();

      expect(mockContentLoader.loadAll).not.toHaveBeenCalled();
    });
  });

  describe('getAgent', () => {
    beforeEach(async () => {
      const mockAgent = { name: 'TestAgent' };
      DimensionalAgent.mockImplementation(() => mockAgent);
      await coordinator.init();
    });

    test('should return agent for valid dimension', () => {
      const agent = coordinator.getAgent('0D');

      expect(agent).toBeDefined();
    });

    test('should normalize dimension format', () => {
      const agent1 = coordinator.getAgent('0d');
      const agent2 = coordinator.getAgent('0D');

      expect(agent1).toBeDefined();
      expect(agent2).toBeDefined();
    });

    test('should default to 0D for invalid dimension', () => {
      const agent = coordinator.getAgent('invalid');

      expect(agent).toBeDefined();
    });

    test('should throw if not initialized', () => {
      coordinator.initialized = false;

      expect(() => coordinator.getAgent('0D')).toThrow('AgentCoordinator not initialized');
    });
  });

  describe('populateSlide', () => {
    let mockAgent;

    beforeEach(async () => {
      mockAgent = {
        populateSlide: jest.fn().mockResolvedValue({
          id: 'slide-1',
          title: 'Populated Slide',
          _populated: true
        })
      };
      DimensionalAgent.mockImplementation(() => mockAgent);
      await coordinator.init();
    });

    test('should populate slide using appropriate agent', async () => {
      const slide = { id: 'slide-1', dimension: '0D' };

      const populated = await coordinator.populateSlide(slide);

      expect(mockAgent.populateSlide).toHaveBeenCalled();
      expect(populated._populated).toBe(true);
    });

    test('should use 0D agent as fallback', async () => {
      const slide = { id: 'slide-1', dimension: 'invalid' };

      const populated = await coordinator.populateSlide(slide);

      expect(mockAgent.populateSlide).toHaveBeenCalled();
    });

    test('should return unmodified slide if no agents available', async () => {
      coordinator.agents.clear();
      const slide = { id: 'slide-1', dimension: '0D' };

      const populated = await coordinator.populateSlide(slide);

      expect(populated).toEqual(slide);
    });
  });

  describe('populateAll', () => {
    let mockAgent;

    beforeEach(async () => {
      mockAgent = {
        populateSlide: jest.fn().mockImplementation((slide) =>
          Promise.resolve({ ...slide, _populated: true })
        )
      };
      DimensionalAgent.mockImplementation(() => mockAgent);
      await coordinator.init();
    });

    test('should populate all slides', async () => {
      const slides = [
        { id: 'slide-1', dimension: '0D' },
        { id: 'slide-2', dimension: '1D' }
      ];

      const populated = await coordinator.populateAll(slides);

      expect(populated).toHaveLength(2);
      expect(populated.every(s => s._populated)).toBe(true);
    });

    test('should group slides by dimension', async () => {
      const slides = [
        { id: 'slide-1', dimension: '0D' },
        { id: 'slide-2', dimension: '0D' },
        { id: 'slide-3', dimension: '1D' }
      ];

      await coordinator.populateAll(slides);

      expect(mockAgent.populateSlide).toHaveBeenCalledTimes(3);
    });

    test('should handle errors gracefully', async () => {
      mockAgent.populateSlide.mockRejectedValueOnce(new Error('Test error'));
      const slides = [
        { id: 'slide-1', dimension: '0D' },
        { id: 'slide-2', dimension: '0D' }
      ];

      const populated = await coordinator.populateAll(slides);

      expect(populated).toHaveLength(2);
      expect(populated[0]).toEqual(slides[0]); // Unmodified on error
    });
  });

  describe('getPopulationStats', () => {
    test('should calculate population statistics', () => {
      const slides = [
        { id: 'slide-1', _populated: true },
        { id: 'slide-2', _populated: true },
        { id: 'slide-3', _populated: false }
      ];

      const stats = coordinator.getPopulationStats(slides);

      expect(stats.populated).toBe(2);
      expect(stats.unpopulated).toBe(1);
      expect(stats.percentage).toBe('66.7');
    });
  });

  describe('reloadContent', () => {
    test('should reload content and reinitialize', async () => {
      mockContentLoader.reload = jest.fn().mockResolvedValue();
      coordinator.initialized = true;

      await coordinator.reloadContent();

      expect(mockContentLoader.reload).toHaveBeenCalled();
      expect(coordinator.initialized).toBe(true);
    });
  });
});

