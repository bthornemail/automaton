/**
 * Slide Editing Service Tests
 * 
 * Tests for interactive slide editing functionality.
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import { slideEditingService, SlideEditOperation } from '../slide-editing-service';
import { Slide, Card } from '../provenance-slide-service';

describe('SlideEditingService', () => {
  let mockSlides: Slide[];
  let mockSlide: Slide;
  let mockCard: Card;

  beforeEach(() => {
    // Reset service state
    slideEditingService['modifiedSlides'].clear();
    slideEditingService['editHistory'].clear();

    mockCard = {
      id: 'card-1',
      pattern: 'identity',
      jsonlLines: [{ id: 'line-1', type: 'node' }],
      metadata: {
        churchEncoding: 'λf.λx.x',
        bqfCoefficients: [0, 0, 0]
      }
    };

    mockSlide = {
      id: 'slide-1',
      type: 'slide',
      title: 'Test Slide',
      dimension: '0D',
      description: 'Test description',
      content: 'Test content',
      cards: [mockCard]
    };

    mockSlides = [mockSlide];
  });

  describe('initializeSlides', () => {
    it('should initialize slides for editing', () => {
      slideEditingService.initializeSlides(mockSlides);
      
      const modified = slideEditingService.getModifiedSlide('slide-1');
      expect(modified).toBeDefined();
      expect(modified?.title).toBe('Test Slide');
    });

    it('should deep clone slides to avoid mutation', () => {
      slideEditingService.initializeSlides(mockSlides);
      
      // Modify original
      mockSlides[0].title = 'Modified';
      
      // Modified slide should not be affected
      const modified = slideEditingService.getModifiedSlide('slide-1');
      expect(modified?.title).toBe('Test Slide');
    });
  });

  describe('editSlide', () => {
    beforeEach(() => {
      slideEditingService.initializeSlides(mockSlides);
    });

    it('should update slide title', () => {
      const updated = slideEditingService.editSlide('slide-1', {
        title: 'Updated Title'
      });

      expect(updated.title).toBe('Updated Title');
    });

    it('should update slide description', () => {
      const updated = slideEditingService.editSlide('slide-1', {
        description: 'Updated description'
      });

      expect(updated.description).toBe('Updated description');
    });

    it('should update slide content', () => {
      const updated = slideEditingService.editSlide('slide-1', {
        content: 'Updated content'
      });

      expect(updated.content).toBe('Updated content');
    });

    it('should update multiple fields', () => {
      const updated = slideEditingService.editSlide('slide-1', {
        title: 'New Title',
        description: 'New Description',
        content: 'New Content'
      });

      expect(updated.title).toBe('New Title');
      expect(updated.description).toBe('New Description');
      expect(updated.content).toBe('New Content');
    });

    it('should record edit in history', () => {
      slideEditingService.editSlide('slide-1', {
        title: 'Updated'
      });

      const history = slideEditingService.getEditHistory('slide-1');
      expect(history.length).toBe(1);
      expect(history[0].type).toBe('edit');
      expect(history[0].data.title).toBe('Updated');
    });

    it('should throw error if slide not found', () => {
      expect(() => {
        slideEditingService.editSlide('non-existent', { title: 'Test' });
      }).toThrow('Slide non-existent not found');
    });
  });

  describe('addCardToSlide', () => {
    beforeEach(() => {
      slideEditingService.initializeSlides(mockSlides);
    });

    it('should add card to slide', () => {
      const newCard: Card = {
        id: 'card-2',
        pattern: 'successor',
        jsonlLines: [],
        metadata: {}
      };

      const updated = slideEditingService.addCardToSlide('slide-1', newCard);
      
      expect(updated.cards).toHaveLength(2);
      expect(updated.cards?.find(c => c.id === 'card-2')).toBeDefined();
    });

    it('should record add operation in history', () => {
      const newCard: Card = {
        id: 'card-2',
        pattern: 'successor',
        jsonlLines: [],
        metadata: {}
      };

      slideEditingService.addCardToSlide('slide-1', newCard);
      
      const history = slideEditingService.getEditHistory('slide-1');
      expect(history.length).toBe(1);
      expect(history[0].type).toBe('add-card');
      expect(history[0].data.cardId).toBe('card-2');
    });
  });

  describe('removeCardFromSlide', () => {
    beforeEach(() => {
      slideEditingService.initializeSlides(mockSlides);
    });

    it('should remove card from slide', () => {
      const updated = slideEditingService.removeCardFromSlide('slide-1', 'card-1');
      
      expect(updated.cards).toHaveLength(0);
    });

    it('should record remove operation in history', () => {
      slideEditingService.removeCardFromSlide('slide-1', 'card-1');
      
      const history = slideEditingService.getEditHistory('slide-1');
      expect(history.length).toBe(1);
      expect(history[0].type).toBe('remove-card');
      expect(history[0].data.cardId).toBe('card-1');
    });
  });

  describe('reorderSlides', () => {
    beforeEach(() => {
      const slide2: Slide = {
        id: 'slide-2',
        type: 'slide',
        title: 'Slide 2',
        cards: []
      };
      mockSlides = [mockSlide, slide2];
      slideEditingService.initializeSlides(mockSlides);
    });

    it('should reorder slides', () => {
      const reordered = slideEditingService.reorderSlides([mockSlides[1], mockSlides[0]]);
      
      expect(reordered[0].id).toBe('slide-2');
      expect(reordered[1].id).toBe('slide-1');
    });

    it('should record reorder operation in history', () => {
      slideEditingService.reorderSlides([mockSlides[1], mockSlides[0]]);
      
      const history = slideEditingService.getEditHistory('all');
      expect(history.length).toBe(1);
      expect(history[0].type).toBe('reorder');
      expect(history[0].data.order).toEqual(['slide-2', 'slide-1']);
    });
  });

  describe('hasModifications', () => {
    it('should return false when no modifications', () => {
      slideEditingService.initializeSlides(mockSlides);
      expect(slideEditingService.hasModifications()).toBe(false);
    });

    it('should return true after editing', () => {
      slideEditingService.initializeSlides(mockSlides);
      slideEditingService.editSlide('slide-1', { title: 'Updated' });
      expect(slideEditingService.hasModifications()).toBe(true);
    });

    it('should return true after adding card', () => {
      slideEditingService.initializeSlides(mockSlides);
      const newCard: Card = {
        id: 'card-2',
        pattern: 'test',
        jsonlLines: [],
        metadata: {}
      };
      slideEditingService.addCardToSlide('slide-1', newCard);
      expect(slideEditingService.hasModifications()).toBe(true);
    });
  });

  describe('getModifiedSlides', () => {
    it('should return all modified slides', () => {
      slideEditingService.initializeSlides(mockSlides);
      slideEditingService.editSlide('slide-1', { title: 'Updated' });
      
      const modified = slideEditingService.getModifiedSlides();
      expect(modified).toHaveLength(1);
      expect(modified[0].title).toBe('Updated');
    });
  });

  describe('undoLastEdit', () => {
    beforeEach(() => {
      slideEditingService.initializeSlides(mockSlides);
    });

    it('should remove last edit from history', () => {
      slideEditingService.editSlide('slide-1', { title: 'Edit 1' });
      slideEditingService.editSlide('slide-1', { title: 'Edit 2' });
      
      const beforeUndo = slideEditingService.getEditHistory('slide-1');
      expect(beforeUndo.length).toBe(2);
      
      slideEditingService.undoLastEdit('slide-1');
      
      const afterUndo = slideEditingService.getEditHistory('slide-1');
      expect(afterUndo.length).toBe(1);
    });

    it('should return false if no history', () => {
      const result = slideEditingService.undoLastEdit('slide-1');
      expect(result).toBe(false);
    });
  });

  describe('resetModifications', () => {
    it('should clear all modifications', () => {
      slideEditingService.initializeSlides(mockSlides);
      slideEditingService.editSlide('slide-1', { title: 'Updated' });
      
      expect(slideEditingService.hasModifications()).toBe(true);
      
      slideEditingService.resetModifications();
      
      expect(slideEditingService.hasModifications()).toBe(false);
      expect(slideEditingService.getModifiedSlides()).toHaveLength(0);
    });
  });

  describe('saveSlidesToEvolution', () => {
    beforeEach(() => {
      // Mock fetch for file saving
      global.fetch = vi.fn().mockResolvedValue({
        ok: true,
        statusText: 'OK'
      } as Response);
    });

    it('should save slides to evolution directory', async () => {
      slideEditingService.initializeSlides(mockSlides);
      slideEditingService.editSlide('slide-1', { title: 'Updated' });
      
      const result = await slideEditingService.saveSlidesToEvolution('/evolutions/test');
      
      expect(result.success).toBe(true);
      expect(result.savedFiles.length).toBeGreaterThan(0);
    });

    it('should handle save errors gracefully', async () => {
      global.fetch = vi.fn().mockRejectedValue(new Error('Save failed'));
      
      slideEditingService.initializeSlides(mockSlides);
      
      const result = await slideEditingService.saveSlidesToEvolution('/evolutions/test');
      
      expect(result.success).toBe(false);
      expect(result.errors).toBeDefined();
    });
  });
});

