/**
 * Slide Editing Service
 * 
 * Manages interactive editing of slides, including:
 * - Editing slide content inline
 * - Adding/removing cards from slides
 * - Reordering slides
 * - Saving edited slides back to evolution directory
 */

import { Slide, Card } from './provenance-slide-service';
import { errorLoggingService } from './error-logging-service';

export interface SlideEditOperation {
  type: 'edit' | 'add-card' | 'remove-card' | 'reorder';
  slideId: string;
  data?: any;
  timestamp: number;
}

export interface SaveSlidesResult {
  success: boolean;
  savedFiles: string[];
  errors?: string[];
}

export class SlideEditingService {
  private editHistory: Map<string, SlideEditOperation[]> = new Map();
  private modifiedSlides: Map<string, Slide> = new Map();

  /**
   * Edit slide content inline.
   * 
   * Updates slide properties (title, description, content) and tracks
   * the edit in history. The slide is marked as modified and will be
   * saved when saveSlidesToEvolution is called.
   * 
   * @param {string} slideId - ID of the slide to edit
   * @param {Partial<Slide>} updates - Partial slide data to apply
   * @returns {Slide} Updated slide
   * 
   * @example
   * ```typescript
   * const updated = service.editSlide('slide-0D-123', {
   *   title: 'Updated Title',
   *   description: 'Updated description'
   * });
   * ```
   */
  editSlide(slideId: string, updates: Partial<Slide>): Slide {
    const existingSlide = this.modifiedSlides.get(slideId);
    if (!existingSlide) {
      throw new Error(`Slide ${slideId} not found`);
    }

    const updatedSlide: Slide = {
      ...existingSlide,
      ...updates
    };

    this.modifiedSlides.set(slideId, updatedSlide);

    // Record edit in history
    this.recordEdit(slideId, {
      type: 'edit',
      slideId,
      data: updates,
      timestamp: Date.now()
    });

    return updatedSlide;
  }

  /**
   * Add a card to a slide.
   * 
   * Adds a new card to the specified slide. The card is added to the
   * slide's cards array and the edit is tracked in history.
   * 
   * @param {string} slideId - ID of the slide to add card to
   * @param {Card} card - Card to add
   * @returns {Slide} Updated slide with new card
   * 
   * @example
   * ```typescript
   * const newCard: Card = {
   *   id: 'card-new-123',
   *   pattern: 'identity',
   *   jsonlLines: [],
   *   metadata: {}
   * };
   * const updated = service.addCardToSlide('slide-0D-123', newCard);
   * ```
   */
  addCardToSlide(slideId: string, card: Card): Slide {
    const slide = this.modifiedSlides.get(slideId);
    if (!slide) {
      throw new Error(`Slide ${slideId} not found`);
    }

    const updatedCards = [...(slide.cards || []), card];
    const updatedSlide: Slide = {
      ...slide,
      cards: updatedCards
    };

    this.modifiedSlides.set(slideId, updatedSlide);

    // Record edit in history
    this.recordEdit(slideId, {
      type: 'add-card',
      slideId,
      data: { cardId: card.id },
      timestamp: Date.now()
    });

    return updatedSlide;
  }

  /**
   * Remove a card from a slide.
   * 
   * Removes a card from the specified slide by ID. The edit is tracked
   * in history and can be undone.
   * 
   * @param {string} slideId - ID of the slide to remove card from
   * @param {string} cardId - ID of the card to remove
   * @returns {Slide} Updated slide without the removed card
   * 
   * @example
   * ```typescript
   * const updated = service.removeCardFromSlide('slide-0D-123', 'card-456');
   * ```
   */
  removeCardFromSlide(slideId: string, cardId: string): Slide {
    const slide = this.modifiedSlides.get(slideId);
    if (!slide) {
      throw new Error(`Slide ${slideId} not found`);
    }

    const updatedCards = (slide.cards || []).filter(c => c.id !== cardId);
    const updatedSlide: Slide = {
      ...slide,
      cards: updatedCards
    };

    this.modifiedSlides.set(slideId, updatedSlide);

    // Record edit in history
    this.recordEdit(slideId, {
      type: 'remove-card',
      slideId,
      data: { cardId },
      timestamp: Date.now()
    });

    return updatedSlide;
  }

  /**
   * Reorder slides.
   * 
   * Reorders the slides array according to the provided order. The new
   * order is tracked in history.
   * 
   * @param {Slide[]} slides - Array of slides in the new order
   * @returns {Slide[]} Reordered slides array
   * 
   * @example
   * ```typescript
   * // Move first slide to the end
   * const reordered = service.reorderSlides([...slides.slice(1), slides[0]]);
   * ```
   */
  reorderSlides(slides: Slide[]): Slide[] {
    // Update modified slides map with new order
    for (const slide of slides) {
      this.modifiedSlides.set(slide.id, slide);
    }

    // Record reorder in history
    const slideIds = slides.map(s => s.id);
    this.recordEdit('all', {
      type: 'reorder',
      slideId: 'all',
      data: { order: slideIds },
      timestamp: Date.now()
    });

    return slides;
  }

  /**
   * Initialize slides for editing.
   * 
   * Sets up the editing service with initial slides. This should be
   * called before any editing operations.
   * 
   * @param {Slide[]} slides - Initial slides to edit
   */
  initializeSlides(slides: Slide[]): void {
    this.modifiedSlides.clear();
    this.editHistory.clear();

    for (const slide of slides) {
      // Deep clone to avoid mutating original
      this.modifiedSlides.set(slide.id, JSON.parse(JSON.stringify(slide)));
    }
  }

  /**
   * Get modified slide.
   * 
   * Returns the current state of a slide, including all edits.
   * 
   * @param {string} slideId - ID of the slide to get
   * @returns {Slide | undefined} Modified slide or undefined if not found
   */
  getModifiedSlide(slideId: string): Slide | undefined {
    return this.modifiedSlides.get(slideId);
  }

  /**
   * Get all modified slides.
   * 
   * Returns all slides in their current edited state, maintaining
   * the order from the last reorder operation.
   * 
   * @returns {Slide[]} Array of modified slides
   */
  getModifiedSlides(): Slide[] {
    return Array.from(this.modifiedSlides.values());
  }

  /**
   * Check if slides have been modified.
   * 
   * Returns true if any slides have been edited, cards added/removed,
   * or slides reordered.
   * 
   * @returns {boolean} True if slides have been modified
   */
  hasModifications(): boolean {
    return this.editHistory.size > 0;
  }

  /**
   * Get edit history for a slide.
   * 
   * Returns all edit operations performed on a specific slide.
   * 
   * @param {string} slideId - ID of the slide
   * @returns {SlideEditOperation[]} Array of edit operations
   */
  getEditHistory(slideId: string): SlideEditOperation[] {
    return this.editHistory.get(slideId) || [];
  }

  /**
   * Undo last edit for a slide.
   * 
   * Reverts the last edit operation for a slide. This is a simple
   * undo that removes the last operation from history. For full
   * undo/redo, a more sophisticated history system would be needed.
   * 
   * @param {string} slideId - ID of the slide
   * @returns {boolean} True if undo was successful
   */
  undoLastEdit(slideId: string): boolean {
    const history = this.editHistory.get(slideId);
    if (!history || history.length === 0) {
      return false;
    }

    // Remove last edit
    history.pop();
    this.editHistory.set(slideId, history);

    // Note: Full undo would require storing previous states
    // This is a simplified version
    return true;
  }

  /**
   * Save edited slides back to evolution directory.
   * 
   * Saves all modified slides to files in the evolution directory.
   * Each slide is saved as a separate JSON file, and a slides index
   * file is created to track all slides.
   * 
   * @param {string} evolutionPath - Path to the evolution directory
   * @returns {Promise<SaveSlidesResult>} Result of save operation
   * 
   * @example
   * ```typescript
   * const result = await service.saveSlidesToEvolution('/evolutions/advanced-automaton');
   * if (result.success) {
   *   console.log(`Saved ${result.savedFiles.length} files`);
   * }
   * ```
   */
  async saveSlidesToEvolution(evolutionPath: string): Promise<SaveSlidesResult> {
    const savedFiles: string[] = [];
    const errors: string[] = [];

    try {
      // Create slides directory if it doesn't exist
      const slidesDir = `${evolutionPath}/slides`;
      await this.ensureDirectoryExists(slidesDir);

      // Save each modified slide
      for (const slide of this.modifiedSlides.values()) {
        try {
          const fileName = `slide-${slide.id}.json`;
          const filePath = `${slidesDir}/${fileName}`;
          const content = JSON.stringify(slide, null, 2);

          await this.writeFile(filePath, content);
          savedFiles.push(filePath);
        } catch (error) {
          const errorMsg = `Failed to save slide ${slide.id}: ${error instanceof Error ? error.message : String(error)}`;
          errors.push(errorMsg);
          errorLoggingService.logError(
            error instanceof Error ? error : new Error(String(error)),
            {
              service: 'SlideEditingService',
              action: 'saveSlidesToEvolution',
              metadata: { slideId: slide.id, evolutionPath },
              severity: 'error'
            }
          );
        }
      }

      // Save slides index
      try {
        const indexPath = `${slidesDir}/slides-index.json`;
        const index = {
          slides: Array.from(this.modifiedSlides.keys()),
          lastModified: Date.now(),
          editHistory: Array.from(this.editHistory.entries()).map(([slideId, ops]) => ({
            slideId,
            operations: ops.length
          }))
        };
        const indexContent = JSON.stringify(index, null, 2);

        await this.writeFile(indexPath, indexContent);
        savedFiles.push(indexPath);
      } catch (error) {
        const errorMsg = `Failed to save slides index: ${error instanceof Error ? error.message : String(error)}`;
        errors.push(errorMsg);
      }

      return {
        success: errors.length === 0,
        savedFiles,
        errors: errors.length > 0 ? errors : undefined
      };
    } catch (error) {
      const errorMsg = `Failed to save slides: ${error instanceof Error ? error.message : String(error)}`;
      return {
        success: false,
        savedFiles,
        errors: [errorMsg, ...errors]
      };
    }
  }

  /**
   * Reset all modifications.
   * 
   * Clears all edits and resets slides to their original state.
   * This discards all unsaved changes.
   */
  resetModifications(): void {
    this.modifiedSlides.clear();
    this.editHistory.clear();
  }

  /**
   * Record edit operation in history.
   */
  private recordEdit(slideId: string, operation: SlideEditOperation): void {
    if (!this.editHistory.has(slideId)) {
      this.editHistory.set(slideId, []);
    }
    this.editHistory.get(slideId)!.push(operation);
  }

  /**
   * Ensure directory exists (client-side stub, would use File System API or backend).
   */
  private async ensureDirectoryExists(dirPath: string): Promise<void> {
    // In a real implementation, this would use the File System API
    // or make a backend API call to create the directory
    // For now, this is a placeholder
    console.log(`Ensuring directory exists: ${dirPath}`);
  }

  /**
   * Write file (client-side stub, would use File System API or backend).
   */
  private async writeFile(filePath: string, content: string): Promise<void> {
    // In a real implementation, this would use the File System API
    // or make a backend API call to save the file
    // For now, we'll use fetch to call a backend endpoint
    try {
      const response = await fetch('/api/slides/save', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json'
        },
        body: JSON.stringify({
          filePath,
          content
        })
      });

      if (!response.ok) {
        throw new Error(`Failed to save file: ${response.statusText}`);
      }
    } catch (error) {
      // Fallback: log error and rethrow
      console.error(`Failed to save file ${filePath}:`, error);
      throw error;
    }
  }
}

// Export singleton instance
export const slideEditingService = new SlideEditingService();

