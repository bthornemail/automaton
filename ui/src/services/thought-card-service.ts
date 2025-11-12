/**
 * Thought Card Service
 * 
 * Manages 3D thought cards for avatars, rendered using offscreen canvas.
 */

import { ProvenanceNode } from './provenance-slide-service';

export interface ThoughtCard {
  id: string;
  avatarId: string;
  content: string;
  position: 'above' | 'left' | 'right' | 'behind';
  offset: [number, number, number];
  size: [number, number];
  opacity: number;
  metadata?: {
    timestamp: number;
    thoughtProcess?: string;
    agentId?: string;
    dimension?: string;
    pattern?: string;
  };
}

export class ThoughtCardService {
  private cards: Map<string, ThoughtCard> = new Map();
  private cardTextures: Map<string, HTMLCanvasElement> = new Map();

  /**
   * Create a thought card for an avatar.
   * 
   * Creates a 3D thought card that will be rendered near an avatar in the 3D scene.
   * The card displays agent thought processes extracted from node metadata. Cards
   * are positioned relative to the avatar and rendered as billboard planes that
   * always face the camera.
   * 
   * @param {string} avatarId - ID of the avatar this card belongs to
   * @param {string} content - Text content to display on the card
   * @param {Object} [options={}] - Configuration options for the card
   * @param {'above' | 'left' | 'right' | 'behind'} [options.position='above'] - Position relative to avatar
   * @param {[number, number, number]} [options.offset] - Custom offset from avatar position
   * @param {[number, number]} [options.size=[2, 1]] - Card size in 3D space
   * @param {number} [options.opacity=0.9] - Card opacity (0-1)
   * @param {ThoughtCard['metadata']} [options.metadata] - Additional metadata
   * @returns {ThoughtCard} Created thought card object
   * 
   * @example
   * ```typescript
   * const card = thoughtCardService.createThoughtCard('avatar-123', 'Processing...', {
   *   position: 'above',
   *   size: [2, 1],
   *   opacity: 0.9
   * });
   * ```
   */
  createThoughtCard(
    avatarId: string,
    content: string,
    options: {
      position?: 'above' | 'left' | 'right' | 'behind';
      offset?: [number, number, number];
      size?: [number, number];
      opacity?: number;
      metadata?: ThoughtCard['metadata'];
    } = {}
  ): ThoughtCard {
    const cardId = `thought-${avatarId}-${Date.now()}`;
    
    // Default offsets based on position
    const defaultOffsets: Record<string, [number, number, number]> = {
      above: [0, 2, 0],
      left: [-1.5, 0, 0],
      right: [1.5, 0, 0],
      behind: [0, 0, -1]
    };
    
    const card: ThoughtCard = {
      id: cardId,
      avatarId,
      content,
      position: options.position || 'above',
      offset: options.offset || defaultOffsets[options.position || 'above'],
      size: options.size || [2, 1],
      opacity: options.opacity ?? 0.9,
      metadata: {
        timestamp: Date.now(),
        ...options.metadata
      }
    };
    
    this.cards.set(cardId, card);
    this.createCardTexture(card);
    
    return card;
  }

  /**
   * Create thought card from agent node.
   * 
   * Automatically extracts thought process information from a provenance node's
   * metadata and creates a thought card. The thought process includes agent ID,
   * dimension, pattern, and Church encoding information formatted as readable text.
   * 
   * Returns null if the node doesn't have an avatar configuration.
   * 
   * @param {ProvenanceNode} node - Provenance node with avatar configuration
   * @returns {ThoughtCard | null} Created thought card or null if node has no avatar
   * 
   * @example
   * ```typescript
   * const card = thoughtCardService.createThoughtCardFromNode(agentNode);
   * if (card) {
   *   // Card created successfully
   * }
   * ```
   */
  createThoughtCardFromNode(node: ProvenanceNode): ThoughtCard | null {
    if (!node.avatar) return null;
    
    // Extract thought process from node metadata
    const thoughtProcess = [
      node.metadata.agentId,
      node.metadata.dimension ? `Dimension: ${node.metadata.dimension}` : '',
      node.metadata.pattern ? `Pattern: ${node.metadata.pattern}` : '',
      node.metadata.churchEncoding ? `Church: ${node.metadata.churchEncoding}` : ''
    ].filter(Boolean).join('\n');
    
    return this.createThoughtCard(
      node.id,
      thoughtProcess,
      {
        position: 'above',
        metadata: {
          agentId: node.metadata.agentId,
          dimension: node.metadata.dimension,
          pattern: node.metadata.pattern,
          thoughtProcess: thoughtProcess
        }
      }
    );
  }

  /**
   * Update thought card content.
   * 
   * Updates the text content of an existing thought card and regenerates its
   * texture. The card's timestamp is updated to reflect the modification.
   * 
   * @param {string} cardId - ID of the card to update
   * @param {string} content - New text content for the card
   * 
   * @example
   * ```typescript
   * thoughtCardService.updateThoughtCard('thought-123', 'Updated content');
   * // Card texture is regenerated with new content
   * ```
   */
  updateThoughtCard(cardId: string, content: string): void {
    const card = this.cards.get(cardId);
    if (!card) return;
    
    card.content = content;
    card.metadata = {
      ...card.metadata,
      timestamp: Date.now()
    };
    
    this.createCardTexture(card);
  }

  /**
   * Remove thought card.
   * 
   * Removes a thought card from the service and cleans up its associated texture.
   * This should be called when a card is no longer needed to free up resources.
   * 
   * @param {string} cardId - ID of the card to remove
   * 
   * @example
   * ```typescript
   * thoughtCardService.removeThoughtCard('thought-123');
   * // Card and texture are removed
   * ```
   */
  removeThoughtCard(cardId: string): void {
    this.cards.delete(cardId);
    this.cardTextures.delete(cardId);
  }

  /**
   * Get thought cards for an avatar.
   * 
   * Returns all thought cards associated with a specific avatar. This is useful
   * for managing multiple cards per avatar or cleaning up cards when an avatar
   * is removed.
   * 
   * @param {string} avatarId - ID of the avatar
   * @returns {ThoughtCard[]} Array of thought cards for the avatar
   * 
   * @example
   * ```typescript
   * const cards = thoughtCardService.getThoughtCardsForAvatar('avatar-123');
   * cards.forEach(card => card.removeThoughtCard(card.id));
   * ```
   */
  getThoughtCardsForAvatar(avatarId: string): ThoughtCard[] {
    return Array.from(this.cards.values()).filter(card => card.avatarId === avatarId);
  }

  /**
   * Get all thought cards.
   * 
   * Returns all thought cards currently managed by the service. Useful for
   * debugging, cleanup, or bulk operations.
   * 
   * @returns {ThoughtCard[]} Array of all thought cards
   * 
   * @example
   * ```typescript
   * const allCards = thoughtCardService.getAllThoughtCards();
   * console.log(`Total cards: ${allCards.length}`);
   * ```
   */
  getAllThoughtCards(): ThoughtCard[] {
    return Array.from(this.cards.values());
  }

  /**
   * Create texture for thought card (public for component access).
   * 
   * Generates a canvas texture from the thought card's content. The texture is
   * rendered with a dark background, blue border, and white text. Text is
   * automatically word-wrapped to fit within the card's dimensions.
   * 
   * The generated texture is cached for reuse and can be retrieved using
   * `getCardTexture()`. Textures are scaled for better text quality (100x scale).
   * 
   * @param {ThoughtCard} card - Thought card to create texture for
   * @returns {HTMLCanvasElement} Canvas element containing the rendered texture
   * @throws {Error} If canvas context cannot be obtained
   * 
   * @example
   * ```typescript
   * const texture = thoughtCardService.createCardTexture(card);
   * // Texture can be used with Three.js CanvasTexture
   * ```
   */
  createCardTexture(card: ThoughtCard): HTMLCanvasElement {
    const canvas = document.createElement('canvas');
    const ctx = canvas.getContext('2d');
    if (!ctx) throw new Error('Failed to get canvas context');
    
    const [width, height] = card.size;
    canvas.width = width * 100; // Scale for better text quality
    canvas.height = height * 100;
    
    // Background
    ctx.fillStyle = 'rgba(30, 30, 30, 0.95)';
    ctx.fillRect(0, 0, canvas.width, canvas.height);
    
    // Border
    ctx.strokeStyle = '#3b82f6';
    ctx.lineWidth = 4;
    ctx.strokeRect(2, 2, canvas.width - 4, canvas.height - 4);
    
    // Text
    ctx.fillStyle = '#ffffff';
    ctx.font = '16px Arial';
    ctx.textAlign = 'left';
    ctx.textBaseline = 'top';
    
    // Word wrap
    const words = card.content.split(' ');
    let line = '';
    let y = 20;
    const maxWidth = canvas.width - 40;
    const lineHeight = 24;
    
    for (const word of words) {
      const testLine = line + word + ' ';
      const metrics = ctx.measureText(testLine);
      
      if (metrics.width > maxWidth && line !== '') {
        ctx.fillText(line, 20, y);
        line = word + ' ';
        y += lineHeight;
      } else {
        line = testLine;
      }
    }
    ctx.fillText(line, 20, y);
    
    this.cardTextures.set(card.id, canvas);
    return canvas;
  }

  /**
   * Get texture for thought card.
   * 
   * Retrieves the cached canvas texture for a thought card. Returns null if
   * the card doesn't exist or its texture hasn't been created yet.
   * 
   * @param {string} cardId - ID of the card
   * @returns {HTMLCanvasElement | null} Canvas texture or null if not found
   * 
   * @example
   * ```typescript
   * const texture = thoughtCardService.getCardTexture('thought-123');
   * if (texture) {
   *   // Use texture for rendering
   * }
   * ```
   */
  getCardTexture(cardId: string): HTMLCanvasElement | null {
    return this.cardTextures.get(cardId) || null;
  }

  /**
   * Clear all thought cards.
   * 
   * Removes all thought cards and their textures from the service. This should
   * be called when the service is disposed or when a complete reset is needed.
   * 
   * @example
   * ```typescript
   * thoughtCardService.clearAll();
   * // All cards and textures are removed
   * ```
   */
  clearAll(): void {
    this.cards.clear();
    this.cardTextures.clear();
  }
}

export const thoughtCardService = new ThoughtCardService();

