/**
 * Example 7: Creating Custom Thought Card Renderer
 * 
 * This example demonstrates how to create a custom thought card
 * renderer with custom styling and behavior.
 */

import { ThoughtCardService, ThoughtCard } from '@/services/thought-card-service';
import { ProvenanceNode } from '@/services/provenance-slide-service';

class CustomThoughtCardService extends ThoughtCardService {
  /**
   * Create thought card with custom styling
   */
  createThoughtCard(
    avatarId: string,
    content: string,
    options: any = {}
  ): ThoughtCard {
    // Call parent method
    const card = super.createThoughtCard(avatarId, content, options);

    // Add custom metadata
    card.metadata = {
      ...card.metadata,
      customStyle: 'animated',
      animationSpeed: 1.0,
      theme: options.theme || 'default'
    };

    return card;
  }

  /**
   * Create card texture with custom rendering
   */
  createCardTexture(card: ThoughtCard): HTMLCanvasElement {
    const canvas = document.createElement('canvas');
    const ctx = canvas.getContext('2d');
    if (!ctx) throw new Error('Failed to get canvas context');

    const [width, height] = card.size;
    const scale = 100; // High resolution
    canvas.width = width * scale;
    canvas.height = height * scale;

    // Custom background based on theme
    const theme = card.metadata?.theme || 'default';
    const backgrounds = {
      default: '#1e293b',
      success: '#065f46',
      warning: '#92400e',
      error: '#991b1b'
    };

    ctx.fillStyle = backgrounds[theme as keyof typeof backgrounds] || backgrounds.default;
    ctx.fillRect(0, 0, canvas.width, canvas.height);

    // Custom border
    ctx.strokeStyle = '#3b82f6';
    ctx.lineWidth = 4 * scale;
    ctx.strokeRect(2 * scale, 2 * scale, canvas.width - 4 * scale, canvas.height - 4 * scale);

    // Custom text rendering
    ctx.scale(scale, scale);
    ctx.font = `${card.fontSize}px Arial`;
    ctx.fillStyle = '#ffffff';
    ctx.textAlign = 'center';
    ctx.textBaseline = 'middle';

    // Word wrap with custom styling
    const words = card.content.split(' ');
    let line = '';
    const lineHeight = card.fontSize * 1.2;
    let y = lineHeight;
    const maxWidth = width - 20;

    for (const word of words) {
      const testLine = line + word + ' ';
      const metrics = ctx.measureText(testLine);
      if (metrics.width > maxWidth && line !== '') {
        ctx.fillText(line, width / 2, y);
        line = word + ' ';
        y += lineHeight;
      } else {
        line = testLine;
      }
    }
    ctx.fillText(line, width / 2, y);

    // Add custom effects (e.g., glow)
    if (card.metadata?.customStyle === 'animated') {
      // Add glow effect
      ctx.shadowColor = '#3b82f6';
      ctx.shadowBlur = 10;
      ctx.fillText(line, width / 2, y);
    }

    this.cardTextures.set(card.id, canvas);
    return canvas;
  }
}

// Usage example
async function useCustomThoughtCardService() {
  const customService = new CustomThoughtCardService();

  // Create custom thought card
  const card = customService.createThoughtCard('avatar-123', 'Processing...', {
    position: 'above',
    theme: 'success',
    size: [2, 1],
    opacity: 0.9
  });

  console.log('Custom thought card created:', card.id);
  console.log('Theme:', card.metadata?.theme);

  // Generate texture
  const texture = customService.createCardTexture(card);
  console.log('Custom texture generated:', texture.width, 'x', texture.height);

  return { card, texture };
}

// Run the example
useCustomThoughtCardService()
  .then(({ card, texture }) => {
    console.log('Custom thought card example completed');
  })
  .catch(error => {
    console.error('Custom thought card example failed:', error);
  });

