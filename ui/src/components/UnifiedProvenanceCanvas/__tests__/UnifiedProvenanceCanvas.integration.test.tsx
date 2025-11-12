/**
 * Integration Tests for UnifiedProvenanceCanvas Component
 * 
 * Tests component initialization, slide navigation, card display, and worker communication
 */

import React from 'react';
import { describe, test, expect, beforeEach, afterEach, vi } from 'vitest';
import { render, screen, waitFor, fireEvent, within } from '@testing-library/react';
import { UnifiedProvenanceCanvas } from '../UnifiedProvenanceCanvas';
import { Slide, ProvenanceChain, ProvenanceNode } from '../../../services/provenance-slide-service';
import { generateMockProvenanceChain } from '../../../services/__tests__/utils/mockProvenanceChain';
import { generateMockEvolutionEntries } from '../../../services/__tests__/utils/mockEvolutionData';

// Mock biwascheme and other dependencies first - must be before any imports
vi.mock('biwascheme', () => ({
  default: {
    evaluate: vi.fn(),
    load: vi.fn()
  }
}));

vi.mock('meta-log-db/browser', () => ({
  CanvasLMetaverseBrowser: class {
    init = vi.fn().mockResolvedValue(undefined);
    query = vi.fn().mockResolvedValue([]);
  }
}));

// Mock projector services to avoid MetaLogBridge import issues
vi.mock('../../../services/projector/Projector', () => ({
  Projector: class {
    onInit = vi.fn().mockResolvedValue(undefined);
  }
}));

vi.mock('../../../services/agent-coordinator/AgentCoordinator', () => ({
  AgentCoordinator: class {
    init = vi.fn().mockResolvedValue(undefined);
  }
}));

vi.mock('../../../services/projector/TopicSlideGenerator', () => ({
  TopicSlideGenerator: class {
    constructor() {}
  }
}));

// Mock dependencies
vi.mock('../../../services/provenance-slide-service', async () => {
  const actual = await vi.importActual('../../../services/provenance-slide-service');
  
  // Create a mock class that can be instantiated
  class MockProvenanceSlideService {
    init = vi.fn().mockResolvedValue(undefined);
    generateSlidesFromEvolution = vi.fn().mockResolvedValue([]);
    buildProvenanceChain = vi.fn().mockResolvedValue({ nodes: [], edges: [] });
  }
  
  return {
    ...actual,
    ProvenanceSlideService: MockProvenanceSlideService
  };
});

vi.mock('../../../services/provenance-canvas-worker-service', () => ({
  ProvenanceCanvasWorkerService: class {
    init = vi.fn().mockResolvedValue(undefined);
    loadProvenanceChain = vi.fn().mockResolvedValue(undefined);
    handleInteraction = vi.fn().mockResolvedValue(null);
    resize = vi.fn();
    dispose = vi.fn();
  }
}));

vi.mock('@react-three/fiber', () => ({
  Canvas: ({ children }: any) => <div data-testid="three-canvas">{children}</div>,
  useFrame: () => {},
  useThree: () => ({
    camera: { position: [0, 0, 5] },
    gl: { domElement: document.createElement('canvas') }
  })
}));

vi.mock('@react-three/drei', () => ({
  OrbitControls: () => null,
  Text: () => null,
  Html: () => null
}));

vi.mock('framer-motion', () => ({
  motion: {
    div: ({ children, ...props }: any) => <div {...props}>{children}</div>
  },
  AnimatePresence: ({ children }: any) => <>{children}</>
}));

vi.mock('lucide-react', () => ({
  ChevronLeft: () => <span data-testid="chevron-left">←</span>,
  ChevronRight: () => <span data-testid="chevron-right">→</span>,
  ZoomIn: () => <span data-testid="zoom-in">+</span>,
  ZoomOut: () => <span data-testid="zoom-out">-</span>,
  RotateCcw: () => <span data-testid="rotate">↻</span>,
  Info: () => <span data-testid="info">ℹ</span>
}));

// Mock d3
vi.mock('d3', () => ({
  select: vi.fn(() => ({
    selectAll: vi.fn(() => ({
      remove: vi.fn(),
      enter: vi.fn(() => ({
        append: vi.fn(() => ({
          attr: vi.fn().mockReturnThis(),
          text: vi.fn().mockReturnThis()
        }))
      })),
      data: vi.fn().mockReturnThis()
    })),
    attr: vi.fn().mockReturnThis(),
    append: vi.fn(() => ({
      selectAll: vi.fn(() => ({
        data: vi.fn().mockReturnThis(),
        enter: vi.fn(() => ({
          append: vi.fn(() => ({
            attr: vi.fn().mockReturnThis(),
            text: vi.fn().mockReturnThis()
          }))
        }))
      }))
    }))
  }))
}));

// Helper function to create mock slides
function createMockSlides(count: number = 3): Slide[] {
  const slides: Slide[] = [];
  const dimensions = ['0D', '1D', '2D', '3D', '4D', '5D', '6D', '7D'];
  
  for (let i = 0; i < count; i++) {
    const dimension = dimensions[i % dimensions.length];
    const chain = generateMockProvenanceChain(5, { dimension });
    
    slides.push({
      id: `slide-${i}`,
      type: 'provenance',
      title: `Slide ${i + 1}`,
      dimension,
      description: `Provenance visualization for ${dimension}`,
      provenanceChain: chain,
      cards: [
        {
          id: `card-${i}-0`,
          pattern: `pattern-${i}`,
          jsonlLines: [{ id: `line-${i}-0`, type: 'test' }],
          metadata: {
            churchEncoding: 'λf.λx.x',
            bqfCoefficients: [0, 0, 0]
          }
        }
      ]
    });
  }
  
  return slides;
}

describe('UnifiedProvenanceCanvas - Integration Tests', () => {
  let mockOnSlideChange: ReturnType<typeof vi.fn>;

  beforeEach(() => {
    vi.clearAllMocks();
    mockOnSlideChange = vi.fn();
    
    // Mock OffscreenCanvas
    global.OffscreenCanvas = class {
      width = 800;
      height = 600;
      constructor(public w: number, public h: number) {
        this.width = w;
        this.height = h;
      }
      transferControlToOffscreen = vi.fn().mockReturnValue(this);
    } as any;
    
    // Mock HTMLCanvasElement.transferControlToOffscreen
    HTMLCanvasElement.prototype.transferControlToOffscreen = vi.fn().mockReturnValue(
      new OffscreenCanvas(800, 600)
    );
  });

  afterEach(() => {
    vi.restoreAllMocks();
  });

  describe('Component Initialization', () => {
    test('should mount component with provenance chain', async () => {
      const slides = createMockSlides(3);
      
      render(
        <UnifiedProvenanceCanvas
          slides={slides}
          onSlideChange={mockOnSlideChange}
        />
      );

      await waitFor(() => {
        expect(screen.queryByText('Loading provenance visualization...')).not.toBeInTheDocument();
      });

      // Should display first slide
      expect(screen.getByText('Slide 1')).toBeInTheDocument();
      expect(screen.getByText('0D')).toBeInTheDocument();
    });

    test('should mount component without provenance chain', async () => {
      render(
        <UnifiedProvenanceCanvas
          onSlideChange={mockOnSlideChange}
        />
      );

      await waitFor(() => {
        expect(screen.getByText('No slides available')).toBeInTheDocument();
      });
    });

    test('should handle error during initialization', async () => {
      const { ProvenanceSlideService } = await import('../../../services/provenance-slide-service');
      const MockedService = ProvenanceSlideService as any;
      
      // Mock init to throw error
      MockedService.prototype.init = vi.fn().mockRejectedValue(new Error('Init failed'));

      const slides = createMockSlides(1);
      
      render(
        <UnifiedProvenanceCanvas
          slides={slides}
          onSlideChange={mockOnSlideChange}
        />
      );

      // Should still render, but may show error state
      await waitFor(() => {
        expect(screen.queryByText('Loading provenance visualization...')).not.toBeInTheDocument();
      });
    });

    test('should load slides from evolution path when provided', async () => {
      const { ProvenanceSlideService } = await import('../../../services/provenance-slide-service');
      const mockSlides = createMockSlides(2);
      
      // Mock the method on the prototype so all instances use it
      const originalMethod = ProvenanceSlideService.prototype.generateSlidesFromEvolution;
      ProvenanceSlideService.prototype.generateSlidesFromEvolution = vi.fn().mockResolvedValue(mockSlides);

      render(
        <UnifiedProvenanceCanvas
          evolutionPath="evolution/test"
          onSlideChange={mockOnSlideChange}
        />
      );

      await waitFor(() => {
        expect(ProvenanceSlideService.prototype.generateSlidesFromEvolution).toHaveBeenCalledWith('evolution/test');
      }, { timeout: 3000 });
      
      // Restore original method
      ProvenanceSlideService.prototype.generateSlidesFromEvolution = originalMethod;
    });
  });

  describe('Slide Navigation', () => {
    test('should generate slides from evolution directory', async () => {
      const { ProvenanceSlideService } = await import('../../../services/provenance-slide-service');
      const mockSlides = createMockSlides(3);
      
      // Mock the method on the prototype
      const originalMethod = ProvenanceSlideService.prototype.generateSlidesFromEvolution;
      const mockMethod = vi.fn().mockResolvedValue(mockSlides);
      ProvenanceSlideService.prototype.generateSlidesFromEvolution = mockMethod;

      render(
        <UnifiedProvenanceCanvas
          evolutionPath="evolution/test"
          onSlideChange={mockOnSlideChange}
        />
      );

      await waitFor(() => {
        expect(mockMethod).toHaveBeenCalled();
      }, { timeout: 5000 });
      
      // Restore original method
      ProvenanceSlideService.prototype.generateSlidesFromEvolution = originalMethod;
    });

    test('should navigate to next slide', async () => {
      const slides = createMockSlides(3);
      
      render(
        <UnifiedProvenanceCanvas
          slides={slides}
          onSlideChange={mockOnSlideChange}
        />
      );

      await waitFor(() => {
        expect(screen.getByText('Slide 1')).toBeInTheDocument();
      });

      // Click next button
      const nextButton = screen.getByTestId('chevron-right').closest('button');
      expect(nextButton).toBeInTheDocument();
      
      if (nextButton) {
        fireEvent.click(nextButton);
        
        await waitFor(() => {
          expect(screen.getByText('Slide 2')).toBeInTheDocument();
          expect(mockOnSlideChange).toHaveBeenCalledWith(slides[1]);
        });
      }
    });

    test('should navigate to previous slide', async () => {
      const slides = createMockSlides(3);
      
      render(
        <UnifiedProvenanceCanvas
          slides={slides}
          onSlideChange={mockOnSlideChange}
        />
      );

      await waitFor(() => {
        expect(screen.getByText('Slide 1')).toBeInTheDocument();
      });

      // Navigate to second slide first
      const nextButton = screen.getByTestId('chevron-right').closest('button');
      if (nextButton) {
        fireEvent.click(nextButton);
      }

      await waitFor(() => {
        expect(screen.getByText('Slide 2')).toBeInTheDocument();
      });

      // Click previous button
      const prevButton = screen.getByTestId('chevron-left').closest('button');
      expect(prevButton).toBeInTheDocument();
      
      if (prevButton) {
        fireEvent.click(prevButton);
        
        await waitFor(() => {
          expect(screen.getByText('Slide 1')).toBeInTheDocument();
          expect(mockOnSlideChange).toHaveBeenCalledWith(slides[0]);
        });
      }
    });

    test('should disable navigation buttons at boundaries', async () => {
      const slides = createMockSlides(2);
      
      render(
        <UnifiedProvenanceCanvas
          slides={slides}
          onSlideChange={mockOnSlideChange}
        />
      );

      await waitFor(() => {
        expect(screen.getByText('Slide 1')).toBeInTheDocument();
      });

      // Previous button should be disabled on first slide
      const prevButton = screen.getByTestId('chevron-left').closest('button');
      expect(prevButton).toHaveAttribute('disabled');

      // Navigate to last slide
      const nextButton = screen.getByTestId('chevron-right').closest('button');
      if (nextButton) {
        fireEvent.click(nextButton);
      }

      await waitFor(() => {
        expect(screen.getByText('Slide 2')).toBeInTheDocument();
      });

      // Next button should be disabled on last slide
      const nextButton2 = screen.getByTestId('chevron-right').closest('button');
      expect(nextButton2).toHaveAttribute('disabled');
    });

    test('should filter by dimension', async () => {
      const slides = createMockSlides(5);
      
      render(
        <UnifiedProvenanceCanvas
          slides={slides}
          onSlideChange={mockOnSlideChange}
        />
      );

      await waitFor(() => {
        expect(screen.getByText('0D')).toBeInTheDocument();
      });

      // Navigate through slides and verify dimensions
      const dimensions = ['0D', '1D', '2D', '3D', '4D'];
      for (let i = 0; i < dimensions.length; i++) {
        if (i > 0) {
          const nextButton = screen.getByTestId('chevron-right').closest('button');
          if (nextButton && !nextButton.hasAttribute('disabled')) {
            fireEvent.click(nextButton);
          }
        }
        
        await waitFor(() => {
          expect(screen.getByText(dimensions[i])).toBeInTheDocument();
        });
      }
    });
  });

  describe('Card Display', () => {
    test('should render cards for each pattern', async () => {
      const slides = createMockSlides(1);
      slides[0].cards = [
        {
          id: 'card-1',
          pattern: 'identity',
          jsonlLines: [{ id: 'line-1', type: 'test' }],
          metadata: {}
        },
        {
          id: 'card-2',
          pattern: 'successor',
          jsonlLines: [{ id: 'line-2', type: 'test' }],
          metadata: {}
        }
      ];
      
      render(
        <UnifiedProvenanceCanvas
          slides={slides}
          onSlideChange={mockOnSlideChange}
        />
      );

      await waitFor(() => {
        expect(screen.getByText('identity')).toBeInTheDocument();
        expect(screen.getByText('successor')).toBeInTheDocument();
      });
    });

    test('should display card interaction on click', async () => {
      const slides = createMockSlides(1);
      slides[0].cards = [
        {
          id: 'card-1',
          pattern: 'identity',
          jsonlLines: [{ id: 'line-1', type: 'test' }],
          metadata: {}
        }
      ];
      
      render(
        <UnifiedProvenanceCanvas
          slides={slides}
          onSlideChange={mockOnSlideChange}
        />
      );

      await waitFor(() => {
        const card = screen.getByText('identity');
        expect(card).toBeInTheDocument();
        
        // Click on card
        fireEvent.click(card.closest('.bg-gray-700') || card);
      });
    });

    test('should display card detail views', async () => {
      const slides = createMockSlides(1);
      slides[0].cards = [
        {
          id: 'card-1',
          pattern: 'identity',
          jsonlLines: [
            { id: 'line-1', type: 'test', content: 'test content' },
            { id: 'line-2', type: 'test', content: 'test content 2' }
          ],
          metadata: {
            churchEncoding: 'λf.λx.x',
            bqfCoefficients: [0, 0, 0]
          }
        }
      ];
      
      render(
        <UnifiedProvenanceCanvas
          slides={slides}
          onSlideChange={mockOnSlideChange}
        />
      );

      await waitFor(() => {
        expect(screen.getByText('identity')).toBeInTheDocument();
        expect(screen.getByText('2 lines')).toBeInTheDocument();
      });
    });
  });

  describe('Worker Communication', () => {
    test('should initialize worker on mount', async () => {
      const { ProvenanceCanvasWorkerService } = await import('../../../services/provenance-canvas-worker-service');
      
      const slides = createMockSlides(1);
      
      render(
        <UnifiedProvenanceCanvas
          slides={slides}
          onSlideChange={mockOnSlideChange}
        />
      );

      await waitFor(() => {
        expect(ProvenanceCanvasWorkerService.prototype.init).toHaveBeenCalled();
      }, { timeout: 3000 });
    });

    test('should load provenance chain into worker', async () => {
      const { ProvenanceCanvasWorkerService } = await import('../../../services/provenance-canvas-worker-service');
      
      const slides = createMockSlides(1);
      const chain = generateMockProvenanceChain(5, { dimension: '0D' });
      slides[0].provenanceChain = chain;
      
      render(
        <UnifiedProvenanceCanvas
          slides={slides}
          onSlideChange={mockOnSlideChange}
        />
      );

      await waitFor(() => {
        expect(ProvenanceCanvasWorkerService.prototype.loadProvenanceChain).toHaveBeenCalledWith(chain);
      }, { timeout: 3000 });
    });

    test('should handle click interaction events', async () => {
      const { ProvenanceCanvasWorkerService } = await import('../../../services/provenance-canvas-worker-service');
      
      const mockNode: ProvenanceNode = {
        id: 'node-1',
        type: 'evolution',
        position: [0, 0, 0],
        metadata: {
          timestamp: Date.now(),
          file: 'test.jsonl',
          line: 1,
          agentId: '0D-Agent',
          dimension: '0D'
        },
        data: {}
      };
      
      // Mock handleInteraction on prototype
      const originalMethod = ProvenanceCanvasWorkerService.prototype.handleInteraction;
      ProvenanceCanvasWorkerService.prototype.handleInteraction = vi.fn().mockResolvedValue(mockNode);
      
      const slides = createMockSlides(1);
      
      render(
        <UnifiedProvenanceCanvas
          slides={slides}
          onSlideChange={mockOnSlideChange}
        />
      );

      await waitFor(() => {
        const canvas = document.querySelector('canvas');
        expect(canvas).toBeInTheDocument();
      });

      const canvas = document.querySelector('canvas');
      if (canvas) {
        // Simulate click on canvas
        fireEvent.click(canvas, {
          clientX: 100,
          clientY: 100
        });
      }

      await waitFor(() => {
        expect(ProvenanceCanvasWorkerService.prototype.handleInteraction).toHaveBeenCalled();
        expect(screen.getByText('node-1')).toBeInTheDocument();
      }, { timeout: 3000 });
      
      // Restore original method
      ProvenanceCanvasWorkerService.prototype.handleInteraction = originalMethod;
    });

    test('should handle hover interaction events', async () => {
      const { ProvenanceCanvasWorkerService } = await import('../../../services/provenance-canvas-worker-service');
      
      const mockNode: ProvenanceNode = {
        id: 'node-2',
        type: 'evolution',
        position: [0, 0, 0],
        metadata: {
          timestamp: Date.now(),
          file: 'test.jsonl',
          line: 2,
          agentId: '0D-Agent',
          dimension: '0D'
        },
        data: {}
      };
      
      // Mock handleInteraction on prototype
      const originalMethod = ProvenanceCanvasWorkerService.prototype.handleInteraction;
      ProvenanceCanvasWorkerService.prototype.handleInteraction = vi.fn().mockResolvedValue(mockNode);
      
      const slides = createMockSlides(1);
      
      render(
        <UnifiedProvenanceCanvas
          slides={slides}
          onSlideChange={mockOnSlideChange}
        />
      );

      await waitFor(() => {
        const canvas = document.querySelector('canvas');
        expect(canvas).toBeInTheDocument();
      });

      const canvas = document.querySelector('canvas');
      if (canvas) {
        // Simulate hover on canvas
        fireEvent.mouseMove(canvas, {
          clientX: 200,
          clientY: 200
        });
      }

      await waitFor(() => {
        expect(ProvenanceCanvasWorkerService.prototype.handleInteraction).toHaveBeenCalledWith(
          expect.any(Number),
          expect.any(Number),
          expect.any(Number),
          expect.any(Number),
          'hover'
        );
      }, { timeout: 3000 });
      
      // Restore original method
      ProvenanceCanvasWorkerService.prototype.handleInteraction = originalMethod;
    });

    test('should synchronize camera updates', async () => {
      const { ProvenanceCanvasWorkerService } = await import('../../../services/provenance-canvas-worker-service');
      
      const slides = createMockSlides(2);
      
      render(
        <UnifiedProvenanceCanvas
          slides={slides}
          onSlideChange={mockOnSlideChange}
        />
      );

      await waitFor(() => {
        expect(screen.getByText('Slide 1')).toBeInTheDocument();
      });

      // Navigate to next slide
      const nextButton = screen.getByTestId('chevron-right').closest('button');
      if (nextButton) {
        fireEvent.click(nextButton);
      }

      await waitFor(() => {
        expect(screen.getByText('Slide 2')).toBeInTheDocument();
        // Worker should load new chain when slide changes
        expect(ProvenanceCanvasWorkerService.prototype.loadProvenanceChain).toHaveBeenCalledTimes(2);
      }, { timeout: 3000 });
    });
  });
});

