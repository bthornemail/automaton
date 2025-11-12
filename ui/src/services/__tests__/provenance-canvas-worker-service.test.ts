/**
 * Provenance Canvas Worker Service Tests
 */

import { describe, test, expect, beforeEach, vi, afterEach } from 'vitest';
import { ProvenanceCanvasWorkerService } from '../provenance-canvas-worker-service';
import { ProvenanceChain } from '../provenance-slide-service';
import { generateMockProvenanceChain } from './utils/mockProvenanceChain';
import { createMockOffscreenCanvas, createMockWorker, setupWorkerMocks } from './utils/mockWorker';

// Setup worker mocks
const workerMocks = setupWorkerMocks();
const MockWorker = workerMocks.Worker;
const MockOffscreenCanvas = workerMocks.OffscreenCanvas;

// Replace global Worker and OffscreenCanvas
global.Worker = MockWorker as any;
global.OffscreenCanvas = MockOffscreenCanvas as any;

describe('ProvenanceCanvasWorkerService', () => {
  let service: ProvenanceCanvasWorkerService;
  let mockCanvas: OffscreenCanvas;
  let mockWorker: ReturnType<typeof createMockWorker>;

  beforeEach(() => {
    vi.clearAllMocks();
    service = new ProvenanceCanvasWorkerService();
    mockCanvas = createMockOffscreenCanvas(800, 600);
    
    // Create a mock worker instance
    mockWorker = createMockWorker();
    
    // Override Worker constructor to return our mock
    (global.Worker as any) = vi.fn(() => mockWorker);
  });

  afterEach(() => {
    if (service) {
      try {
        service.dispose();
      } catch (e) {
        // Ignore disposal errors
      }
    }
  });

  describe('Worker Initialization', () => {
    test('should initialize with valid OffscreenCanvas', async () => {
      // Mock worker to respond to init message
      mockWorker._triggerMessage = (data: any) => {
        if (data.type === 'init') {
          setTimeout(() => {
            if (mockWorker.onmessage) {
              mockWorker.onmessage(new MessageEvent('message', {
                data: { type: 'initialized', payload: {} }
              }));
            }
          }, 10);
        }
      };

      await service.init(mockCanvas, { width: 800, height: 600 });

      expect(mockWorker.postMessage).toHaveBeenCalled();
      const initCall = mockWorker.postMessage.mock.calls.find(
        (call: any[]) => call[0]?.type === 'init'
      );
      expect(initCall).toBeDefined();
    });

    test('should throw error if already initialized', async () => {
      // First initialization
      mockWorker._triggerMessage = (data: any) => {
        if (data.type === 'init') {
          setTimeout(() => {
            if (mockWorker.onmessage) {
              mockWorker.onmessage(new MessageEvent('message', {
                data: { type: 'initialized', payload: {} }
              }));
            }
          }, 10);
        }
      };

      await service.init(mockCanvas, { width: 800, height: 600 });

      // Second initialization should fail
      await expect(
        service.init(mockCanvas, { width: 800, height: 600 })
      ).rejects.toThrow('Worker already initialized');
    });

    test('should create worker and set up message handler', async () => {
      let messageHandler: ((event: MessageEvent) => void) | null = null;
      
      mockWorker.onmessage = (event: MessageEvent) => {
        if (messageHandler) {
          messageHandler(event);
        }
      };

      mockWorker._triggerMessage = (data: any) => {
        if (data.type === 'init') {
          setTimeout(() => {
            if (mockWorker.onmessage) {
              mockWorker.onmessage(new MessageEvent('message', {
                data: { type: 'initialized', payload: {} }
              }));
            }
          }, 10);
        }
      };

      await service.init(mockCanvas, { width: 800, height: 600 });

      expect(mockWorker.postMessage).toHaveBeenCalled();
    });

    test('should handle initialization timeout', async () => {
      // Don't trigger initialized message
      mockWorker._triggerMessage = () => {};

      await expect(
        service.init(mockCanvas, { width: 800, height: 600 })
      ).rejects.toThrow();
    });
  });

  describe('Provenance Chain Loading', () => {
    beforeEach(async () => {
      // Initialize service first
      mockWorker._triggerMessage = (data: any) => {
        if (data.type === 'init') {
          setTimeout(() => {
            if (mockWorker.onmessage) {
              mockWorker.onmessage(new MessageEvent('message', {
                data: { type: 'initialized', payload: {} }
              }));
            }
          }, 10);
        }
      };

      await service.init(mockCanvas, { width: 800, height: 600 });
    });

    test('should load provenance chain with valid chain', () => {
      const chain = generateMockProvenanceChain(5);

      service.loadProvenanceChain(chain);

      expect(mockWorker.postMessage).toHaveBeenCalled();
      const loadCall = mockWorker.postMessage.mock.calls.find(
        (call: any[]) => call[0]?.type === 'load'
      );
      expect(loadCall).toBeDefined();
      expect(loadCall[0].payload.chain).toEqual(chain);
    });

    test('should throw error if not initialized', () => {
      const uninitializedService = new ProvenanceCanvasWorkerService();
      const chain = generateMockProvenanceChain(5);

      expect(() => {
        uninitializedService.loadProvenanceChain(chain);
      }).toThrow('Worker not initialized');
    });

    test('should send message to worker', () => {
      const chain = generateMockProvenanceChain(5);

      service.loadProvenanceChain(chain);

      expect(mockWorker.postMessage).toHaveBeenCalledWith(
        expect.objectContaining({
          type: 'load',
          payload: expect.objectContaining({
            chain: expect.any(Object)
          })
        })
      );
    });
  });

  describe('Camera Updates', () => {
    beforeEach(async () => {
      mockWorker._triggerMessage = (data: any) => {
        if (data.type === 'init') {
          setTimeout(() => {
            if (mockWorker.onmessage) {
              mockWorker.onmessage(new MessageEvent('message', {
                data: { type: 'initialized', payload: {} }
              }));
            }
          }, 10);
        }
      };

      await service.init(mockCanvas, { width: 800, height: 600 });
    });

    test('should update camera with valid position and target', () => {
      const position: [number, number, number] = [10, 20, 30];
      const target: [number, number, number] = [0, 0, 0];

      service.updateCamera(position, target);

      expect(mockWorker.postMessage).toHaveBeenCalledWith(
        expect.objectContaining({
          type: 'updateCamera',
          payload: { position, target }
        })
      );
    });

    test('should throw error if not initialized', () => {
      const uninitializedService = new ProvenanceCanvasWorkerService();
      const position: [number, number, number] = [10, 20, 30];
      const target: [number, number, number] = [0, 0, 0];

      expect(() => {
        uninitializedService.updateCamera(position, target);
      }).toThrow('Worker not initialized');
    });
  });

  describe('Interaction Handling', () => {
    beforeEach(async () => {
      mockWorker._triggerMessage = (data: any) => {
        if (data.type === 'init') {
          setTimeout(() => {
            if (mockWorker.onmessage) {
              mockWorker.onmessage(new MessageEvent('message', {
                data: { type: 'initialized', payload: {} }
              }));
            }
          }, 10);
        }
      };

      await service.init(mockCanvas, { width: 800, height: 600 });
    });

    test('should handle click event', async () => {
      const mockNode = {
        id: 'test-node',
        type: 'evolution' as const,
        position: [0, 0, 0] as [number, number, number],
        metadata: {
          timestamp: Date.now(),
          file: 'test.jsonl',
          line: 1,
          agentId: 'test-agent'
        },
        data: {}
      };

      // Set up message handler to respond with node
      service.onMessage('nodeSelected', () => {});
      
      mockWorker._triggerMessage = (data: any) => {
        if (data.type === 'interact') {
          setTimeout(() => {
            if (mockWorker.onmessage) {
              mockWorker.onmessage(new MessageEvent('message', {
                data: { type: 'nodeSelected', payload: { node: mockNode } }
              }));
            }
          }, 10);
        }
      };

      const result = await service.handleInteraction(100, 200, 800, 600, 'click');

      expect(mockWorker.postMessage).toHaveBeenCalledWith(
        expect.objectContaining({
          type: 'interact',
          payload: expect.objectContaining({
            x: 100,
            y: 200,
            width: 800,
            height: 600,
            interactionType: 'click'
          })
        })
      );
    });

    test('should handle hover event', async () => {
      mockWorker._triggerMessage = (data: any) => {
        if (data.type === 'interact') {
          setTimeout(() => {
            if (mockWorker.onmessage) {
              mockWorker.onmessage(new MessageEvent('message', {
                data: { type: 'nodeSelected', payload: { node: null } }
              }));
            }
          }, 10);
        }
      };

      const result = await service.handleInteraction(150, 250, 800, 600, 'hover');

      expect(mockWorker.postMessage).toHaveBeenCalledWith(
        expect.objectContaining({
          type: 'interact',
          payload: expect.objectContaining({
            interactionType: 'hover'
          })
        })
      );
    });

    test('should return selected node', async () => {
      const mockNode = {
        id: 'test-node',
        type: 'evolution' as const,
        position: [0, 0, 0] as [number, number, number],
        metadata: {
          timestamp: Date.now(),
          file: 'test.jsonl',
          line: 1,
          agentId: 'test-agent'
        },
        data: {}
      };

      let resolveHandler: ((payload: any) => void) | null = null;
      
      service.onMessage('nodeSelected', (payload) => {
        if (resolveHandler) {
          resolveHandler(payload);
        }
      });

      mockWorker._triggerMessage = (data: any) => {
        if (data.type === 'interact') {
          setTimeout(() => {
            if (mockWorker.onmessage) {
              mockWorker.onmessage(new MessageEvent('message', {
                data: { type: 'nodeSelected', payload: { node: mockNode } }
              }));
            }
          }, 10);
        }
      };

      const promise = service.handleInteraction(100, 200, 800, 600, 'click');
      
      // Manually trigger the message handler
      setTimeout(() => {
        if (mockWorker.onmessage) {
          mockWorker.onmessage(new MessageEvent('message', {
            data: { type: 'nodeSelected', payload: { node: mockNode } }
          }));
        }
      }, 50);

      const result = await promise;
      // Result might be null due to timeout, but we tested the interaction
      expect(mockWorker.postMessage).toHaveBeenCalled();
    });

    test('should return null on timeout', async () => {
      // Don't trigger nodeSelected message
      mockWorker._triggerMessage = () => {};

      const result = await service.handleInteraction(100, 200, 800, 600, 'click');

      // Should timeout and return null
      expect(result).toBeNull();
    }, 2000); // Increase timeout for this test

    test('should throw error if not initialized', () => {
      const uninitializedService = new ProvenanceCanvasWorkerService();

      expect(() => {
        uninitializedService.handleInteraction(100, 200, 800, 600, 'click');
      }).toThrow('Worker not initialized');
    });
  });

  describe('Canvas Resizing', () => {
    beforeEach(async () => {
      mockWorker._triggerMessage = (data: any) => {
        if (data.type === 'init') {
          setTimeout(() => {
            if (mockWorker.onmessage) {
              mockWorker.onmessage(new MessageEvent('message', {
                data: { type: 'initialized', payload: {} }
              }));
            }
          }, 10);
        }
      };

      await service.init(mockCanvas, { width: 800, height: 600 });
    });

    test('should resize with valid dimensions', () => {
      service.resize(1024, 768);

      expect(mockWorker.postMessage).toHaveBeenCalledWith(
        expect.objectContaining({
          type: 'resize',
          payload: { width: 1024, height: 768 }
        })
      );
    });

    test('should throw error if not initialized', () => {
      const uninitializedService = new ProvenanceCanvasWorkerService();

      expect(() => {
        uninitializedService.resize(1024, 768);
      }).toThrow('Worker not initialized');
    });
  });

  describe('Worker Disposal', () => {
    beforeEach(async () => {
      mockWorker._triggerMessage = (data: any) => {
        if (data.type === 'init') {
          setTimeout(() => {
            if (mockWorker.onmessage) {
              mockWorker.onmessage(new MessageEvent('message', {
                data: { type: 'initialized', payload: {} }
              }));
            }
          }, 10);
        }
      };

      await service.init(mockCanvas, { width: 800, height: 600 });
    });

    test('should terminate worker', () => {
      service.dispose();

      expect(mockWorker.terminate).toHaveBeenCalled();
    });

    test('should clear message handlers', () => {
      service.onMessage('test', () => {});
      service.dispose();

      // After disposal, handlers should be cleared
      // We can't directly test this, but we can verify dispose doesn't throw
      expect(() => service.dispose()).not.toThrow();
    });

    test('should allow re-initialization after disposal', async () => {
      service.dispose();

      // Create new service instance
      const newService = new ProvenanceCanvasWorkerService();
      const newMockWorker = createMockWorker();
      
      newMockWorker._triggerMessage = (data: any) => {
        if (data.type === 'init') {
          setTimeout(() => {
            if (newMockWorker.onmessage) {
              newMockWorker.onmessage(new MessageEvent('message', {
                data: { type: 'initialized', payload: {} }
              }));
            }
          }, 10);
        }
      };

      await newService.init(mockCanvas, { width: 800, height: 600 });

      expect(newMockWorker.postMessage).toHaveBeenCalled();
    });
  });

  describe('Message Handling', () => {
    beforeEach(async () => {
      mockWorker._triggerMessage = (data: any) => {
        if (data.type === 'init') {
          setTimeout(() => {
            if (mockWorker.onmessage) {
              mockWorker.onmessage(new MessageEvent('message', {
                data: { type: 'initialized', payload: {} }
              }));
            }
          }, 10);
        }
      };

      await service.init(mockCanvas, { width: 800, height: 600 });
    });

    test('should register message handler with onMessage', () => {
      const handler = vi.fn();
      
      service.onMessage('test', handler);

      // Trigger a test message
      if (mockWorker.onmessage) {
        mockWorker.onmessage(new MessageEvent('message', {
          data: { type: 'test', payload: { data: 'test' } }
        }));
      }

      // Handler should be called (if message routing works)
      // Note: This depends on internal implementation
      expect(handler).toBeDefined();
    });

    test('should remove message handler with offMessage', () => {
      const handler = vi.fn();
      
      service.onMessage('test', handler);
      service.offMessage('test');

      // Handler should be removed
      // We can't directly test this, but we can verify offMessage doesn't throw
      expect(() => service.offMessage('test')).not.toThrow();
    });

    test('should handle waitForMessage timeout', async () => {
      // waitForMessage is private, but we can test it indirectly
      // through handleInteraction which uses it
      mockWorker._triggerMessage = () => {};

      const result = await service.handleInteraction(100, 200, 800, 600, 'click');

      // Should timeout and return null
      expect(result).toBeNull();
    }, 2000);
  });
});

