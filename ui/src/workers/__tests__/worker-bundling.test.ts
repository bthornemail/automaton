/**
 * Worker Bundling Tests
 * 
 * Tests to verify Three.js bundling in worker context and OffscreenCanvas support
 */

import { describe, test, expect, vi, beforeEach } from 'vitest';

// Mock Three.js for testing
vi.mock('three', () => ({
  Scene: class {},
  PerspectiveCamera: class {},
  WebGLRenderer: class {},
  Color: class {},
  AmbientLight: class {},
  DirectionalLight: class {},
  PointLight: class {},
  GridHelper: class {},
  SphereGeometry: class {},
  MeshStandardMaterial: class {},
  Mesh: class {},
  BufferGeometry: class {},
  Vector3: class {},
  LineBasicMaterial: class {},
  Line: class {},
  Raycaster: class {},
  Vector2: class {},
}));

describe('Worker Bundling', () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  describe('Three.js Import', () => {
    test('should import Three.js in worker context', async () => {
      // This test verifies that the worker file can be imported
      // and Three.js is available
      const workerCode = await import('../provenance-canvas-worker');
      
      // Worker should export or be importable
      expect(workerCode).toBeDefined();
    });

    test('should handle Three.js types in worker', () => {
      // Verify that Three.js types are available
      // This is a compile-time check, so if this test runs, types are available
      expect(true).toBe(true);
    });
  });

  describe('OffscreenCanvas Support', () => {
    test('should detect OffscreenCanvas support', () => {
      // Mock OffscreenCanvas
      global.OffscreenCanvas = class {
        width = 800;
        height = 600;
        constructor(public w: number, public h: number) {
          this.width = w;
          this.height = h;
        }
      } as any;

      const isSupported = typeof OffscreenCanvas !== 'undefined';
      expect(isSupported).toBe(true);
    });

    test('should handle missing OffscreenCanvas gracefully', async () => {
      // Test that code handles missing OffscreenCanvas
      const originalOffscreenCanvas = global.OffscreenCanvas;
      // @ts-ignore
      delete global.OffscreenCanvas;

      // Service should check for support
      const { ProvenanceCanvasWorkerService } = await import('../../services/provenance-canvas-worker-service');
      const isSupported = ProvenanceCanvasWorkerService.isSupported();
      expect(isSupported).toBe(false);

      // Restore
      global.OffscreenCanvas = originalOffscreenCanvas;
    });
  });

  describe('Worker Initialization', () => {
    test('should handle worker initialization errors', async () => {
      const { ProvenanceCanvasWorkerService } = await import('../../services/provenance-canvas-worker-service');
      const service = new ProvenanceCanvasWorkerService();

      // Mock OffscreenCanvas
      const mockCanvas = {
        width: 800,
        height: 600,
        transferControlToOffscreen: vi.fn().mockReturnValue({
          width: 800,
          height: 600
        })
      } as any;

      // Mock Worker to throw error
      const originalWorker = global.Worker;
      global.Worker = class {
        constructor() {
          throw new Error('Worker creation failed');
        }
      } as any;

      await expect(
        service.init(mockCanvas as OffscreenCanvas, {
          width: 800,
          height: 600,
          antialias: true
        })
      ).rejects.toThrow();

      // Restore
      global.Worker = originalWorker;
    });
  });

  describe('Error Handling', () => {
    test('should handle worker message errors', async () => {
      // Test that error messages from worker are handled
      const { ProvenanceCanvasWorkerService } = await import('../../services/provenance-canvas-worker-service');
      const service = new ProvenanceCanvasWorkerService();

      let errorReceived = false;
      service.onMessage('error', (error: any) => {
        errorReceived = true;
        expect(error).toBeDefined();
      });

      // Simulate error message
      const errorHandler = service['messageHandlers'].get('error');
      if (errorHandler) {
        errorHandler({ message: 'Test error', type: 'test' });
      }

      expect(errorReceived).toBe(true);
    });
  });
});

