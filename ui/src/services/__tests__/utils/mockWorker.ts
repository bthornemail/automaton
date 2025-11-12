/**
 * Mock Web Worker
 * Provides mock implementations for Web Worker API
 */

import { vi } from 'vitest';

export interface MockWorker extends Worker {
  postMessage: ReturnType<typeof vi.fn>;
  terminate: ReturnType<typeof vi.fn>;
  onmessage: ((event: MessageEvent) => void) | null;
  onerror: ((event: ErrorEvent) => void) | null;
  _triggerMessage: (data: any) => void;
  _triggerError: (error: Error) => void;
}

/**
 * Create mock Web Worker
 */
export function createMockWorker(): MockWorker {
  const mockWorker = {
    postMessage: vi.fn(),
    terminate: vi.fn(),
    onmessage: null as ((event: MessageEvent) => void) | null,
    onerror: null as ((event: ErrorEvent) => void) | null,
    _triggerMessage: (data: any) => {
      if (mockWorker.onmessage) {
        mockWorker.onmessage(new MessageEvent('message', { data }));
      }
    },
    _triggerError: (error: Error) => {
      if (mockWorker.onerror) {
        mockWorker.onerror(new ErrorEvent('error', { error }));
      }
    }
  } as MockWorker;

  return mockWorker;
}

/**
 * Create mock OffscreenCanvas
 */
export function createMockOffscreenCanvas(
  width: number = 800,
  height: number = 600
): OffscreenCanvas {
  // Create a mock OffscreenCanvas that will be returned by transferControlToOffscreen
  const mockOffscreenCanvas = {
    width,
    height,
    getContext: vi.fn(),
    convertToBlob: vi.fn(),
    transferToImageBitmap: vi.fn()
  } as unknown as OffscreenCanvas;

  // Create the main canvas with transferControlToOffscreen method
  const canvas = {
    width,
    height,
    getContext: vi.fn(),
    convertToBlob: vi.fn(),
    transferToImageBitmap: vi.fn(),
    // Add transferControlToOffscreen method that returns the mock OffscreenCanvas
    transferControlToOffscreen: vi.fn(() => mockOffscreenCanvas)
  } as unknown as OffscreenCanvas & { transferControlToOffscreen: () => OffscreenCanvas };

  return canvas as OffscreenCanvas;
}

/**
 * Mock Worker constructor
 */
export function setupWorkerMocks(): {
  Worker: typeof Worker;
  OffscreenCanvas: typeof OffscreenCanvas;
} {
  const mockWorkers = new Map<string, MockWorker>();

  // Mock Worker constructor
  const MockWorkerClass = vi.fn((url: URL | string, options?: WorkerOptions) => {
    const workerId = typeof url === 'string' ? url : url.toString();
    let worker = mockWorkers.get(workerId);

    if (!worker) {
      worker = createMockWorker();
      mockWorkers.set(workerId, worker);
    }

    return worker;
  }) as unknown as typeof Worker;

  // Mock OffscreenCanvas constructor
  const MockOffscreenCanvasClass = vi.fn(
    (width: number, height: number) => {
      return createMockOffscreenCanvas(width, height);
    }
  ) as unknown as typeof OffscreenCanvas;

  return {
    Worker: MockWorkerClass,
    OffscreenCanvas: MockOffscreenCanvasClass
  };
}

