/**
 * Provenance Canvas Worker Service
 * 
 * Wrapper service for provenance-canvas-worker.ts to manage offscreen canvas workers
 * and handle communication with the worker.
 */

import { ProvenanceChain, ProvenanceNode } from './provenance-slide-service';

export interface WorkerMessage {
  type: 'init' | 'load' | 'query' | 'render' | 'interact' | 'updateCamera' | 'resize' | 'dispose';
  payload: any;
}

export interface CanvasOptions {
  width: number;
  height: number;
  antialias?: boolean;
}

export class ProvenanceCanvasWorkerService {
  private worker: Worker | null;
  private canvas: OffscreenCanvas | null;
  private messageHandlers: Map<string, (data: any) => void>;
  private initialized: boolean;

  constructor() {
    this.worker = null;
    this.canvas = null;
    this.messageHandlers = new Map();
    this.initialized = false;
  }

  /**
   * Initialize worker with offscreen canvas
   */
  async init(canvas: OffscreenCanvas, options: CanvasOptions): Promise<void> {
    if (this.initialized) {
      throw new Error('Worker already initialized');
    }

    // Check OffscreenCanvas support
    if (!canvas || !canvas.transferControlToOffscreen) {
      throw new Error('OffscreenCanvas not supported in this browser');
    }

    this.canvas = canvas;
    
    try {
      // Create worker
      this.worker = new Worker(
        new URL('../workers/provenance-canvas-worker.ts', import.meta.url),
        { type: 'module' }
      );

      // Set up error handler
      this.worker.onerror = (error) => {
        console.error('Worker error:', error);
        this.handleWorkerError(error);
      };

      // Set up message handler
      this.worker.onmessage = (event: MessageEvent) => {
        const { type, payload } = event.data;
        
        // Handle error messages from worker
        if (type === 'error') {
          console.error('Worker reported error:', payload);
          this.handleWorkerError(new Error(payload.message || 'Unknown worker error'));
          return;
        }
        
        const handler = this.messageHandlers.get(type);
        if (handler) {
          handler(payload);
        }
      };

      // Send init message
      this.sendMessage({
        type: 'init',
        payload: { canvas, options }
      });

      // Wait for initialization with timeout
      await this.waitForMessage('initialized', 10000);
      
      this.initialized = true;
    } catch (error) {
      // Clean up on failure
      if (this.worker) {
        this.worker.terminate();
        this.worker = null;
      }
      this.canvas = null;
      this.initialized = false;
      
      throw new Error(`Failed to initialize worker: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }
  }

  /**
   * Load provenance chain into worker
   */
  loadProvenanceChain(chain: ProvenanceChain): void {
    if (!this.initialized || !this.worker) {
      throw new Error('Worker not initialized');
    }

    this.sendMessage({
      type: 'load',
      payload: { chain }
    });
  }

  /**
   * Handle user interaction (click/hover)
   */
  handleInteraction(
    x: number,
    y: number,
    width: number,
    height: number,
    interactionType: 'click' | 'hover'
  ): Promise<ProvenanceNode | null> {
    if (!this.initialized || !this.worker) {
      throw new Error('Worker not initialized');
    }

    return new Promise((resolve) => {
      const handler = (payload: any) => {
        if (payload.node) {
          resolve(payload.node);
        } else {
          resolve(null);
        }
      };

      this.messageHandlers.set('nodeSelected', handler);

      this.sendMessage({
        type: 'interact',
        payload: { x, y, width, height, interactionType }
      });

      // Clean up handler after timeout
      setTimeout(() => {
        this.messageHandlers.delete('nodeSelected');
        resolve(null);
      }, 1000);
    });
  }

  /**
   * Update camera position
   */
  updateCamera(position: [number, number, number], target: [number, number, number]): void {
    if (!this.initialized || !this.worker) {
      throw new Error('Worker not initialized');
    }

    this.sendMessage({
      type: 'updateCamera',
      payload: { position, target }
    });
  }

  /**
   * Resize canvas
   */
  resize(width: number, height: number): void {
    if (!this.initialized || !this.worker) {
      throw new Error('Worker not initialized');
    }

    this.sendMessage({
      type: 'resize',
      payload: { width, height }
    });
  }

  /**
   * Dispose worker
   */
  dispose(): void {
    if (this.worker) {
      this.sendMessage({
        type: 'dispose',
        payload: {}
      });
      this.worker.terminate();
      this.worker = null;
    }
    this.canvas = null;
    this.initialized = false;
    this.messageHandlers.clear();
  }

  /**
   * Send message to worker
   */
  private sendMessage(message: WorkerMessage): void {
    if (!this.worker) {
      throw new Error('Worker not initialized');
    }
    this.worker.postMessage(message);
  }

  /**
   * Wait for specific message type
   */
  private waitForMessage(type: string, timeout: number = 5000): Promise<any> {
    return new Promise((resolve, reject) => {
      const handler = (payload: any) => {
        this.messageHandlers.delete(type);
        resolve(payload);
      };

      this.messageHandlers.set(type, handler);

      setTimeout(() => {
        this.messageHandlers.delete(type);
        reject(new Error(`Timeout waiting for message: ${type}`));
      }, timeout);
    });
  }

  /**
   * Register message handler
   */
  onMessage(type: string, handler: (data: any) => void): void {
    this.messageHandlers.set(type, handler);
  }

  /**
   * Remove message handler
   */
  offMessage(type: string): void {
    this.messageHandlers.delete(type);
  }

  /**
   * Handle worker errors
   */
  private handleWorkerError(error: Error | ErrorEvent): void {
    console.error('Worker error occurred:', error);
    
    // Notify any error handlers
    const errorHandler = this.messageHandlers.get('error');
    if (errorHandler) {
      errorHandler({
        message: error instanceof Error ? error.message : error.message || 'Unknown worker error',
        type: 'worker-error'
      });
    }
    
    // Optionally dispose worker on critical errors
    // Uncomment if you want automatic cleanup on errors
    // this.dispose();
  }

  /**
   * Check if worker is available and supported
   */
  static isSupported(): boolean {
    return typeof Worker !== 'undefined' && typeof OffscreenCanvas !== 'undefined';
  }

  /**
   * Get worker initialization status
   */
  isInitialized(): boolean {
    return this.initialized;
  }
}

