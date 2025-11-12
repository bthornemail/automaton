/**
 * Provenance Canvas Worker Service
 * 
 * Wrapper service for provenance-canvas-worker.ts to manage offscreen canvas workers
 * and handle communication with the worker.
 */

import { ProvenanceChain, ProvenanceNode } from './provenance-slide-service';
import { WorkerError } from '../utils/error-types';
import { errorLoggingService } from './error-logging-service';
import { performanceMonitoringService } from './performance-monitoring-service';
import { retryWithBackoff, RetryOptions } from '../utils/error-handling';

export interface WorkerMessage {
  type: 'init' | 'load' | 'query' | 'render' | 'interact' | 'updateCamera' | 'resize' | 'dispose';
  payload: any;
}

export interface CanvasOptions {
  width: number;
  height: number;
  antialias?: boolean;
}

export interface ProvenanceCanvasWorkerServiceConfig {
  retryOptions?: RetryOptions;
  maxRestartAttempts?: number;
}

export class ProvenanceCanvasWorkerService {
  private worker: Worker | null;
  private canvas: OffscreenCanvas | null;
  private messageHandlers: Map<string, (data: any) => void>;
  private initialized: boolean;
  private fallbackMode: 'normal' | '2d-only';
  private restartAttempts: number;
  private maxRestartAttempts: number;
  private retryOptions: RetryOptions;
  private currentChain: ProvenanceChain | null;

  constructor(config: ProvenanceCanvasWorkerServiceConfig = {}) {
    this.worker = null;
    this.canvas = null;
    this.messageHandlers = new Map();
    this.initialized = false;
    this.fallbackMode = 'normal';
    this.restartAttempts = 0;
    this.maxRestartAttempts = config.maxRestartAttempts || 3;
    this.currentChain = null;
    
    // Configure retry options (per service, no fixed defaults)
    this.retryOptions = config.retryOptions || {
      maxRetries: 2,
      initialDelay: 1000,
      maxDelay: 5000,
      backoffStrategy: 'exponential'
    };
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
      this.fallbackMode = '2d-only';
      throw new WorkerError(
        'OffscreenCanvas not supported in this browser. Falling back to 2D view.',
        { operation: 'init', fallbackMode: '2d-only' }
      );
    }

    this.canvas = canvas;
    
    try {
      // Create worker with retry logic
      const { result: worker } = await retryWithBackoff(
        async () => {
          try {
            return new Worker(
              new URL('../workers/provenance-canvas-worker.ts', import.meta.url),
              { type: 'module' }
            );
          } catch (error) {
            const errorObj = error instanceof Error ? error : new Error(String(error));
            (errorObj as any).name = 'NetworkError'; // Mark as retryable
            throw errorObj;
          }
        },
        this.retryOptions
      );
      
      this.worker = worker;

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

      // Track message latency
      const initMessageId = `init-${Date.now()}`;
      performanceMonitoringService.trackMessageStart('init', initMessageId);

      // Send init message
      this.sendMessage({
        type: 'init',
        payload: { canvas, options }
      });

      // Wait for initialization with timeout
      await this.waitForMessage('initialized', 10000);
      
      // Track latency end
      performanceMonitoringService.trackMessageEnd(initMessageId);
      
      this.initialized = true;
      this.restartAttempts = 0; // Reset on successful init
    } catch (error) {
      // Clean up on failure
      if (this.worker) {
        this.worker.terminate();
        this.worker = null;
      }
      this.canvas = null;
      this.initialized = false;
      
      const errorObj = error instanceof Error ? error : new Error(String(error));
      errorLoggingService.logError(errorObj, {
        service: 'ProvenanceCanvasWorkerService',
        action: 'init',
        metadata: { options },
        severity: 'error'
      });
      
      // Set fallback mode
      this.fallbackMode = '2d-only';
      
      throw new WorkerError(
        `Failed to initialize worker: ${errorObj.message}. Falling back to 2D view.`,
        { operation: 'init', fallbackMode: '2d-only' }
      );
    }
  }

  /**
   * Load provenance chain into worker
   */
  loadProvenanceChain(chain: ProvenanceChain): void {
    if (!this.initialized || !this.worker) {
      // Store chain for recovery
      this.currentChain = chain;
      throw new WorkerError('Worker not initialized', { operation: 'loadProvenanceChain' });
    }

    try {
      this.currentChain = chain;
      this.sendMessage({
        type: 'load',
        payload: { chain }
      });
    } catch (error) {
      const errorObj = error instanceof Error ? error : new Error(String(error));
      errorLoggingService.logError(errorObj, {
        service: 'ProvenanceCanvasWorkerService',
        action: 'loadProvenanceChain',
        severity: 'error'
      });
      throw errorObj;
    }
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
   * Handle worker errors with recovery
   */
  private handleWorkerError(error: Error | ErrorEvent): void {
    const errorObj = error instanceof Error ? error : new Error(error.message || 'Unknown worker error');
    console.error('Worker error occurred:', errorObj);
    
    // Log error
    errorLoggingService.logError(errorObj, {
      service: 'ProvenanceCanvasWorkerService',
      action: 'handleWorkerError',
      severity: 'error'
    });
    
    // Notify any error handlers
    const errorHandler = this.messageHandlers.get('error');
    if (errorHandler) {
      errorHandler({
        message: errorObj.message,
        type: 'worker-error'
      });
    }
    
    // Attempt worker recovery if not exceeded max attempts
    if (this.restartAttempts < this.maxRestartAttempts && this.canvas) {
      this.restartAttempts++;
      this.recoverWorker();
    } else {
      // Max restart attempts reached, set fallback mode
      this.fallbackMode = '2d-only';
      if (this.worker) {
        this.worker.terminate();
        this.worker = null;
      }
      this.initialized = false;
    }
  }

  /**
   * Recover worker after crash
   */
  private async recoverWorker(): Promise<void> {
    try {
      // Dispose current worker
      if (this.worker) {
        this.worker.terminate();
        this.worker = null;
      }
      
      this.initialized = false;
      
      // Reinitialize if canvas is still available
      if (this.canvas) {
        const options = {
          width: this.canvas.width,
          height: this.canvas.height,
          antialias: true
        };
        
        await this.init(this.canvas, options);
        
        // Reload provenance chain if available
        if (this.currentChain) {
          this.loadProvenanceChain(this.currentChain);
        }
        
        // Notify recovery
        const recoveryHandler = this.messageHandlers.get('recovered');
        if (recoveryHandler) {
          recoveryHandler({ attempts: this.restartAttempts });
        }
      }
    } catch (recoveryError) {
      const errorObj = recoveryError instanceof Error ? recoveryError : new Error(String(recoveryError));
      errorLoggingService.logError(errorObj, {
        service: 'ProvenanceCanvasWorkerService',
        action: 'recoverWorker',
        metadata: { restartAttempts: this.restartAttempts },
        severity: 'error'
      });
      
      // Set fallback mode on recovery failure
      this.fallbackMode = '2d-only';
    }
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

  /**
   * Get fallback mode
   */
  getFallbackMode(): 'normal' | '2d-only' {
    return this.fallbackMode;
  }

  /**
   * Manually trigger fallback mode
   */
  setFallbackMode(mode: 'normal' | '2d-only'): void {
    this.fallbackMode = mode;
    if (mode === '2d-only' && this.worker) {
      // Dispose worker when switching to 2D-only mode
      this.dispose();
    }
  }
}

