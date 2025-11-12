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
   * Initialize worker with offscreen canvas.
   * 
   * Creates and initializes a Web Worker for offscreen canvas rendering. The worker
   * handles all 3D rendering operations in a separate thread to avoid blocking the
   * main thread. If OffscreenCanvas is not supported, the method throws an error
   * and sets fallback mode to '2d-only'.
   * 
   * The initialization process includes:
   * - Creating the worker instance with retry logic
   * - Setting up error and message handlers
   * - Sending initialization message to worker
   * - Waiting for worker confirmation
   * - Tracking performance metrics
   * 
   * @param {OffscreenCanvas} canvas - Offscreen canvas to transfer to worker
   * @param {CanvasOptions} options - Canvas rendering options (width, height, antialias)
   * @returns {Promise<void>} Promise that resolves when worker is initialized
   * @throws {WorkerError} If OffscreenCanvas is not supported or initialization fails
   * 
   * @example
   * ```typescript
   * const canvas = htmlCanvas.transferControlToOffscreen();
   * const service = new ProvenanceCanvasWorkerService();
   * await service.init(canvas, { width: 800, height: 600, antialias: true });
   * // Worker is now ready for rendering
   * ```
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
   * Load provenance chain into worker for rendering.
   * 
   * Sends a provenance chain to the worker thread for 3D rendering. The chain
   * includes all nodes and edges that will be visualized in the offscreen canvas.
   * The chain is stored internally for recovery purposes if the worker crashes.
   * 
   * @param {ProvenanceChain} chain - Provenance chain with nodes and edges to render
   * @throws {WorkerError} If worker is not initialized
   * 
   * @example
   * ```typescript
   * const chain = await provenanceSlideService.buildProvenanceChain('/evolutions/advanced-automaton');
   * workerService.loadProvenanceChain(chain);
   * // Chain is now being rendered in the worker
   * ```
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
   * Handle user interaction (click/hover) on the canvas.
   * 
   * Sends interaction coordinates to the worker and waits for the selected node.
   * The worker performs raycasting to determine which node was clicked/hovered
   * based on the screen coordinates. Returns the selected node or null if no node
   * was hit.
   * 
   * The method sets up a temporary message handler and cleans it up after a timeout
   * to prevent memory leaks.
   * 
   * @param {number} x - X coordinate of the interaction (screen space)
   * @param {number} y - Y coordinate of the interaction (screen space)
   * @param {number} width - Canvas width (for coordinate normalization)
   * @param {number} height - Canvas height (for coordinate normalization)
   * @param {'click' | 'hover'} interactionType - Type of interaction
   * @returns {Promise<ProvenanceNode | null>} Promise resolving to selected node or null
   * @throws {Error} If worker is not initialized
   * 
   * @example
   * ```typescript
   * const node = await workerService.handleInteraction(
   *   event.clientX,
   *   event.clientY,
   *   canvas.width,
   *   canvas.height,
   *   'click'
   * );
   * if (node) {
   *   console.log('Selected node:', node.id);
   * }
   * ```
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
   * Update camera position and target in the worker.
   * 
   * Sends camera update message to the worker to change the viewport. This is
   * typically called when the user interacts with camera controls (e.g., OrbitControls).
   * 
   * @param {[number, number, number]} position - Camera position in 3D space
   * @param {[number, number, number]} target - Camera target/look-at point
   * @throws {Error} If worker is not initialized
   * 
   * @example
   * ```typescript
   * workerService.updateCamera([10, 10, 10], [0, 0, 0]);
   * // Camera now looks at origin from position (10, 10, 10)
   * ```
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
   * Resize the offscreen canvas in the worker.
   * 
   * Updates the canvas dimensions and adjusts the camera aspect ratio accordingly.
   * This should be called when the container element is resized to maintain
   * correct rendering proportions.
   * 
   * @param {number} width - New canvas width in pixels
   * @param {number} height - New canvas height in pixels
   * @throws {Error} If worker is not initialized
   * 
   * @example
   * ```typescript
   * window.addEventListener('resize', () => {
   *   workerService.resize(canvas.width, canvas.height);
   * });
   * ```
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
   * Dispose and clean up the worker.
   * 
   * Terminates the worker thread, clears all message handlers, and resets the
   * service state. This should be called when the component is unmounted or
   * when the service is no longer needed to free up resources.
   * 
   * @example
   * ```typescript
   * // Clean up on component unmount
   * useEffect(() => {
   *   return () => {
   *     workerService.dispose();
   *   };
   * }, []);
   * ```
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
   * Register a message handler for worker messages.
   * 
   * Sets up a handler function that will be called when the worker sends a message
   * of the specified type. Multiple handlers can be registered for different message
   * types. Handlers are called synchronously when messages arrive.
   * 
   * @param {string} type - Message type to listen for (e.g., 'initialized', 'nodeSelected')
   * @param {(data: any) => void} handler - Handler function to call when message is received
   * 
   * @example
   * ```typescript
   * workerService.onMessage('nodeSelected', (node) => {
   *   console.log('Node selected in worker:', node.id);
   * });
   * ```
   */
  onMessage(type: string, handler: (data: any) => void): void {
    this.messageHandlers.set(type, handler);
  }

  /**
   * Remove a message handler for worker messages.
   * 
   * Unregisters the handler for the specified message type. This should be called
   * when the handler is no longer needed to prevent memory leaks.
   * 
   * @param {string} type - Message type to unregister handler for
   * 
   * @example
   * ```typescript
   * workerService.offMessage('nodeSelected');
   * // Handler for 'nodeSelected' messages is now removed
   * ```
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
   * Check if Web Workers and OffscreenCanvas are supported in the current browser.
   * 
   * Performs a static check to determine if the required browser APIs are available.
   * This should be called before attempting to initialize the service to provide
   * appropriate fallback behavior.
   * 
   * @returns {boolean} True if both Worker and OffscreenCanvas are supported
   * 
   * @example
   * ```typescript
   * if (ProvenanceCanvasWorkerService.isSupported()) {
   *   await service.init(canvas, options);
   * } else {
   *   // Use 2D fallback rendering
   * }
   * ```
   */
  static isSupported(): boolean {
    return typeof Worker !== 'undefined' && typeof OffscreenCanvas !== 'undefined';
  }

  /**
   * Get worker initialization status.
   * 
   * Returns whether the worker has been successfully initialized and is ready
   * to receive commands. This can be used to check service state before
   * calling methods that require an initialized worker.
   * 
   * @returns {boolean} True if worker is initialized, false otherwise
   * 
   * @example
   * ```typescript
   * if (service.isInitialized()) {
   *   service.loadProvenanceChain(chain);
   * }
   * ```
   */
  isInitialized(): boolean {
    return this.initialized;
  }

  /**
   * Get current fallback mode.
   * 
   * Returns the current fallback mode, which indicates whether the service is
   * operating normally ('normal') or has fallen back to 2D-only rendering
   * ('2d-only') due to lack of browser support or worker failures.
   * 
   * @returns {'normal' | '2d-only'} Current fallback mode
   * 
   * @example
   * ```typescript
   * const mode = service.getFallbackMode();
   * if (mode === '2d-only') {
   *   // Render 2D fallback UI
   * }
   * ```
   */
  getFallbackMode(): 'normal' | '2d-only' {
    return this.fallbackMode;
  }

  /**
   * Manually set fallback mode.
   * 
   * Allows manual control of the fallback mode. Setting mode to '2d-only' will
   * dispose the worker if it exists. This can be useful for forcing fallback
   * behavior or recovering from errors.
   * 
   * @param {'normal' | '2d-only'} mode - Fallback mode to set
   * 
   * @example
   * ```typescript
   * // Force 2D fallback mode
   * service.setFallbackMode('2d-only');
   * // Worker is disposed and 2D rendering should be used
   * ```
   */
  setFallbackMode(mode: 'normal' | '2d-only'): void {
    this.fallbackMode = mode;
    if (mode === '2d-only' && this.worker) {
      // Dispose worker when switching to 2D-only mode
      this.dispose();
    }
  }
}

