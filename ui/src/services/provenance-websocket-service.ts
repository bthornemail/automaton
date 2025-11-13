/**
 * Provenance WebSocket Service
 * 
 * Manages WebSocket connection for real-time provenance chain updates.
 * Handles incremental updates, live slide/card updates, and conflict resolution.
 */

import { unifiedWebSocket, componentBus } from './unifiedWebSocket';
import { ProvenanceChain, ProvenanceNode, ProvenanceEdge } from './provenance-slide-service';
import { ChainUpdate, SlideUpdate, CardUpdate, Conflict, UpdateHandler, ProvenanceSubscription } from '../types/provenance-updates';
import { errorLoggingService } from './error-logging-service';

export class ProvenanceWebSocketService {
  private subscriptions: Map<string, ProvenanceSubscription> = new Map();
  private updateHandlers: Map<string, UpdateHandler> = new Map();
  private updateQueue: Array<ChainUpdate | SlideUpdate | CardUpdate> = [];
  private isProcessingQueue: boolean = false;
  private clientId: string;

  constructor() {
    this.clientId = `client-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
    this.setupEventHandlers();
  }

  /**
   * Connect to WebSocket server.
   * 
   * Establishes connection to the WebSocket server using the unified WebSocket service.
   * If already connected, this method does nothing.
   * 
   * @param {string} [url] - Optional WebSocket server URL (uses default if not provided)
   * @returns {Promise<void>} Promise that resolves when connection is established
   * 
   * @example
   * ```typescript
   * const service = new ProvenanceWebSocketService();
   * await service.connect();
   * // WebSocket is now connected
   * ```
   */
  async connect(url?: string): Promise<void> {
    if (unifiedWebSocket.isConnected()) {
      return;
    }

    unifiedWebSocket.connect(url);
    
    // Wait for connection
    return new Promise((resolve, reject) => {
      const timeout = setTimeout(() => {
        reject(new Error('WebSocket connection timeout'));
      }, 10000);

      const unsubscribe = componentBus.on('ws:connected', () => {
        clearTimeout(timeout);
        unsubscribe();
        resolve();
      });

      componentBus.once('ws:error', (error) => {
        clearTimeout(timeout);
        unsubscribe();
        reject(error);
      });
    });
  }

  /**
   * Subscribe to provenance chain updates for an evolution directory.
   * 
   * Subscribes to real-time updates for a specific evolution path. When files
   * in that directory change, incremental updates will be received and processed.
   * 
   * @param {string} evolutionPath - Path to the evolution directory
   * @param {UpdateHandler} [handler] - Optional handler for updates
   * @returns {Promise<void>} Promise that resolves when subscription is confirmed
   * 
   * @example
   * ```typescript
   * await service.subscribeToChain('/evolutions/advanced-automaton', {
   *   onChainUpdate: (update) => {
   *     console.log('Chain updated:', update.type);
   *   }
   * });
   * ```
   */
  async subscribeToChain(evolutionPath: string, handler?: UpdateHandler): Promise<void> {
    if (this.subscriptions.has(evolutionPath)) {
      console.warn(`Already subscribed to ${evolutionPath}`);
      return;
    }

    if (!unifiedWebSocket.isConnected()) {
      await this.connect();
    }

    // Register handler
    if (handler) {
      this.updateHandlers.set(evolutionPath, handler);
    }

    // Send subscription request
    unifiedWebSocket.sendCommand('provenance:subscribe', {
      evolutionPath,
      clientId: this.clientId
    });

    // Wait for subscription confirmation
    return new Promise((resolve, reject) => {
      const timeout = setTimeout(() => {
        reject(new Error('Subscription timeout'));
      }, 5000);

      const unsubscribe = componentBus.once('provenance:subscribed', (data: { evolutionPath: string }) => {
        if (data.evolutionPath === evolutionPath) {
          clearTimeout(timeout);
          unsubscribe();
          
          this.subscriptions.set(evolutionPath, {
            evolutionPath,
            clientId: this.clientId,
            subscribedAt: Date.now()
          });
          
          resolve();
        }
      });

      componentBus.once('provenance:subscription-error', (error: any) => {
        clearTimeout(timeout);
        unsubscribe();
        reject(new Error(error.message || 'Subscription failed'));
      });
    });
  }

  /**
   * Unsubscribe from provenance chain updates.
   * 
   * Stops receiving updates for the specified evolution path and removes
   * the subscription and associated handlers.
   * 
   * @param {string} evolutionPath - Path to unsubscribe from
   * 
   * @example
   * ```typescript
   * service.unsubscribeFromChain('/evolutions/advanced-automaton');
   * // No longer receiving updates for this path
   * ```
   */
  unsubscribeFromChain(evolutionPath: string): void {
    if (!this.subscriptions.has(evolutionPath)) {
      return;
    }

    unifiedWebSocket.sendCommand('provenance:unsubscribe', {
      evolutionPath,
      clientId: this.clientId
    });

    this.subscriptions.delete(evolutionPath);
    this.updateHandlers.delete(evolutionPath);
  }

  /**
   * Register update handler for an evolution path.
   * 
   * Sets up handlers for chain, slide, and card updates. Handlers are called
   * when updates are received from the server.
   * 
   * @param {string} evolutionPath - Evolution path to register handler for
   * @param {UpdateHandler} handler - Handler functions for different update types
   * 
   * @example
   * ```typescript
   * service.onUpdate('/evolutions/advanced-automaton', {
   *   onChainUpdate: (update) => {
   *     // Handle chain update
   *   },
   *   onSlideUpdate: (update) => {
   *     // Handle slide update
   *   },
   *   onConflict: (conflict) => {
   *     // Handle conflict
   *   }
   * });
   * ```
   */
  onUpdate(evolutionPath: string, handler: UpdateHandler): void {
    this.updateHandlers.set(evolutionPath, handler);
  }

  /**
   * Apply incremental update to a provenance chain.
   * 
   * Applies an incremental update to an existing provenance chain, modifying
   * nodes and edges as specified by the update. Returns a new chain object
   * with the updates applied.
   * 
   * @param {ProvenanceChain} chain - Current provenance chain
   * @param {ChainUpdate} update - Incremental update to apply
   * @returns {ProvenanceChain} Updated provenance chain
   * 
   * @example
   * ```typescript
   * const updatedChain = service.applyIncrementalUpdate(chain, update);
   * // Chain now includes the incremental changes
   * ```
   */
  applyIncrementalUpdate(chain: ProvenanceChain, update: ChainUpdate): ProvenanceChain {
    const updatedChain: ProvenanceChain = {
      nodes: [...chain.nodes],
      edges: [...chain.edges]
    };

    switch (update.type) {
      case 'node:added':
        if (update.data.node) {
          updatedChain.nodes.push(update.data.node);
        }
        break;

      case 'node:updated':
        if (update.data.node) {
          const index = updatedChain.nodes.findIndex(n => n.id === update.data.node!.id);
          if (index >= 0) {
            updatedChain.nodes[index] = update.data.node;
          }
        }
        break;

      case 'node:removed':
        if (update.data.nodeId) {
          updatedChain.nodes = updatedChain.nodes.filter(n => n.id !== update.data.nodeId);
          // Also remove edges connected to this node
          updatedChain.edges = updatedChain.edges.filter(
            e => e.from !== update.data.nodeId && e.to !== update.data.nodeId
          );
        }
        break;

      case 'edge:added':
        if (update.data.edge) {
          updatedChain.edges.push(update.data.edge);
        }
        break;

      case 'edge:removed':
        if (update.data.edgeId) {
          updatedChain.edges = updatedChain.edges.filter(e => e.id !== update.data.edgeId);
        }
        break;

      case 'chain:rebuilt':
        if (update.data.chain) {
          return update.data.chain;
        }
        break;
    }

    return updatedChain;
  }

  /**
   * Send chain update to server (for conflict resolution).
   * 
   * Sends a chain update to the server, which will broadcast it to other
   * clients. Used for conflict resolution when multiple clients modify
   * the same chain.
   * 
   * @param {ChainUpdate} update - Update to send
   * 
   * @example
   * ```typescript
   * service.sendChainUpdate({
   *   type: 'node:updated',
   *   evolutionPath: '/evolutions/advanced-automaton',
   *   timestamp: Date.now(),
   *   clientId: service.clientId,
   *   data: { node: updatedNode }
   * });
   * ```
   */
  sendChainUpdate(update: ChainUpdate): void {
    unifiedWebSocket.sendCommand('provenance:update', update);
  }

  /**
   * Get client ID.
   * 
   * Returns the unique client ID for this service instance. Used for
   * identifying updates from this client.
   * 
   * @returns {string} Client ID
   */
  getClientId(): string {
    return this.clientId;
  }

  /**
   * Get active subscriptions.
   * 
   * Returns all evolution paths currently being watched for updates.
   * 
   * @returns {string[]} Array of subscribed evolution paths
   */
  getSubscriptions(): string[] {
    return Array.from(this.subscriptions.keys());
  }

  /**
   * Disconnect from WebSocket server.
   * 
   * Unsubscribes from all chains and disconnects from the WebSocket server.
   * This should be called when the service is no longer needed.
   * 
   * @example
   * ```typescript
   * service.disconnect();
   * // All subscriptions are removed and connection is closed
   * ```
   */
  disconnect(): void {
    // Unsubscribe from all chains
    for (const evolutionPath of this.subscriptions.keys()) {
      this.unsubscribeFromChain(evolutionPath);
    }

    // Clear handlers
    this.updateHandlers.clear();
    this.updateQueue = [];
  }

  /**
   * Setup event handlers for WebSocket events.
   */
  private setupEventHandlers(): void {
    // Handle chain updates
    componentBus.on('provenance:chain-update', (update: ChainUpdate) => {
      this.handleChainUpdate(update);
    });

    // Handle slide updates
    componentBus.on('provenance:slide-update', (update: SlideUpdate) => {
      this.handleSlideUpdate(update);
    });

    // Handle card updates
    componentBus.on('provenance:card-update', (update: CardUpdate) => {
      this.handleCardUpdate(update);
    });

    // Handle conflicts
    componentBus.on('provenance:conflict', (conflict: Conflict) => {
      this.handleConflict(conflict);
    });
  }

  /**
   * Handle chain update from server.
   */
  private handleChainUpdate(update: ChainUpdate): void {
    // Add to queue
    this.updateQueue.push(update);

    // Process queue if not already processing
    if (!this.isProcessingQueue) {
      this.processUpdateQueue();
    }

    // Notify handlers
    const handler = this.updateHandlers.get(update.evolutionPath);
    if (handler?.onChainUpdate) {
      try {
        handler.onChainUpdate(update);
      } catch (error) {
        const errorObj = error instanceof Error ? error : new Error(String(error));
        errorLoggingService.logError(errorObj, {
          service: 'ProvenanceWebSocketService',
          action: 'handleChainUpdate',
          metadata: { update },
          severity: 'error'
        });
      }
    }
  }

  /**
   * Handle slide update from server.
   */
  private handleSlideUpdate(update: SlideUpdate): void {
    const handler = this.updateHandlers.get(update.evolutionPath);
    if (handler?.onSlideUpdate) {
      try {
        handler.onSlideUpdate(update);
      } catch (error) {
        const errorObj = error instanceof Error ? error : new Error(String(error));
        errorLoggingService.logError(errorObj, {
          service: 'ProvenanceWebSocketService',
          action: 'handleSlideUpdate',
          metadata: { update },
          severity: 'error'
        });
      }
    }
  }

  /**
   * Handle card update from server.
   */
  private handleCardUpdate(update: CardUpdate): void {
    const handler = this.updateHandlers.get(update.evolutionPath);
    if (handler?.onCardUpdate) {
      try {
        handler.onCardUpdate(update);
      } catch (error) {
        const errorObj = error instanceof Error ? error : new Error(String(error));
        errorLoggingService.logError(errorObj, {
          service: 'ProvenanceWebSocketService',
          action: 'handleCardUpdate',
          metadata: { update },
          severity: 'error'
        });
      }
    }
  }

  /**
   * Handle conflict from server.
   */
  private handleConflict(conflict: Conflict): void {
    const handler = this.updateHandlers.get(conflict.evolutionPath);
    if (handler?.onConflict) {
      try {
        handler.onConflict(conflict);
      } catch (error) {
        const errorObj = error instanceof Error ? error : new Error(String(error));
        errorLoggingService.logError(errorObj, {
          service: 'ProvenanceWebSocketService',
          action: 'handleConflict',
          metadata: { conflict },
          severity: 'error'
        });
      }
    }
  }

  /**
   * Process update queue.
   */
  private async processUpdateQueue(): Promise<void> {
    if (this.isProcessingQueue || this.updateQueue.length === 0) {
      return;
    }

    this.isProcessingQueue = true;

    while (this.updateQueue.length > 0) {
      const update = this.updateQueue.shift();
      if (!update) continue;

      // Process update based on type
      if ('type' in update && update.type.startsWith('node:') || update.type.startsWith('edge:') || update.type === 'chain:rebuilt') {
        // Chain update - will be handled by handler
        continue;
      }

      // Small delay to batch rapid updates
      await new Promise(resolve => setTimeout(resolve, 10));
    }

    this.isProcessingQueue = false;
  }
}

// Export singleton instance
export const provenanceWebSocketService = new ProvenanceWebSocketService();

