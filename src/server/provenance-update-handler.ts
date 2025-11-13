/**
 * Provenance Update Handler
 * 
 * Server-side handler for real-time provenance chain updates.
 * Watches evolution directories for file changes and broadcasts updates to clients.
 */

import { Server as SocketIOServer, Socket } from 'socket.io';
import * as fs from 'fs/promises';
import * as path from 'path';
import { watch, FSWatcher } from 'chokidar';
import { ChainUpdate, SlideUpdate, CardUpdate, Conflict } from '../../ui/src/types/provenance-updates';
import { IncrementalDiffService } from '../../ui/src/services/incremental-diff-service';

interface FileContent {
  path: string;
  content: string;
  lastModified: number;
}

interface EvolutionWatcher {
  path: string;
  watcher: FSWatcher;
  fileContents: Map<string, FileContent>;
  subscribers: Set<string>; // Set of socket IDs
}

export class ProvenanceUpdateHandler {
  private io: SocketIOServer;
  private watchers: Map<string, EvolutionWatcher> = new Map();
  private pendingUpdates: Map<string, NodeJS.Timeout> = new Map();
  private diffService: IncrementalDiffService;

  constructor(io: SocketIOServer) {
    this.io = io;
    this.diffService = new IncrementalDiffService();
  }

  /**
   * Watch evolution directory for file changes.
   * 
   * Starts watching a directory for JSONL/CanvasL file changes. When files
   * are added, modified, or deleted, incremental updates are generated and
   * broadcast to subscribed clients.
   * 
   * @param {string} evolutionPath - Path to the evolution directory
   * @returns {Promise<void>} Promise that resolves when watching starts
   * 
   * @example
   * ```typescript
   * await handler.watchEvolutionDirectory('/evolutions/advanced-automaton');
   * // Directory is now being watched for changes
   * ```
   */
  async watchEvolutionDirectory(evolutionPath: string): Promise<void> {
    if (this.watchers.has(evolutionPath)) {
      console.log(`Already watching ${evolutionPath}`);
      return;
    }

    // Normalize path
    const normalizedPath = path.resolve(evolutionPath);
    
    // Check if directory exists
    try {
      const stats = await fs.stat(normalizedPath);
      if (!stats.isDirectory()) {
        throw new Error(`${normalizedPath} is not a directory`);
      }
    } catch (error) {
      throw new Error(`Cannot watch ${normalizedPath}: ${error instanceof Error ? error.message : String(error)}`);
    }

    // Create watcher
    const watcher = watch(normalizedPath, {
      ignored: /(^|[\/\\])\../, // Ignore dotfiles
      persistent: true,
      ignoreInitial: false,
      awaitWriteFinish: {
        stabilityThreshold: 500,
        pollInterval: 100
      }
    });

    // Load initial file contents
    const fileContents = new Map<string, FileContent>();
    await this.loadDirectoryFiles(normalizedPath, fileContents);

    // Create watcher entry
    const evolutionWatcher: EvolutionWatcher = {
      path: normalizedPath,
      watcher,
      fileContents,
      subscribers: new Set()
    };

    this.watchers.set(evolutionPath, evolutionWatcher);

    // Setup file change handlers
    watcher.on('add', (filePath) => {
      this.handleFileChange(evolutionPath, filePath, 'add');
    });

    watcher.on('change', (filePath) => {
      this.handleFileChange(evolutionPath, filePath, 'change');
    });

    watcher.on('unlink', (filePath) => {
      this.handleFileChange(evolutionPath, filePath, 'unlink');
    });

    watcher.on('error', (error) => {
      console.error(`Error watching ${evolutionPath}:`, error);
    });

    console.log(`📁 Watching evolution directory: ${normalizedPath}`);
  }

  /**
   * Stop watching evolution directory.
   * 
   * Stops watching the specified directory and cleans up resources.
   * 
   * @param {string} evolutionPath - Path to stop watching
   */
  stopWatching(evolutionPath: string): void {
    const watcher = this.watchers.get(evolutionPath);
    if (!watcher) {
      return;
    }

    watcher.watcher.close();
    this.watchers.delete(evolutionPath);
    console.log(`📁 Stopped watching: ${evolutionPath}`);
  }

  /**
   * Subscribe client to evolution directory updates.
   * 
   * Adds a client (socket) to the list of subscribers for an evolution path.
   * The client will receive updates when files in that directory change.
   * 
   * @param {string} evolutionPath - Evolution path to subscribe to
   * @param {string} socketId - Socket ID of the subscribing client
   */
  subscribeClient(evolutionPath: string, socketId: string): void {
    const watcher = this.watchers.get(evolutionPath);
    if (!watcher) {
      // Start watching if not already watching
      this.watchEvolutionDirectory(evolutionPath).catch(error => {
        console.error(`Failed to start watching ${evolutionPath}:`, error);
      });
      return;
    }

    watcher.subscribers.add(socketId);
    console.log(`👤 Client ${socketId} subscribed to ${evolutionPath}`);
  }

  /**
   * Unsubscribe client from evolution directory updates.
   * 
   * Removes a client from the list of subscribers. If no clients remain,
   * watching may be stopped to save resources.
   * 
   * @param {string} evolutionPath - Evolution path to unsubscribe from
   * @param {string} socketId - Socket ID of the unsubscribing client
   */
  unsubscribeClient(evolutionPath: string, socketId: string): void {
    const watcher = this.watchers.get(evolutionPath);
    if (!watcher) {
      return;
    }

    watcher.subscribers.delete(socketId);
    console.log(`👤 Client ${socketId} unsubscribed from ${evolutionPath}`);

    // Stop watching if no subscribers
    if (watcher.subscribers.size === 0) {
      this.stopWatching(evolutionPath);
    }
  }

  /**
   * Broadcast update to all subscribed clients.
   * 
   * Sends an update to all clients subscribed to the evolution path.
   * 
   * @param {string} evolutionPath - Evolution path
   * @param {ChainUpdate | SlideUpdate | CardUpdate} update - Update to broadcast
   */
  broadcastUpdate(evolutionPath: string, update: ChainUpdate | SlideUpdate | CardUpdate): void {
    const watcher = this.watchers.get(evolutionPath);
    if (!watcher || watcher.subscribers.size === 0) {
      return;
    }

    // Determine event type based on update
    let eventType: string;
    if ('type' in update && (update.type.startsWith('node:') || update.type.startsWith('edge:') || update.type === 'chain:rebuilt')) {
      eventType = 'provenance:chain-update';
    } else if ('slideId' in update) {
      eventType = 'provenance:slide-update';
    } else if ('cardId' in update) {
      eventType = 'provenance:card-update';
    } else {
      return;
    }

    // Broadcast to all subscribers
    watcher.subscribers.forEach(socketId => {
      const socket = this.io.sockets.sockets.get(socketId);
      if (socket) {
        socket.emit(eventType, update);
      }
    });
  }

  /**
   * Handle file change event.
   */
  private async handleFileChange(evolutionPath: string, filePath: string, event: 'add' | 'change' | 'unlink'): Promise<void> {
    // Only process JSONL/CanvasL files
    if (!filePath.endsWith('.jsonl') && !filePath.endsWith('.canvasl')) {
      return;
    }

    const watcher = this.watchers.get(evolutionPath);
    if (!watcher) {
      return;
    }

    // Debounce rapid changes
    const debounceKey = `${evolutionPath}:${filePath}`;
    const existingTimeout = this.pendingUpdates.get(debounceKey);
    if (existingTimeout) {
      clearTimeout(existingTimeout);
    }

    const timeout = setTimeout(async () => {
      this.pendingUpdates.delete(debounceKey);
      await this.processFileChange(evolutionPath, filePath, event, watcher);
    }, 500); // 500ms debounce

    this.pendingUpdates.set(debounceKey, timeout);
  }

  /**
   * Process file change.
   */
  private async processFileChange(
    evolutionPath: string,
    filePath: string,
    event: 'add' | 'change' | 'unlink',
    watcher: EvolutionWatcher
  ): Promise<void> {
    try {
      const oldContent = watcher.fileContents.get(filePath)?.content || '';
      let newContent = '';

      if (event !== 'unlink') {
        try {
          newContent = await fs.readFile(filePath, 'utf-8');
        } catch (error) {
          console.error(`Failed to read file ${filePath}:`, error);
          return;
        }
      }

      // Generate incremental update
      const update = await this.generateIncrementalUpdate(
        evolutionPath,
        filePath,
        oldContent,
        newContent,
        event
      );

      if (update) {
        // Update file content cache
        if (event === 'unlink') {
          watcher.fileContents.delete(filePath);
        } else {
          watcher.fileContents.set(filePath, {
            path: filePath,
            content: newContent,
            lastModified: Date.now()
          });
        }

        // Broadcast update
        this.broadcastUpdate(evolutionPath, update);
      }
    } catch (error) {
      console.error(`Error processing file change for ${filePath}:`, error);
    }
  }

  /**
   * Generate incremental update from file change.
   */
  private async generateIncrementalUpdate(
    evolutionPath: string,
    filePath: string,
    oldContent: string,
    newContent: string,
    event: 'add' | 'change' | 'unlink'
  ): Promise<ChainUpdate | null> {
    if (event === 'unlink') {
      // File deleted - signal rebuild
      return {
        type: 'chain:rebuilt',
        evolutionPath,
        timestamp: Date.now(),
        clientId: 'server',
        data: {
          chain: undefined
        }
      };
    }

    // Use incremental diff service to generate specific updates
    try {
      const updates = this.diffService.generateIncrementalUpdates(
        evolutionPath,
        filePath,
        oldContent,
        newContent,
        'server'
      );

      // If we have specific updates, use the first one (or combine them)
      // For now, if there are multiple updates, we'll send a rebuild signal
      if (updates.length === 0) {
        return null; // No changes detected
      } else if (updates.length === 1) {
        return updates[0];
      } else {
        // Multiple updates - send rebuild signal for now
        // In a full implementation, we could batch multiple updates
        return {
          type: 'chain:rebuilt',
          evolutionPath,
          timestamp: Date.now(),
          clientId: 'server',
          data: {
            chain: undefined
          }
        };
      }
    } catch (error) {
      console.error(`Failed to generate incremental diff for ${filePath}:`, error);
      // Fallback to rebuild signal
      return {
        type: 'chain:rebuilt',
        evolutionPath,
        timestamp: Date.now(),
        clientId: 'server',
        data: {
          chain: undefined
        }
      };
    }
  }

  /**
   * Load all files in directory.
   */
  private async loadDirectoryFiles(dirPath: string, fileContents: Map<string, FileContent>): Promise<void> {
    try {
      const entries = await fs.readdir(dirPath, { withFileTypes: true });

      for (const entry of entries) {
        const fullPath = path.join(dirPath, entry.name);

        if (entry.isDirectory()) {
          // Recursively load subdirectories
          await this.loadDirectoryFiles(fullPath, fileContents);
        } else if (entry.isFile() && (entry.name.endsWith('.jsonl') || entry.name.endsWith('.canvasl'))) {
          try {
            const content = await fs.readFile(fullPath, 'utf-8');
            fileContents.set(fullPath, {
              path: fullPath,
              content,
              lastModified: Date.now()
            });
          } catch (error) {
            console.error(`Failed to load file ${fullPath}:`, error);
          }
        }
      }
    } catch (error) {
      console.error(`Failed to load directory ${dirPath}:`, error);
    }
  }

  /**
   * Get active watchers.
   */
  getActiveWatchers(): string[] {
    return Array.from(this.watchers.keys());
  }

  /**
   * Cleanup all watchers.
   */
  cleanup(): void {
    // Clear pending updates
    for (const timeout of this.pendingUpdates.values()) {
      clearTimeout(timeout);
    }
    this.pendingUpdates.clear();

    // Stop all watchers
    for (const [evolutionPath, watcher] of this.watchers.entries()) {
      watcher.watcher.close();
    }
    this.watchers.clear();
  }
}

