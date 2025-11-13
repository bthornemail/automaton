/**
 * Update History Service
 * 
 * Tracks update history for provenance chains, enabling rollback capabilities.
 * Maintains a history of all updates with timestamps and metadata.
 */

import { ChainUpdate, SlideUpdate, CardUpdate } from '../types/provenance-updates';
import { ProvenanceChain } from './provenance-slide-service';

interface UpdateHistoryEntry {
  id: string;
  type: 'chain' | 'slide' | 'card';
  evolutionPath: string;
  update: ChainUpdate | SlideUpdate | CardUpdate;
  timestamp: number;
  applied: boolean;
  rolledBack: boolean;
  metadata?: {
    appliedBy?: string;
    rolledBackBy?: string;
    reason?: string;
  };
}

interface HistorySnapshot {
  id: string;
  evolutionPath: string;
  chain: ProvenanceChain;
  timestamp: number;
  updateId: string;
}

export class UpdateHistoryService {
  private history: Map<string, UpdateHistoryEntry[]> = new Map();
  private snapshots: Map<string, HistorySnapshot[]> = new Map();
  private maxHistorySize: number = 1000;
  private maxSnapshots: number = 100;

  /**
   * Record an update in history.
   */
  recordUpdate(
    evolutionPath: string,
    update: ChainUpdate | SlideUpdate | CardUpdate,
    applied: boolean = true,
    metadata?: UpdateHistoryEntry['metadata']
  ): string {
    const entryId = `update-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
    
    let updateType: 'chain' | 'slide' | 'card';
    if ('type' in update) {
      updateType = 'chain';
    } else if ('slideId' in update) {
      updateType = 'slide';
    } else {
      updateType = 'card';
    }

    const entry: UpdateHistoryEntry = {
      id: entryId,
      type: updateType,
      evolutionPath,
      update,
      timestamp: Date.now(),
      applied,
      rolledBack: false,
      metadata
    };

    if (!this.history.has(evolutionPath)) {
      this.history.set(evolutionPath, []);
    }

    const pathHistory = this.history.get(evolutionPath)!;
    pathHistory.push(entry);

    // Limit history size
    if (pathHistory.length > this.maxHistorySize) {
      pathHistory.shift();
    }

    return entryId;
  }

  /**
   * Get update history for an evolution path.
   */
  getHistory(evolutionPath: string, limit?: number): UpdateHistoryEntry[] {
    const pathHistory = this.history.get(evolutionPath) || [];
    if (limit) {
      return pathHistory.slice(-limit);
    }
    return [...pathHistory];
  }

  /**
   * Get update by ID.
   */
  getUpdate(evolutionPath: string, updateId: string): UpdateHistoryEntry | null {
    const pathHistory = this.history.get(evolutionPath) || [];
    return pathHistory.find(entry => entry.id === updateId) || null;
  }

  /**
   * Rollback an update.
   */
  rollbackUpdate(
    evolutionPath: string,
    updateId: string,
    reason?: string
  ): ChainUpdate | SlideUpdate | CardUpdate | null {
    const entry = this.getUpdate(evolutionPath, updateId);
    if (!entry || entry.rolledBack) {
      return null;
    }

    // Create inverse update
    const inverseUpdate = this.createInverseUpdate(entry.update);
    if (!inverseUpdate) {
      return null;
    }

    // Mark as rolled back
    entry.rolledBack = true;
    entry.metadata = {
      ...entry.metadata,
      rolledBackBy: 'user',
      reason
    };

    // Record rollback as new update
    this.recordUpdate(evolutionPath, inverseUpdate, true, {
      reason: `Rollback of ${updateId}: ${reason || 'No reason provided'}`
    });

    return inverseUpdate;
  }

  /**
   * Rollback to a specific timestamp.
   */
  rollbackToTimestamp(
    evolutionPath: string,
    timestamp: number,
    reason?: string
  ): ChainUpdate[] {
    const pathHistory = this.history.get(evolutionPath) || [];
    const updatesToRollback = pathHistory.filter(
      entry => entry.timestamp >= timestamp && entry.applied && !entry.rolledBack
    );

    const rollbackUpdates: ChainUpdate[] = [];
    for (const entry of updatesToRollback.reverse()) {
      const inverseUpdate = this.createInverseUpdate(entry.update);
      if (inverseUpdate && 'type' in inverseUpdate) {
        rollbackUpdates.push(inverseUpdate as ChainUpdate);
        entry.rolledBack = true;
        entry.metadata = {
          ...entry.metadata,
          rolledBackBy: 'user',
          reason: `Rollback to ${new Date(timestamp).toISOString()}: ${reason || 'No reason provided'}`
        };
      }
    }

    return rollbackUpdates;
  }

  /**
   * Create a snapshot of the current chain state.
   */
  createSnapshot(
    evolutionPath: string,
    chain: ProvenanceChain,
    updateId: string
  ): string {
    const snapshotId = `snapshot-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
    
    const snapshot: HistorySnapshot = {
      id: snapshotId,
      evolutionPath,
      chain: JSON.parse(JSON.stringify(chain)), // Deep clone
      timestamp: Date.now(),
      updateId
    };

    if (!this.snapshots.has(evolutionPath)) {
      this.snapshots.set(evolutionPath, []);
    }

    const pathSnapshots = this.snapshots.get(evolutionPath)!;
    pathSnapshots.push(snapshot);

    // Limit snapshot size
    if (pathSnapshots.length > this.maxSnapshots) {
      pathSnapshots.shift();
    }

    return snapshotId;
  }

  /**
   * Get snapshot by ID.
   */
  getSnapshot(evolutionPath: string, snapshotId: string): HistorySnapshot | null {
    const pathSnapshots = this.snapshots.get(evolutionPath) || [];
    return pathSnapshots.find(snapshot => snapshot.id === snapshotId) || null;
  }

  /**
   * Get all snapshots for an evolution path.
   */
  getSnapshots(evolutionPath: string): HistorySnapshot[] {
    return [...(this.snapshots.get(evolutionPath) || [])];
  }

  /**
   * Create inverse update for rollback.
   */
  private createInverseUpdate(
    update: ChainUpdate | SlideUpdate | CardUpdate
  ): ChainUpdate | SlideUpdate | CardUpdate | null {
    if ('type' in update) {
      const chainUpdate = update as ChainUpdate;
      
      switch (chainUpdate.type) {
        case 'node:added':
          return {
            ...chainUpdate,
            type: 'node:removed',
            data: {
              nodeId: chainUpdate.data.node?.id || chainUpdate.data.nodeId
            }
          };

        case 'node:removed':
          return {
            ...chainUpdate,
            type: 'node:added',
            data: {
              node: chainUpdate.data.node
            }
          };

        case 'node:updated':
          // For updated, we'd need the old node data, which we don't have
          // This is a limitation - we'd need to store old state
          return null;

        case 'edge:added':
          return {
            ...chainUpdate,
            type: 'edge:removed',
            data: {
              edgeId: chainUpdate.data.edge?.id || chainUpdate.data.edgeId
            }
          };

        case 'edge:removed':
          return {
            ...chainUpdate,
            type: 'edge:added',
            data: {
              edge: chainUpdate.data.edge
            }
          };

        case 'chain:rebuilt':
          // Cannot rollback a rebuild without snapshot
          return null;

        default:
          return null;
      }
    }

    // Slide and card updates don't have inverses in the same way
    // They would need to store the old state
    return null;
  }

  /**
   * Clear history for an evolution path.
   */
  clearHistory(evolutionPath: string): void {
    this.history.delete(evolutionPath);
    this.snapshots.delete(evolutionPath);
  }

  /**
   * Get history statistics.
   */
  getHistoryStats(evolutionPath: string): {
    totalUpdates: number;
    appliedUpdates: number;
    rolledBackUpdates: number;
    snapshots: number;
  } {
    const pathHistory = this.history.get(evolutionPath) || [];
    const pathSnapshots = this.snapshots.get(evolutionPath) || [];

    return {
      totalUpdates: pathHistory.length,
      appliedUpdates: pathHistory.filter(e => e.applied && !e.rolledBack).length,
      rolledBackUpdates: pathHistory.filter(e => e.rolledBack).length,
      snapshots: pathSnapshots.length
    };
  }
}

// Export singleton instance
export const updateHistoryService = new UpdateHistoryService();

