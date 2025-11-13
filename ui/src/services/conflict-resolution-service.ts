/**
 * Conflict Resolution Service
 * 
 * Handles conflict resolution for concurrent provenance chain updates.
 * Implements merge strategies (last-write-wins, operational transform, etc.)
 */

import { ChainUpdate, SlideUpdate, CardUpdate, Conflict } from '../types/provenance-updates';
import { ProvenanceChain } from './provenance-slide-service';

export type ConflictResolutionStrategy = 'last-write-wins' | 'manual' | 'operational-transform';

export interface ConflictResolutionResult {
  resolved: boolean;
  update?: ChainUpdate | SlideUpdate | CardUpdate;
  conflict?: Conflict;
  requiresManualResolution: boolean;
}

export class ConflictResolutionService {
  private conflictHistory: Map<string, Conflict[]> = new Map();
  private resolutionCallbacks: Map<string, (conflict: Conflict) => Promise<ConflictResolutionResult>> = new Map();

  /**
   * Detect conflicts between updates.
   * 
   * Compares two updates to determine if they conflict (e.g., both modify
   * the same node/edge). Returns a Conflict object if conflicts are detected.
   * 
   * @param {ChainUpdate | SlideUpdate | CardUpdate} update1 - First update
   * @param {ChainUpdate | SlideUpdate | CardUpdate} update2 - Second update
   * @returns {Conflict | null} Conflict object if conflicts detected, null otherwise
   * 
   * @example
   * ```typescript
   * const conflict = service.detectConflict(update1, update2);
   * if (conflict) {
   *   // Handle conflict
   * }
   * ```
   */
  detectConflict(
    update1: ChainUpdate | SlideUpdate | CardUpdate,
    update2: ChainUpdate | SlideUpdate | CardUpdate
  ): Conflict | null {
    // Check if updates are for the same evolution path
    if (update1.evolutionPath !== update2.evolutionPath) {
      return null;
    }

    // Check if updates are from the same client (no conflict)
    if ('clientId' in update1 && 'clientId' in update2 && update1.clientId === update2.clientId) {
      return null;
    }

    // Check if updates are for the same entity
    let conflictType: 'node' | 'edge' | 'slide' | 'card' | null = null;
    let conflictId: string | null = null;

    if ('type' in update1 && 'type' in update2) {
      // Chain updates
      const update1Chain = update1 as ChainUpdate;
      const update2Chain = update2 as ChainUpdate;

      if (update1Chain.type.startsWith('node:') && update2Chain.type.startsWith('node:')) {
        const nodeId1 = update1Chain.data.nodeId || update1Chain.data.node?.id;
        const nodeId2 = update2Chain.data.nodeId || update2Chain.data.node?.id;
        if (nodeId1 && nodeId2 && nodeId1 === nodeId2) {
          conflictType = 'node';
          conflictId = nodeId1;
        }
      } else if (update1Chain.type.startsWith('edge:') && update2Chain.type.startsWith('edge:')) {
        const edgeId1 = update1Chain.data.edgeId || update1Chain.data.edge?.id;
        const edgeId2 = update2Chain.data.edgeId || update2Chain.data.edge?.id;
        if (edgeId1 && edgeId2 && edgeId1 === edgeId2) {
          conflictType = 'edge';
          conflictId = edgeId1;
        }
      }
    } else if ('slideId' in update1 && 'slideId' in update2) {
      // Slide updates
      const update1Slide = update1 as SlideUpdate;
      const update2Slide = update2 as SlideUpdate;
      if (update1Slide.slideId === update2Slide.slideId) {
        conflictType = 'slide';
        conflictId = update1Slide.slideId;
      }
    } else if ('cardId' in update1 && 'cardId' in update2) {
      // Card updates
      const update1Card = update1 as CardUpdate;
      const update2Card = update2 as CardUpdate;
      if (update1Card.cardId === update2Card.cardId) {
        conflictType = 'card';
        conflictId = update1Card.cardId;
      }
    }

    if (!conflictType || !conflictId) {
      return null;
    }

    // Create conflict object
    const conflict: Conflict = {
      type: conflictType,
      id: conflictId,
      evolutionPath: update1.evolutionPath,
      updates: [update1, update2],
      resolution: 'pending'
    };

    // Store in history
    const historyKey = `${update1.evolutionPath}:${conflictId}`;
    if (!this.conflictHistory.has(historyKey)) {
      this.conflictHistory.set(historyKey, []);
    }
    this.conflictHistory.get(historyKey)!.push(conflict);

    return conflict;
  }

  /**
   * Resolve conflict using specified strategy.
   * 
   * Resolves a conflict using the provided strategy. Supports last-write-wins,
   * manual resolution, and operational transform strategies.
   * 
   * @param {Conflict} conflict - Conflict to resolve
   * @param {ConflictResolutionStrategy} strategy - Resolution strategy to use
   * @returns {Promise<ConflictResolutionResult>} Resolution result
   * 
   * @example
   * ```typescript
   * const result = await service.resolveConflict(conflict, 'last-write-wins');
   * if (result.resolved) {
   *   // Apply resolved update
   * }
   * ```
   */
  async resolveConflict(
    conflict: Conflict,
    strategy: ConflictResolutionStrategy
  ): Promise<ConflictResolutionResult> {
    if (conflict.resolution !== 'pending') {
      return {
        resolved: true,
        conflict,
        requiresManualResolution: false
      };
    }

    switch (strategy) {
      case 'last-write-wins':
        return this.resolveLastWriteWins(conflict);

      case 'manual':
        return {
          resolved: false,
          conflict,
          requiresManualResolution: true
        };

      case 'operational-transform':
        return this.resolveOperationalTransform(conflict);

      default:
        return {
          resolved: false,
          conflict,
          requiresManualResolution: true
        };
    }
  }

  /**
   * Merge multiple updates.
   * 
   * Merges multiple updates into a single update. Used for batching
   * updates or combining non-conflicting updates.
   * 
   * @param {Array<ChainUpdate | SlideUpdate | CardUpdate>} updates - Updates to merge
   * @returns {ChainUpdate | SlideUpdate | CardUpdate | null} Merged update or null if cannot merge
   * 
   * @example
   * ```typescript
   * const merged = service.mergeUpdates([update1, update2, update3]);
   * if (merged) {
   *   // Apply merged update
   * }
   * ```
   */
  mergeUpdates(
    updates: Array<ChainUpdate | SlideUpdate | CardUpdate>
  ): ChainUpdate | SlideUpdate | CardUpdate | null {
    if (updates.length === 0) {
      return null;
    }

    if (updates.length === 1) {
      return updates[0];
    }

    // Check if all updates are for the same evolution path
    const evolutionPath = updates[0].evolutionPath;
    if (!updates.every(u => u.evolutionPath === evolutionPath)) {
      return null; // Cannot merge updates for different paths
    }

    // For now, return the most recent update (last-write-wins)
    // In a full implementation, we'd merge the updates properly
    const sortedUpdates = updates.sort((a, b) => b.timestamp - a.timestamp);
    return sortedUpdates[0];
  }

  /**
   * Register conflict resolution callback.
   * 
   * Registers a callback to be called when a conflict requires manual
   * resolution. The callback should return a resolution result.
   * 
   * @param {string} evolutionPath - Evolution path to register callback for
   * @param {(conflict: Conflict) => Promise<ConflictResolutionResult>} callback - Resolution callback
   * 
   * @example
   * ```typescript
   * service.onConflictRequiresResolution('/evolutions/advanced-automaton', async (conflict) => {
   *   // Show UI for manual resolution
   *   const userChoice = await showConflictDialog(conflict);
   *   return { resolved: true, update: userChoice };
   * });
   * ```
   */
  onConflictRequiresResolution(
    evolutionPath: string,
    callback: (conflict: Conflict) => Promise<ConflictResolutionResult>
  ): void {
    this.resolutionCallbacks.set(evolutionPath, callback);
  }

  /**
   * Get conflict history for an entity.
   * 
   * Returns all conflicts that have occurred for a specific entity
   * (node, edge, slide, or card).
   * 
   * @param {string} evolutionPath - Evolution path
   * @param {string} entityId - Entity ID
   * @returns {Conflict[]} Array of conflicts
   */
  getConflictHistory(evolutionPath: string, entityId: string): Conflict[] {
    const historyKey = `${evolutionPath}:${entityId}`;
    return this.conflictHistory.get(historyKey) || [];
  }

  /**
   * Clear conflict history.
   * 
   * Clears all stored conflict history. Useful for cleanup or testing.
   * 
   * @param {string} [evolutionPath] - Optional evolution path to clear (clears all if not provided)
   */
  clearConflictHistory(evolutionPath?: string): void {
    if (evolutionPath) {
      // Clear conflicts for specific path
      for (const [key] of this.conflictHistory.entries()) {
        if (key.startsWith(`${evolutionPath}:`)) {
          this.conflictHistory.delete(key);
        }
      }
    } else {
      // Clear all conflicts
      this.conflictHistory.clear();
    }
  }

  /**
   * Resolve conflict using last-write-wins strategy.
   */
  private resolveLastWriteWins(conflict: Conflict): ConflictResolutionResult {
    // Sort updates by timestamp (most recent first)
    const sortedUpdates = [...conflict.updates].sort((a, b) => b.timestamp - a.timestamp);
    const winningUpdate = sortedUpdates[0];

    // Mark conflict as resolved
    conflict.resolution = 'resolved';
    conflict.resolvedBy = 'last-write-wins';
    conflict.resolvedAt = Date.now();

    return {
      resolved: true,
      update: winningUpdate,
      conflict,
      requiresManualResolution: false
    };
  }

  /**
   * Resolve conflict using operational transform strategy.
   * 
   * This is a placeholder for operational transform implementation.
   * In a full implementation, this would transform operations to be
   * commutative and resolve conflicts automatically.
   */
  private async resolveOperationalTransform(conflict: Conflict): Promise<ConflictResolutionResult> {
    // For now, fall back to last-write-wins
    // TODO: Implement proper operational transform
    return this.resolveLastWriteWins(conflict);
  }
}

// Export singleton instance
export const conflictResolutionService = new ConflictResolutionService();

