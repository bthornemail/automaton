/**
 * Incremental Diff Service
 * 
 * Parses JSONL/CanvasL files to generate specific node/edge diffs.
 * Compares old and new file contents to identify precise changes.
 */

import { ChainUpdate } from '../types/provenance-updates';
import { ProvenanceNode, ProvenanceEdge } from './provenance-slide-service';

interface JSONLLine {
  id: string;
  type: string;
  [key: string]: any;
}

interface DiffResult {
  added: JSONLLine[];
  updated: Array<{ old: JSONLLine; new: JSONLLine }>;
  removed: JSONLLine[];
}

export class IncrementalDiffService {
  /**
   * Parse JSONL content into array of objects.
   */
  private parseJSONL(content: string): JSONLLine[] {
    if (!content.trim()) {
      return [];
    }

    const lines = content.split('\n').filter(line => line.trim());
    const parsed: JSONLLine[] = [];

    for (const line of lines) {
      try {
        const parsedLine = JSON.parse(line);
        if (parsedLine.id && parsedLine.type) {
          parsed.push(parsedLine);
        }
      } catch (error) {
        // Skip invalid JSON lines
        console.warn('Failed to parse JSONL line:', line);
      }
    }

    return parsed;
  }

  /**
   * Compare two JSONL contents and generate diff.
   */
  generateDiff(oldContent: string, newContent: string): DiffResult {
    const oldLines = this.parseJSONL(oldContent);
    const newLines = this.parseJSONL(newContent);

    // Create maps for efficient lookup
    const oldMap = new Map<string, JSONLLine>();
    const newMap = new Map<string, JSONLLine>();

    oldLines.forEach(line => oldMap.set(line.id, line));
    newLines.forEach(line => newMap.set(line.id, line));

    // Find added, updated, and removed
    const added: JSONLLine[] = [];
    const updated: Array<{ old: JSONLLine; new: JSONLLine }> = [];
    const removed: JSONLLine[] = [];

    // Check for added and updated
    for (const [id, newLine] of newMap.entries()) {
      const oldLine = oldMap.get(id);
      if (!oldLine) {
        added.push(newLine);
      } else if (JSON.stringify(oldLine) !== JSON.stringify(newLine)) {
        updated.push({ old: oldLine, new: newLine });
      }
    }

    // Check for removed
    for (const [id, oldLine] of oldMap.entries()) {
      if (!newMap.has(id)) {
        removed.push(oldLine);
      }
    }

    return { added, updated, removed };
  }

  /**
   * Convert diff result to chain updates.
   */
  diffToChainUpdates(
    diff: DiffResult,
    evolutionPath: string,
    clientId: string = 'server'
  ): ChainUpdate[] {
    const updates: ChainUpdate[] = [];

    // Process added nodes/edges
    for (const added of diff.added) {
      if (this.isNode(added)) {
        updates.push({
          type: 'node:added',
          evolutionPath,
          timestamp: Date.now(),
          clientId,
          data: {
            node: this.jsonlToNode(added)
          }
        });
      } else if (this.isEdge(added)) {
        updates.push({
          type: 'edge:added',
          evolutionPath,
          timestamp: Date.now(),
          clientId,
          data: {
            edge: this.jsonlToEdge(added)
          }
        });
      }
    }

    // Process updated nodes/edges
    for (const { old: oldLine, new: newLine } of diff.updated) {
      if (this.isNode(newLine)) {
        updates.push({
          type: 'node:updated',
          evolutionPath,
          timestamp: Date.now(),
          clientId,
          data: {
            node: this.jsonlToNode(newLine)
          }
        });
      } else if (this.isEdge(newLine)) {
        updates.push({
          type: 'edge:added', // Edge updates are treated as replace
          evolutionPath,
          timestamp: Date.now(),
          clientId,
          data: {
            edge: this.jsonlToEdge(newLine)
          }
        });
      }
    }

    // Process removed nodes/edges
    for (const removed of diff.removed) {
      if (this.isNode(removed)) {
        updates.push({
          type: 'node:removed',
          evolutionPath,
          timestamp: Date.now(),
          clientId,
          data: {
            nodeId: removed.id
          }
        });
      } else if (this.isEdge(removed)) {
        updates.push({
          type: 'edge:removed',
          evolutionPath,
          timestamp: Date.now(),
          clientId,
          data: {
            edgeId: removed.id
          }
        });
      }
    }

    return updates;
  }

  /**
   * Generate incremental updates from file change.
   */
  generateIncrementalUpdates(
    evolutionPath: string,
    filePath: string,
    oldContent: string,
    newContent: string,
    clientId: string = 'server'
  ): ChainUpdate[] {
    const diff = this.generateDiff(oldContent, newContent);
    return this.diffToChainUpdates(diff, evolutionPath, clientId);
  }

  /**
   * Check if JSONL line is a node.
   */
  private isNode(line: JSONLLine): boolean {
    return line.type === 'node' || 
           line.type === 'agent' || 
           line.type === 'document' || 
           line.type === 'code' || 
           line.type === 'interaction' || 
           line.type === 'evolution';
  }

  /**
   * Check if JSONL line is an edge.
   */
  private isEdge(line: JSONLLine): boolean {
    return line.type === 'edge' || 
           (line.from && line.to) ||
           line.type === 'consumes' || 
           line.type === 'produces' || 
           line.type === 'references' || 
           line.type === 'evolves' || 
           line.type === 'interacts';
  }

  /**
   * Convert JSONL line to ProvenanceNode.
   */
  private jsonlToNode(line: JSONLLine): ProvenanceNode {
    return {
      id: line.id,
      type: line.type as any,
      position: line.position || [0, 0, 0],
      metadata: {
        timestamp: line.timestamp || Date.now(),
        file: line.file || '',
        line: line.line || 0,
        agentId: line.agentId || '',
        dimension: line.dimension,
        churchEncoding: line.churchEncoding,
        pattern: line.pattern
      },
      data: line.data || {},
      avatar: line.avatar
    };
  }

  /**
   * Convert JSONL line to ProvenanceEdge.
   */
  private jsonlToEdge(line: JSONLLine): ProvenanceEdge {
    return {
      id: line.id,
      type: line.type as any,
      from: line.from || '',
      to: line.to || '',
      metadata: {
        timestamp: line.timestamp || Date.now(),
        weight: line.weight || 1,
        context: line.context || ''
      }
    };
  }
}

// Export singleton instance
export const incrementalDiffService = new IncrementalDiffService();

