#!/usr/bin/env tsx
/**
 * Bipartite-BQF Synchronizer
 * 
 * Synchronizes bipartite metadata between CanvasL files and Obsidian frontmatter
 * Implements bidirectional sync with conflict detection and resolution
 */

import * as fs from 'fs';
import * as path from 'path';
import * as yaml from 'js-yaml';

// Note: Import would be from meta-log-db package in production
// import { BipartiteBQFValidator } from 'meta-log-db/src/validation/bipartite-bqf-validator';

/**
 * Bipartite metadata types (mirrored from canvasl-language.ts)
 */
export interface BQFObject {
  form: string;
  coefficients?: number[];
  signature?: string;
  variables?: string[];
  polynomial?: string;
  symbol?: string;
  procedure?: string;
}

export interface BQFTransformation {
  from: BQFObject;
  to: BQFObject;
  transformation?: string;
  polynomial?: string;
}

export interface PolynomialObject {
  monad: number[];
  functor: number[];
  perceptron: number[];
}

export interface BipartiteMetadata {
  partition?: 'topology' | 'system' | 'topology-system' | 'topology-topology' | 'system-system';
  bqf?: BQFObject | BQFTransformation;
  polynomial?: PolynomialObject;
  progression?: string;
  mapping?: string;
}

export interface CanvasLASTNode {
  type: 'node' | 'edge' | 'directive' | 'r5rs-call' | 'reference';
  id?: string;
  line: number;
  column: number;
  length: number;
  metadata?: {
    dimension?: string;
    r5rsFunction?: string;
    fromNode?: string;
    toNode?: string;
    bipartite?: BipartiteMetadata;
  };
}

/**
 * Parse CanvasL content (simplified version)
 */
function parseCanvasLAST(content: string): CanvasLASTNode[] {
  const ast: CanvasLASTNode[] = [];
  const lines = content.split('\n');

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    if (!line.trim()) continue;

    try {
      const entry = JSON.parse(line);
      if (entry.id) {
        const node: CanvasLASTNode = {
          type: entry.type === 'vertical' || entry.type === 'horizontal' || entry.type === 'transition'
            ? 'edge'
            : entry.function && entry.function.startsWith('r5rs:')
            ? 'r5rs-call'
            : 'node',
          id: entry.id,
          line: i + 1,
          column: 0,
          length: line.length,
          metadata: {
            dimension: entry.dimension,
            r5rsFunction: entry.function,
            fromNode: entry.from || entry.fromNode,
            toNode: entry.to || entry.toNode,
            bipartite: entry.bipartite
          }
        };
        ast.push(node);
      }
    } catch (e) {
      // Skip invalid JSON lines
    }
  }

  return ast;
}

/**
 * Conflict information
 */
export interface Conflict {
  nodeId: string;
  canvaslPath?: string;
  frontmatterPath?: string;
  canvaslTimestamp?: string;
  frontmatterTimestamp?: string;
  canvaslBipartite?: BipartiteMetadata;
  frontmatterBipartite?: BipartiteMetadata;
  reason: string;
}

/**
 * Conflict resolution strategy
 */
export type ConflictResolution = 'canvasl' | 'frontmatter' | 'merge' | 'manual';

/**
 * Synchronization configuration
 */
export interface SyncConfig {
  updateFrontmatterFromCanvasL?: boolean;
  updateCanvasLFromFrontmatter?: boolean;
  conflictResolution?: ConflictResolution;
  createMissingNodes?: boolean;
}

/**
 * Bipartite-BQF Synchronizer
 */
export class BipartiteBQFSynchronizer {
  private config: SyncConfig;

  constructor(config: SyncConfig = {}) {
    this.config = {
      updateFrontmatterFromCanvasL: true,
      updateCanvasLFromFrontmatter: false,
      conflictResolution: 'manual',
      createMissingNodes: false,
      ...config
    };
  }

  /**
   * Sync CanvasL node to frontmatter file
   */
  async syncCanvasLToFrontmatter(
    canvaslNode: CanvasLASTNode,
    frontmatterPath: string
  ): Promise<{ updated: boolean; conflict?: Conflict }> {
    if (!canvaslNode.metadata?.bipartite) {
      return { updated: false };
    }

    // Read existing frontmatter
    const existing = this.readFrontmatter(frontmatterPath);
    if (!existing) {
      // Create new frontmatter if file doesn't exist or has no frontmatter
      await this.createFrontmatterWithBipartite(frontmatterPath, canvaslNode);
      return { updated: true };
    }

    // Check for conflicts
    const conflict = this.detectConflict(canvaslNode, existing);
    if (conflict) {
      return { updated: false, conflict };
    }

    // Update frontmatter
    if (this.config.updateFrontmatterFromCanvasL) {
      await this.updateFrontmatterBipartite(frontmatterPath, existing, canvaslNode.metadata.bipartite);
      return { updated: true };
    }

    return { updated: false };
  }

  /**
   * Sync frontmatter to CanvasL file
   */
  async syncFrontmatterToCanvasL(
    frontmatterPath: string,
    canvaslPath: string,
    nodeId: string
  ): Promise<{ updated: boolean; conflict?: Conflict }> {
    const frontmatter = this.readFrontmatter(frontmatterPath);
    if (!frontmatter?.bipartite) {
      return { updated: false };
    }

    // Read CanvasL file
    const canvaslContent = fs.readFileSync(canvaslPath, 'utf-8');
    const ast = parseCanvasLAST(canvaslContent);
    const canvaslNode = ast.find(n => n.id === nodeId);

    if (!canvaslNode) {
      if (this.config.createMissingNodes) {
        // Create new node in CanvasL (simplified - would need full CanvasL writer)
        console.warn('Creating missing CanvasL nodes not yet implemented');
      }
      return { updated: false };
    }

    // Check for conflicts
    if (canvaslNode.metadata?.bipartite) {
      const conflict = this.detectConflict(canvaslNode, frontmatter);
      if (conflict) {
        return { updated: false, conflict };
      }
    }

    // Update CanvasL
    if (this.config.updateCanvasLFromFrontmatter) {
      await this.updateCanvasLBipartite(canvaslPath, canvaslNode, frontmatter.bipartite!);
      return { updated: true };
    }

    return { updated: false };
  }

  /**
   * Detect conflicts between CanvasL and frontmatter
   */
  detectConflict(
    canvaslNode: CanvasLASTNode,
    frontmatter: { frontmatter: any; body: string }
  ): Conflict | null {
    const canvaslBipartite = canvaslNode.metadata?.bipartite;
    const frontmatterBipartite = frontmatter.frontmatter?.bipartite;

    if (!canvaslBipartite && !frontmatterBipartite) {
      return null; // No conflict if both missing
    }

    if (!canvaslBipartite || !frontmatterBipartite) {
      return null; // No conflict if one is missing (can sync)
    }

    // Compare content (simplified - full implementation would hash content)
    const canvaslStr = JSON.stringify(canvaslBipartite);
    const frontmatterStr = JSON.stringify(frontmatterBipartite);

    if (canvaslStr !== frontmatterStr) {
      // Check timestamps if available
      const canvaslTimestamp = canvaslNode.metadata?.bipartite ? undefined : undefined; // Would extract from node
      const frontmatterTimestamp = frontmatter.frontmatter?.blackboard?.lastUpdate;

      return {
        nodeId: canvaslNode.id || 'unknown',
        canvaslBipartite,
        frontmatterBipartite,
        canvaslTimestamp,
        frontmatterTimestamp,
        reason: 'Bipartite metadata differs between CanvasL and frontmatter'
      };
    }

    return null;
  }

  /**
   * Validate frontmatter ↔ CanvasL synchronization
   */
  validateSync(
    frontmatter: { frontmatter: any; body: string },
    canvaslNode: CanvasLASTNode
  ): Array<{ code: string; message: string; path: string }> {
    const errors: Array<{ code: string; message: string; path: string }> = [];

    const frontmatterBipartite = frontmatter.frontmatter?.bipartite;
    const canvaslBipartite = canvaslNode.metadata?.bipartite;

    if (!frontmatterBipartite && !canvaslBipartite) {
      return errors; // Both missing, no sync needed
    }

    if (!frontmatterBipartite || !canvaslBipartite) {
      errors.push({
        code: 'FRONTMATTER_SYNC_MISMATCH',
        message: 'Bipartite metadata exists in one but not the other',
        path: 'bipartite'
      });
      return errors;
    }

    // Compare partition
    const frontmatterPartition = frontmatterBipartite.partition;
    const canvaslPartition = canvaslBipartite.partition;

    // Normalize partitions for comparison
    const normalizedFrontmatter = this.normalizePartition(frontmatterPartition);
    const normalizedCanvasl = this.normalizePartition(canvaslPartition);

    if (normalizedFrontmatter !== normalizedCanvasl) {
      errors.push({
        code: 'FRONTMATTER_SYNC_MISMATCH',
        message: `Partition mismatch: frontmatter has "${frontmatterPartition}", CanvasL has "${canvaslPartition}"`,
        path: 'bipartite.partition'
      });
    }

    // Compare dimension
    if (frontmatterBipartite.dimension !== canvaslBipartite.dimension) {
      errors.push({
        code: 'FRONTMATTER_SYNC_MISMATCH',
        message: `Dimension mismatch: frontmatter has "${frontmatterBipartite.dimension}", CanvasL has "${canvaslBipartite.dimension}"`,
        path: 'bipartite.dimension'
      });
    }

    // Compare BQF if present
    if (frontmatterBipartite.bqf && canvaslBipartite.bqf) {
      const frontmatterBQF = frontmatterBipartite.bqf;
      const canvaslBQF = canvaslBipartite.bqf;

      // Handle BQF transformation in CanvasL
      const canvaslBQFObj = 'from' in canvaslBQF ? canvaslBQF.to : canvaslBQF;

      if (frontmatterBQF.form !== canvaslBQFObj.form) {
        errors.push({
          code: 'FRONTMATTER_SYNC_MISMATCH',
          message: `BQF form mismatch: frontmatter has "${frontmatterBQF.form}", CanvasL has "${canvaslBQFObj.form}"`,
          path: 'bipartite.bqf.form'
        });
      }
    }

    return errors;
  }

  /**
   * Normalize partition for comparison
   */
  private normalizePartition(partition?: string): string {
    if (!partition) return '';
    if (partition === 'topology' || partition.startsWith('topology')) return 'topology';
    if (partition === 'system' || partition.startsWith('system')) return 'system';
    return partition;
  }

  /**
   * Resolve conflict
   */
  async resolveConflict(
    conflict: Conflict,
    resolution: ConflictResolution,
    canvaslPath?: string,
    frontmatterPath?: string
  ): Promise<void> {
    switch (resolution) {
      case 'canvasl':
        if (frontmatterPath && conflict.canvaslBipartite) {
          const existing = this.readFrontmatter(frontmatterPath);
          if (existing) {
            await this.updateFrontmatterBipartite(frontmatterPath, existing, conflict.canvaslBipartite);
          }
        }
        break;

      case 'frontmatter':
        if (canvaslPath && conflict.frontmatterBipartite) {
          const canvaslContent = fs.readFileSync(canvaslPath, 'utf-8');
          const ast = parseCanvasLAST(canvaslContent);
          const node = ast.find(n => n.id === conflict.nodeId);
          if (node) {
            await this.updateCanvasLBipartite(canvaslPath, node, conflict.frontmatterBipartite);
          }
        }
        break;

      case 'merge':
        // Merge strategy: prefer non-null values, CanvasL takes precedence
        const merged = this.mergeBipartiteMetadata(
          conflict.canvaslBipartite,
          conflict.frontmatterBipartite
        );
        if (frontmatterPath && merged) {
          const existing = this.readFrontmatter(frontmatterPath);
          if (existing) {
            await this.updateFrontmatterBipartite(frontmatterPath, existing, merged);
          }
        }
        if (canvaslPath && merged) {
          const canvaslContent = fs.readFileSync(canvaslPath, 'utf-8');
          const ast = parseCanvasLAST(canvaslContent);
          const node = ast.find(n => n.id === conflict.nodeId);
          if (node) {
            await this.updateCanvasLBipartite(canvaslPath, node, merged);
          }
        }
        break;

      case 'manual':
        // Manual resolution - just report
        console.warn(`Manual resolution required for conflict: ${conflict.nodeId}`);
        console.warn(`Reason: ${conflict.reason}`);
        break;
    }
  }

  /**
   * Read frontmatter from file
   */
  private readFrontmatter(filePath: string): { frontmatter: any; body: string } | null {
    try {
      if (!fs.existsSync(filePath)) {
        return null;
      }

      const content = fs.readFileSync(filePath, 'utf-8');
      const frontmatterMatch = content.match(/^---\n([\s\S]*?)\n---\n([\s\S]*)$/);

      if (!frontmatterMatch) {
        return { frontmatter: null, body: content };
      }

      const frontmatterYaml = frontmatterMatch[1];
      const body = frontmatterMatch[2];

      if (!frontmatterYaml) {
        return { frontmatter: null, body: content };
      }

      const frontmatter = yaml.load(frontmatterYaml) as any;
      return { frontmatter, body: body || '' };
    } catch (error) {
      console.warn(`Failed to read frontmatter from ${filePath}:`, error);
      return null;
    }
  }

  /**
   * Update frontmatter bipartite section
   */
  private async updateFrontmatterBipartite(
    filePath: string,
    existing: { frontmatter: any; body: string },
    bipartite: BipartiteMetadata
  ): Promise<void> {
    // Convert CanvasL bipartite to frontmatter format
    const frontmatterBipartite = this.canvaslToFrontmatterBipartite(bipartite);

    // Update frontmatter
    existing.frontmatter.bipartite = frontmatterBipartite;
    if (existing.frontmatter.blackboard) {
      existing.frontmatter.blackboard.lastUpdate = new Date().toISOString();
    } else {
      existing.frontmatter.blackboard = { lastUpdate: new Date().toISOString() };
    }

    // Write back to file
    const frontmatterYaml = yaml.dump(existing.frontmatter, { lineWidth: -1 });
    const newContent = `---\n${frontmatterYaml}---\n${existing.body}`;
    fs.writeFileSync(filePath, newContent, 'utf-8');
  }

  /**
   * Update CanvasL bipartite metadata
   */
  private async updateCanvasLBipartite(
    canvaslPath: string,
    node: CanvasLASTNode,
    bipartite: BipartiteMetadata
  ): Promise<void> {
    // Read CanvasL file
    const content = fs.readFileSync(canvaslPath, 'utf-8');
    const lines = content.split('\n');

    // Find and update the line containing this node
    for (let i = 0; i < lines.length; i++) {
      try {
        const entry = JSON.parse(lines[i]);
        if (entry.id === node.id) {
          // Update bipartite metadata
          entry.bipartite = bipartite;
          lines[i] = JSON.stringify(entry);
          break;
        }
      } catch (e) {
        // Skip invalid JSON lines
      }
    }

    // Write back
    fs.writeFileSync(canvaslPath, lines.join('\n'), 'utf-8');
  }

  /**
   * Create frontmatter with bipartite metadata
   */
  private async createFrontmatterWithBipartite(
    filePath: string,
    canvaslNode: CanvasLASTNode
  ): Promise<void> {
    const frontmatterBipartite = this.canvaslToFrontmatterBipartite(canvaslNode.metadata!.bipartite!);

    const frontmatter = {
      id: canvaslNode.id,
      title: canvaslNode.id,
      bipartite: frontmatterBipartite,
      blackboard: {
        lastUpdate: new Date().toISOString()
      }
    };

    const frontmatterYaml = yaml.dump(frontmatter, { lineWidth: -1 });
    const content = `---\n${frontmatterYaml}---\n\n# ${canvaslNode.id}\n\n`;
    
    // Ensure directory exists
    const dir = path.dirname(filePath);
    if (!fs.existsSync(dir)) {
      fs.mkdirSync(dir, { recursive: true });
    }

    fs.writeFileSync(filePath, content, 'utf-8');
  }

  /**
   * Convert CanvasL bipartite to frontmatter format
   */
  private canvaslToFrontmatterBipartite(bipartite: BipartiteMetadata): any {
    const result: any = {};

    // Extract partition (handle topology-system case)
    if (bipartite.partition) {
      if (bipartite.partition === 'topology' || bipartite.partition === 'system') {
        result.partition = bipartite.partition;
      } else if (bipartite.partition.startsWith('topology')) {
        result.partition = 'topology';
      } else if (bipartite.partition.startsWith('system')) {
        result.partition = 'system';
      }
    }

    // Copy BQF (handle transformation case)
    if (bipartite.bqf) {
      if ('from' in bipartite.bqf && 'to' in bipartite.bqf) {
        // BQF Transformation - use 'to' for frontmatter
        result.bqf = {
          form: bipartite.bqf.to.form,
          coefficients: bipartite.bqf.to.coefficients,
          signature: bipartite.bqf.to.signature,
          variables: bipartite.bqf.to.variables,
          polynomial: bipartite.bqf.to.polynomial,
          symbol: bipartite.bqf.to.symbol,
          procedure: bipartite.bqf.to.procedure
        };
      } else {
        result.bqf = bipartite.bqf;
      }
    }

    // Copy polynomial
    if (bipartite.polynomial) {
      result.polynomial = bipartite.polynomial;
    }

    // Copy relationships
    if (bipartite.mapping) {
      result.relationships = {
        system: bipartite.mapping.split('→')[1]?.trim() || null
      };
    }

    return result;
  }

  /**
   * Merge bipartite metadata (CanvasL takes precedence)
   */
  private mergeBipartiteMetadata(
    canvasl?: BipartiteMetadata,
    frontmatter?: BipartiteMetadata
  ): BipartiteMetadata | undefined {
    if (!canvasl && !frontmatter) return undefined;
    if (!canvasl) return frontmatter;
    if (!frontmatter) return canvasl;

    // Merge: prefer CanvasL values, fallback to frontmatter
    return {
      partition: canvasl.partition || frontmatter.partition,
      bqf: canvasl.bqf || frontmatter.bqf,
      polynomial: canvasl.polynomial || frontmatter.polynomial,
      progression: canvasl.progression || frontmatter.progression,
      mapping: canvasl.mapping || frontmatter.mapping
    };
  }
}

