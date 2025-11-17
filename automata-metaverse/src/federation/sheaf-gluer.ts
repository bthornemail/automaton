/**
 * Sheaf Gluer
 * 
 * Glues federated data from multiple peers using DAG operations
 * Uses meta-log-db: DAGManager, MetaLogNodeManager
 */

import type { MetaLogDb } from 'meta-log-db';
// @ts-ignore - extensions not yet exported in meta-log-db package.json
import { DAGManager } from 'meta-log-db/src/extensions/dag';
// @ts-ignore - extensions not yet exported in meta-log-db package.json
import { MetaLogNodeManager } from 'meta-log-db/src/extensions/metalog-node';

export interface PeerNodes {
  peerId: string;
  nodes: any[]; // MetaLogNode[]
}

export interface SheafGlueResult {
  merged: boolean;
  conflicts: string[];
  lca?: string;
}

/**
 * Sheaf Gluer
 * 
 * Glues federated data from multiple peers using DAG structure
 */
export class SheafGluer {
  private db?: MetaLogDb;
  private dagManager?: DAGManager;
  private nodeManager?: MetaLogNodeManager;

  constructor(db?: MetaLogDb) {
    this.db = db;
    if (db) {
      this.nodeManager = new MetaLogNodeManager();
      this.dagManager = new DAGManager();
    }
  }

  /**
   * Glue sheaves from multiple peers
   */
  async glueSheaves(peerNodes: PeerNodes[]): Promise<SheafGlueResult> {
    if (!this.db || !this.dagManager || !this.nodeManager) {
      throw new Error('MetaLogDb not provided');
    }

    const conflicts: string[] = [];
    const allNodes: any[] = [];
    const dag: any = {};

    // Collect all nodes and build DAG
    for (const { peerId, nodes } of peerNodes) {
      for (const node of nodes) {
        // Verify node
        const isValid = await this.nodeManager.verifyNode(node);
        if (!isValid) {
          conflicts.push(`Invalid node ${node.cid} from peer ${peerId}`);
          continue;
        }

        allNodes.push(node);

        // Add to DAG
        if (node.parent && node.parent !== 'genesis') {
          if (!dag[node.parent]) {
            dag[node.parent] = [];
          }
          if (!dag[node.cid]) {
            dag[node.cid] = [];
          }
          dag[node.parent].push(node.cid);
        }
      }
    }

    // Update DAG manager
    this.dagManager = new DAGManager(dag);

    // Find LCA for conflict resolution
    let lca: string | undefined;
    if (allNodes.length >= 2) {
      const cids = allNodes.map(n => n.cid);
      for (let i = 0; i < cids.length - 1; i++) {
        for (let j = i + 1; j < cids.length; j++) {
          const foundLCA = this.dagManager.findLCA(cids[i], cids[j]);
          if (foundLCA && (!lca || foundLCA !== cids[i] && foundLCA !== cids[j])) {
            lca = foundLCA;
          }
        }
      }
    }

    // Check for conflicts (nodes with same CID but different content)
    const cidMap = new Map<string, any[]>();
    for (const node of allNodes) {
      if (!cidMap.has(node.cid)) {
        cidMap.set(node.cid, []);
      }
      cidMap.get(node.cid)!.push(node);
    }

    for (const [cid, nodes] of cidMap.entries()) {
      if (nodes.length > 1) {
        // Check if nodes are identical
        const firstNode = nodes[0];
        const allSame = nodes.every(n => 
          JSON.stringify(n.topo) === JSON.stringify(firstNode.topo) &&
          JSON.stringify(n.geo) === JSON.stringify(firstNode.geo)
        );

        if (!allSame) {
          conflicts.push(`Conflicting nodes with CID ${cid}`);
        }
      }
    }

    return {
      merged: conflicts.length === 0,
      conflicts,
      lca
    };
  }

  /**
   * Resolve conflicts using LCA
   */
  async resolveConflicts(conflicts: string[], lca?: string): Promise<any[]> {
    if (!this.dagManager) {
      return [];
    }

    // Simple conflict resolution: use LCA as base
    // In a full implementation, this would merge changes more intelligently
    const resolved: any[] = [];

    if (lca) {
      // Get all ancestors of LCA
      const ancestors = this.dagManager.getAncestors(lca);
      resolved.push(...ancestors);
    }

    return resolved;
  }

  /**
   * Get DAG structure
   */
  getDAG(): any {
    return this.dagManager ? (this.dagManager as any).dag : {};
  }
}

