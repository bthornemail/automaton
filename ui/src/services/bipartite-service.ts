/**
 * Bipartite Relationship Service
 * Tracks markdown ↔ JSONL relationships and maintains reference graph
 */

import { markdownService } from './markdown-service';
import { databaseService } from './database-service';
import { frontMatterParser } from '../utils/front-matter-parser';

export interface BipartiteRelationship {
  markdownPath: string;
  jsonlPath: string;
  createdAt: number;
  lastModified: number;
}

export interface RelationshipGraph {
  markdownToJSONL: Map<string, Set<string>>;
  jsonlToMarkdown: Map<string, Set<string>>;
  relationships: BipartiteRelationship[];
}

export interface BQFCoefficients {
  a: number;
  b: number;
  c: number;
}

export interface BipartiteGraph {
  topology: {
    nodes: any[];
    edges: any[];
  };
  system: {
    nodes: any[];
    edges: any[];
  };
  horizontalEdges: any[];
  verticalEdges: any[];
}

export interface BipartiteService {
  getRelationships(): Promise<RelationshipGraph>;
  addRelationship(markdownPath: string, jsonlPath: string): Promise<void>;
  removeRelationship(markdownPath: string, jsonlPath: string): Promise<void>;
  getMarkdownFilesForJSONL(jsonlPath: string): Promise<string[]>;
  getJSONLFilesForMarkdown(markdownPath: string): Promise<string[]>;
  validateBipartiteStructure(): Promise<{ valid: boolean; errors: string[] }>;
  updateRelationshipsFromFiles(): Promise<void>;
  // BQF encoding extensions
  encodeBQF(dimension: string, partition: 'topology' | 'system'): BQFCoefficients;
  buildBipartiteGraphFromCanvasL(canvasLFile: string): Promise<BipartiteGraph>;
  validateBipartiteBQF(graph: BipartiteGraph): Promise<{ valid: boolean; errors: string[] }>;
  syncBipartiteFrontmatter(graph: BipartiteGraph, targetDirectory: string): Promise<{ updatedFiles: string[] }>;
}

class BipartiteServiceImpl implements BipartiteService {
  private relationships: Map<string, BipartiteRelationship> = new Map();
  private cache: RelationshipGraph | null = null;

  /**
   * Get all relationships
   */
  async getRelationships(): Promise<RelationshipGraph> {
    if (this.cache) {
      return this.cache;
    }

    await this.updateRelationshipsFromFiles();
    return this.cache || this.buildEmptyGraph();
  }

  /**
   * Build empty graph
   */
  private buildEmptyGraph(): RelationshipGraph {
    return {
      markdownToJSONL: new Map(),
      jsonlToMarkdown: new Map(),
      relationships: []
    };
  }

  /**
   * Add relationship
   */
  async addRelationship(markdownPath: string, jsonlPath: string): Promise<void> {
    const key = `${markdownPath}::${jsonlPath}`;
    
    // Update markdown file front matter
    await markdownService.linkToJSONL(markdownPath, jsonlPath);
    
    // Cache relationship
    const relationship: BipartiteRelationship = {
      markdownPath,
      jsonlPath,
      createdAt: Date.now(),
      lastModified: Date.now()
    };
    
    this.relationships.set(key, relationship);
    this.cache = null; // Invalidate cache
  }

  /**
   * Remove relationship
   */
  async removeRelationship(markdownPath: string, jsonlPath: string): Promise<void> {
    const key = `${markdownPath}::${jsonlPath}`;
    this.relationships.delete(key);
    
    // Update markdown file front matter
    const file = await markdownService.loadMarkdown(markdownPath);
    if (file) {
      const jsonlRefs = file.jsonlReferences.filter(ref => ref !== jsonlPath);
      if (jsonlRefs.length === 0) {
        delete file.frontMatter.jsonl;
      } else if (jsonlRefs.length === 1) {
        file.frontMatter.jsonl = jsonlRefs[0];
      } else {
        file.frontMatter.jsonl = jsonlRefs;
      }
      await markdownService.saveMarkdown(file);
    }
    
    this.cache = null; // Invalidate cache
  }

  /**
   * Get markdown files referencing a JSONL file
   */
  async getMarkdownFilesForJSONL(jsonlPath: string): Promise<string[]> {
    const graph = await this.getRelationships();
    return Array.from(graph.jsonlToMarkdown.get(jsonlPath) || []);
  }

  /**
   * Get JSONL files referenced by a markdown file
   */
  async getJSONLFilesForMarkdown(markdownPath: string): Promise<string[]> {
    const graph = await this.getRelationships();
    return Array.from(graph.markdownToJSONL.get(markdownPath) || []);
  }

  /**
   * Validate bipartite structure
   */
  async validateBipartiteStructure(): Promise<{ valid: boolean; errors: string[] }> {
    const errors: string[] = [];
    const graph = await this.getRelationships();
    
    // Check for cycles (shouldn't exist in bipartite graph)
    for (const [markdownPath, jsonlSet] of graph.markdownToJSONL.entries()) {
      for (const jsonlPath of jsonlSet) {
        // Verify reverse relationship exists
        const reverseSet = graph.jsonlToMarkdown.get(jsonlPath);
        if (!reverseSet || !reverseSet.has(markdownPath)) {
          errors.push(`Missing reverse relationship: ${jsonlPath} -> ${markdownPath}`);
        }
      }
    }
    
    // Check for orphaned relationships
    for (const relationship of graph.relationships) {
      // Verify markdown file exists (would need file system access)
      // Verify JSONL file exists
      try {
        await databaseService.readJSONL(relationship.jsonlPath);
      } catch {
        errors.push(`JSONL file not found: ${relationship.jsonlPath}`);
      }
    }
    
    return {
      valid: errors.length === 0,
      errors
    };
  }

  /**
   * Update relationships from files
   */
  async updateRelationshipsFromFiles(): Promise<void> {
    const markdownToJSONL = new Map<string, Set<string>>();
    const jsonlToMarkdown = new Map<string, Set<string>>();
    const relationships: BipartiteRelationship[] = [];
    
    // This would typically scan all markdown files
    // For now, we'll use a simple approach: check known markdown files
    const knownMarkdownFiles: string[] = [
      // Add known markdown files here
      // 'docs/example.md',
    ];
    
    for (const markdownPath of knownMarkdownFiles) {
      try {
        const file = await markdownService.loadMarkdown(markdownPath);
        if (file && file.jsonlReferences.length > 0) {
          const jsonlSet = new Set<string>();
          
          for (const jsonlPath of file.jsonlReferences) {
            jsonlSet.add(jsonlPath);
            
            // Add reverse relationship
            if (!jsonlToMarkdown.has(jsonlPath)) {
              jsonlToMarkdown.set(jsonlPath, new Set());
            }
            jsonlToMarkdown.get(jsonlPath)!.add(markdownPath);
            
            // Create relationship
            relationships.push({
              markdownPath,
              jsonlPath,
              createdAt: file.lastModified,
              lastModified: file.lastModified
            });
          }
          
          markdownToJSONL.set(markdownPath, jsonlSet);
        }
      } catch (error) {
        console.warn(`Failed to process markdown file ${markdownPath}:`, error);
      }
    }
    
    this.cache = {
      markdownToJSONL,
      jsonlToMarkdown,
      relationships
    };
  }

  /**
   * Encode dimension as BQF (Binary Quadratic Form)
   */
  encodeBQF(dimension: string, partition: 'topology' | 'system'): BQFCoefficients {
    const dimNum = parseInt(dimension.replace('D', '')) || 0;
    
    // Topology: mathematical foundations
    if (partition === 'topology') {
      return {
        a: 1,
        b: 0,
        c: dimNum
      };
    } else {
      // System: computational implementations
      return {
        a: dimNum,
        b: 1,
        c: 1
      };
    }
  }

  /**
   * Build bipartite graph from CanvasL file
   */
  async buildBipartiteGraphFromCanvasL(canvasLFile: string): Promise<BipartiteGraph> {
    const { databaseService } = await import('./database-service');
    const entries = await databaseService.readJSONL(canvasLFile);
    
    const topology: { nodes: any[]; edges: any[] } = { nodes: [], edges: [] };
    const system: { nodes: any[]; edges: any[] } = { nodes: [], edges: [] };
    const horizontalEdges: any[] = [];
    const verticalEdges: any[] = [];
    
    for (const entry of entries) {
      // Determine partition from frontmatter or type
      const partition = entry.frontmatter?.bipartite?.partition || 
                       (entry.type === 'topology' ? 'topology' : 'system');
      
      if (partition === 'topology') {
        topology.nodes.push(entry);
      } else {
        system.nodes.push(entry);
      }
      
      // Extract edges
      if (entry.type === 'horizontal' || entry.type?.startsWith('h:')) {
        horizontalEdges.push(entry);
      } else if (entry.type === 'vertical' || entry.type?.startsWith('v:')) {
        verticalEdges.push(entry);
      }
    }
    
    return {
      topology,
      system,
      horizontalEdges,
      verticalEdges
    };
  }

  /**
   * Validate bipartite BQF structure
   */
  async validateBipartiteBQF(graph: BipartiteGraph): Promise<{ valid: boolean; errors: string[] }> {
    const errors: string[] = [];
    
    // Validate topology partition
    for (const node of graph.topology.nodes) {
      if (!node.frontmatter?.bipartite?.partition) {
        errors.push(`Topology node ${node.id} missing bipartite partition`);
      }
      if (node.frontmatter?.bipartite?.partition !== 'topology') {
        errors.push(`Node ${node.id} in topology partition has incorrect partition value`);
      }
    }
    
    // Validate system partition
    for (const node of graph.system.nodes) {
      if (!node.frontmatter?.bipartite?.partition) {
        errors.push(`System node ${node.id} missing bipartite partition`);
      }
      if (node.frontmatter?.bipartite?.partition !== 'system') {
        errors.push(`Node ${node.id} in system partition has incorrect partition value`);
      }
    }
    
    // Validate horizontal edges (topology ↔ system)
    for (const edge of graph.horizontalEdges) {
      const fromNode = [...graph.topology.nodes, ...graph.system.nodes].find(n => n.id === edge.fromNode || n.id === edge.from);
      const toNode = [...graph.topology.nodes, ...graph.system.nodes].find(n => n.id === edge.toNode || n.id === edge.to);
      
      if (!fromNode || !toNode) {
        errors.push(`Horizontal edge ${edge.id} references non-existent nodes`);
        continue;
      }
      
      const fromPartition = fromNode.frontmatter?.bipartite?.partition;
      const toPartition = toNode.frontmatter?.bipartite?.partition;
      
      if (fromPartition === toPartition) {
        errors.push(`Horizontal edge ${edge.id} connects nodes in same partition (should connect topology ↔ system)`);
      }
    }
    
    // Validate vertical edges (dimensional progression)
    for (const edge of graph.verticalEdges) {
      const fromNode = [...graph.topology.nodes, ...graph.system.nodes].find(n => n.id === edge.fromNode || n.id === edge.from);
      const toNode = [...graph.topology.nodes, ...graph.system.nodes].find(n => n.id === edge.toNode || n.id === edge.to);
      
      if (!fromNode || !toNode) {
        errors.push(`Vertical edge ${edge.id} references non-existent nodes`);
        continue;
      }
      
      const fromDim = parseInt((fromNode.dimension || fromNode.frontmatter?.bipartite?.dimension || '0D').replace('D', ''));
      const toDim = parseInt((toNode.dimension || toNode.frontmatter?.bipartite?.dimension || '0D').replace('D', ''));
      
      if (toDim !== fromDim + 1 && !(fromDim === 7 && toDim === 0)) {
        errors.push(`Vertical edge ${edge.id} does not follow dimensional progression (${fromDim}D → ${toDim}D)`);
      }
    }
    
    return {
      valid: errors.length === 0,
      errors
    };
  }

  /**
   * Sync bipartite metadata with frontmatter
   */
  async syncBipartiteFrontmatter(graph: BipartiteGraph, targetDirectory: string): Promise<{ updatedFiles: string[] }> {
    const { markdownService } = await import('./markdown-service');
    const updatedFiles: string[] = [];
    
    // Sync topology nodes
    for (const node of graph.topology.nodes) {
      if (node.filePath) {
        try {
          const file = await markdownService.loadMarkdown(node.filePath);
          if (file) {
            file.frontMatter = {
              ...file.frontMatter,
              bipartite: {
                partition: 'topology',
                dimension: node.dimension || node.frontmatter?.bipartite?.dimension,
                bqf: node.frontmatter?.bipartite?.bqf
              }
            };
            await markdownService.saveMarkdown(file);
            updatedFiles.push(node.filePath);
          }
        } catch (error) {
          console.warn(`Failed to sync frontmatter for ${node.filePath}:`, error);
        }
      }
    }
    
    // Sync system nodes
    for (const node of graph.system.nodes) {
      if (node.filePath) {
        try {
          const file = await markdownService.loadMarkdown(node.filePath);
          if (file) {
            file.frontMatter = {
              ...file.frontMatter,
              bipartite: {
                partition: 'system',
                dimension: node.dimension || node.frontmatter?.bipartite?.dimension,
                bqf: node.frontmatter?.bipartite?.bqf
              }
            };
            await markdownService.saveMarkdown(file);
            updatedFiles.push(node.filePath);
          }
        } catch (error) {
          console.warn(`Failed to sync frontmatter for ${node.filePath}:`, error);
        }
      }
    }
    
    return { updatedFiles };
  }
}

export const bipartiteService: BipartiteService = new BipartiteServiceImpl();
