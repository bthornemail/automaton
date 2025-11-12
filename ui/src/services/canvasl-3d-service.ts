/**
 * CanvasL to 3D Service
 * Converts CanvasL/JSONL files to 3D node/edge structures for Metaverse rendering
 */

import { CanvasGraph, JSONLNode, JSONLEdge } from './jsonl-canvas-service';
import { jsonlCanvasService } from './jsonl-canvas-service';
import { databaseService } from './database-service';

export interface Node3D {
  id: string;
  type: string;
  position: [number, number, number];
  rotation?: [number, number, number];
  scale?: [number, number, number];
  color: string;
  radius: number;
  text?: string;
  metadata?: any;
  dimension?: number;
  churchEncoding?: string;
}

export interface Edge3D {
  id: string;
  type: 'vertical' | 'horizontal' | 'transition' | 'self-ref';
  from: string;
  to: string;
  fromPosition: [number, number, number];
  toPosition: [number, number, number];
  color: string;
  label?: string;
  metadata?: any;
}

export interface Canvas3D {
  nodes: Map<string, Node3D>;
  edges: Map<string, Edge3D>;
  nodeList: Node3D[];
  edgeList: Edge3D[];
  bounds: {
    min: [number, number, number];
    max: [number, number, number];
    center: [number, number, number];
  };
}

export interface BipartiteCanvas3D {
  topology: Canvas3D;
  system: Canvas3D;
  horizontalEdges: Edge3D[];
  verticalEdges: Edge3D[];
}

export interface CanvasL3DService {
  loadCanvasLTo3D(filename: string): Promise<Canvas3D>;
  convertGraphTo3D(graph: CanvasGraph): Canvas3D;
  convert3DToGraph(canvas3D: Canvas3D): CanvasGraph;
  updateNode3D(canvas3D: Canvas3D, nodeId: string, updates: Partial<Node3D>): Canvas3D;
  addNode3D(canvas3D: Canvas3D, node: Node3D): Canvas3D;
  deleteNode3D(canvas3D: Canvas3D, nodeId: string): Canvas3D;
  addEdge3D(canvas3D: Canvas3D, edge: Edge3D): Canvas3D;
  deleteEdge3D(canvas3D: Canvas3D, edgeId: string): Canvas3D;
  sync3DToCanvasL(canvas3D: Canvas3D, filename: string): Promise<void>;
  // Bipartite-BQF extensions
  loadBipartiteCanvasL(topologyFile: string, systemFile: string): Promise<BipartiteCanvas3D>;
  renderBipartitePartition(canvas3D: Canvas3D, partition: 'topology' | 'system'): Canvas3D;
  extractBipartiteStructure(canvas3D: Canvas3D): BipartiteCanvas3D;
}

class CanvasL3DServiceImpl implements CanvasL3DService {
  /**
   * Load CanvasL file and convert to 3D structure
   */
  async loadCanvasLTo3D(filename: string): Promise<Canvas3D> {
    try {
      const entries = await databaseService.readJSONL(filename);
      const jsonlContent = entries.map(entry => JSON.stringify(entry)).join('\n');
      const graph = jsonlCanvasService.parseJSONL(jsonlContent);
      return this.convertGraphTo3D(graph);
    } catch (error) {
      console.error('Failed to load CanvasL to 3D:', error);
      throw error;
    }
  }

  /**
   * Convert 2D graph to 3D structure
   */
  convertGraphTo3D(graph: CanvasGraph): Canvas3D {
    const nodes3D = new Map<string, Node3D>();
    const edges3D = new Map<string, Edge3D>();
    
    let minX = Infinity, minY = Infinity, minZ = 0;
    let maxX = -Infinity, maxY = -Infinity, maxZ = 0;

    // Convert nodes to 3D
    for (const node of graph.nodeList) {
      // Extract 3D position if available, otherwise use 2D position
      const x = node.x || 0;
      const y = node.y || 0;
      const z = node.z || (this.getDimensionFromType(node.type) * 0.5); // Stack by dimension
      
      const position: [number, number, number] = [x / 100, z, -y / 100]; // Scale and convert to 3D space
      
      const node3D: Node3D = {
        id: node.id,
        type: node.type,
        position,
        rotation: node.rotation || [0, 0, 0],
        scale: node.scale || [1, 1, 1],
        color: this.getColorForType(node.type),
        radius: this.getRadiusForType(node.type),
        text: node.text,
        metadata: node,
        dimension: this.getDimensionFromType(node.type),
        churchEncoding: this.extractChurchEncoding(node)
      };
      
      nodes3D.set(node.id, node3D);
      
      // Update bounds
      minX = Math.min(minX, position[0]);
      minY = Math.min(minY, position[1]);
      minZ = Math.min(minZ, position[2]);
      maxX = Math.max(maxX, position[0]);
      maxY = Math.max(maxY, position[1]);
      maxZ = Math.max(maxZ, position[2]);
    }

    // Convert edges to 3D
    for (const edge of graph.edgeList) {
      const fromNode = nodes3D.get(edge.from || edge.fromNode || '');
      const toNode = nodes3D.get(edge.to || edge.toNode || '');
      
      if (!fromNode || !toNode) continue;
      
      const edge3D: Edge3D = {
        id: edge.id,
        type: edge.type,
        from: edge.from || edge.fromNode || '',
        to: edge.to || edge.toNode || '',
        fromPosition: fromNode.position,
        toPosition: toNode.position,
        color: this.getColorForEdgeType(edge.type),
        label: edge.label,
        metadata: edge
      };
      
      edges3D.set(edge.id, edge3D);
    }

    const center: [number, number, number] = [
      (minX + maxX) / 2,
      (minY + maxY) / 2,
      (minZ + maxZ) / 2
    ];

    return {
      nodes: nodes3D,
      edges: edges3D,
      nodeList: Array.from(nodes3D.values()),
      edgeList: Array.from(edges3D.values()),
      bounds: {
        min: [minX, minY, minZ],
        max: [maxX, maxY, maxZ],
        center
      }
    };
  }

  /**
   * Convert 3D structure back to 2D graph
   */
  convert3DToGraph(canvas3D: Canvas3D): CanvasGraph {
    const nodes = new Map<string, JSONLNode>();
    const edges = new Map<string, JSONLEdge>();

    // Convert 3D nodes back to 2D nodes
    for (const node3D of canvas3D.nodeList) {
      const [x, z, y] = node3D.position; // Convert back from 3D space
      
      const node: JSONLNode = {
        id: node3D.id,
        type: node3D.type,
        x: x * 100, // Scale back
        y: -y * 100, // Scale back and invert
        z: z, // Keep Z for 3D info
        text: node3D.text,
        rotation: node3D.rotation,
        scale: node3D.scale,
        ...node3D.metadata
      };
      
      nodes.set(node.id, node);
    }

    // Convert 3D edges back to 2D edges
    for (const edge3D of canvas3D.edgeList) {
      const edge: JSONLEdge = {
        id: edge3D.id,
        type: edge3D.type,
        from: edge3D.from,
        to: edge3D.to,
        fromNode: edge3D.from,
        toNode: edge3D.to,
        label: edge3D.label,
        ...edge3D.metadata
      };
      
      edges.set(edge.id, edge);
    }

    return {
      nodes,
      edges,
      nodeList: Array.from(nodes.values()),
      edgeList: Array.from(edges.values())
    };
  }

  /**
   * Update a 3D node
   */
  updateNode3D(canvas3D: Canvas3D, nodeId: string, updates: Partial<Node3D>): Canvas3D {
    const node = canvas3D.nodes.get(nodeId);
    if (!node) return canvas3D;

    const updated = { ...node, ...updates };
    const newNodes = new Map(canvas3D.nodes);
    newNodes.set(nodeId, updated);

    // Update edge positions if node moved
    const newEdges = new Map(canvas3D.edges);
    for (const [edgeId, edge] of newEdges.entries()) {
      if (edge.from === nodeId) {
        newEdges.set(edgeId, { ...edge, fromPosition: updated.position });
      }
      if (edge.to === nodeId) {
        newEdges.set(edgeId, { ...edge, toPosition: updated.position });
      }
    }

    return {
      ...canvas3D,
      nodes: newNodes,
      edges: newEdges,
      nodeList: Array.from(newNodes.values()),
      edgeList: Array.from(newEdges.values())
    };
  }

  /**
   * Add a new 3D node
   */
  addNode3D(canvas3D: Canvas3D, node: Node3D): Canvas3D {
    const newNodes = new Map(canvas3D.nodes);
    newNodes.set(node.id, node);

    return {
      ...canvas3D,
      nodes: newNodes,
      nodeList: Array.from(newNodes.values())
    };
  }

  /**
   * Delete a 3D node
   */
  deleteNode3D(canvas3D: Canvas3D, nodeId: string): Canvas3D {
    const newNodes = new Map(canvas3D.nodes);
    newNodes.delete(nodeId);

    // Remove connected edges
    const newEdges = new Map(canvas3D.edges);
    for (const [edgeId, edge] of newEdges.entries()) {
      if (edge.from === nodeId || edge.to === nodeId) {
        newEdges.delete(edgeId);
      }
    }

    return {
      ...canvas3D,
      nodes: newNodes,
      edges: newEdges,
      nodeList: Array.from(newNodes.values()),
      edgeList: Array.from(newEdges.values())
    };
  }

  /**
   * Add a new 3D edge
   */
  addEdge3D(canvas3D: Canvas3D, edge: Edge3D): Canvas3D {
    const newEdges = new Map(canvas3D.edges);
    newEdges.set(edge.id, edge);

    return {
      ...canvas3D,
      edges: newEdges,
      edgeList: Array.from(newEdges.values())
    };
  }

  /**
   * Delete a 3D edge
   */
  deleteEdge3D(canvas3D: Canvas3D, edgeId: string): Canvas3D {
    const newEdges = new Map(canvas3D.edges);
    newEdges.delete(edgeId);

    return {
      ...canvas3D,
      edges: newEdges,
      edgeList: Array.from(newEdges.values())
    };
  }

  /**
   * Sync 3D changes back to CanvasL file
   */
  async sync3DToCanvasL(canvas3D: Canvas3D, filename: string): Promise<void> {
    const graph = this.convert3DToGraph(canvas3D);
    const jsonlContent = jsonlCanvasService.exportToJSONL(graph);
    
    const lines = jsonlContent.split('\n').filter(l => l.trim());
    const entries = lines.map(line => JSON.parse(line));
    
    await databaseService.writeJSONL(filename, entries);
  }

  // Helper methods

  private getColorForType(type: string): string {
    const colorMap: Record<string, string> = {
      'text': '#6366f1',
      'file': '#8b5cf6',
      'node': '#ec4899',
      'automaton': '#f43f5e',
      'shacl': '#f97316',
      'rfc2119': '#eab308',
      'asp': '#22c55e',
      'prolog': '#06b6d4',
      'datalog': '#14b8a6'
    };
    return colorMap[type] || '#9ca3af';
  }

  private getColorForEdgeType(type: string): string {
    const colorMap: Record<string, string> = {
      'vertical': '#6366f1',
      'horizontal': '#8b5cf6',
      'transition': '#ec4899',
      'self-ref': '#f43f5e'
    };
    return colorMap[type] || '#9ca3af';
  }

  private getRadiusForType(type: string): number {
    const radiusMap: Record<string, number> = {
      'text': 0.5,
      'file': 0.6,
      'node': 0.7,
      'automaton': 0.8,
      'shacl': 0.6,
      'rfc2119': 0.6,
      'asp': 0.6,
      'prolog': 0.7,
      'datalog': 0.7
    };
    return radiusMap[type] || 0.5;
  }

  private getDimensionFromType(type: string): number {
    // Extract dimension from type or metadata
    if (type.includes('0D') || type.includes('topology')) return 0;
    if (type.includes('1D') || type.includes('temporal')) return 1;
    if (type.includes('2D') || type.includes('structural')) return 2;
    if (type.includes('3D') || type.includes('algebraic')) return 3;
    if (type.includes('4D') || type.includes('network')) return 4;
    if (type.includes('5D') || type.includes('consensus')) return 5;
    if (type.includes('6D') || type.includes('intelligence')) return 6;
    if (type.includes('7D') || type.includes('quantum')) return 7;
    return 0;
  }

  private extractChurchEncoding(node: JSONLNode): string | undefined {
    // Try to extract Church encoding from node metadata
    if (node.metadata?.churchEncoding) return node.metadata.churchEncoding;
    if (node.text?.includes('λ')) {
      const match = node.text.match(/λ[^λ]*/);
      return match ? match[0] : undefined;
    }
    return undefined;
  }

  /**
   * Load bipartite CanvasL files (topology and system partitions)
   */
  async loadBipartiteCanvasL(topologyFile: string, systemFile: string): Promise<BipartiteCanvas3D> {
    const topology3D = await this.loadCanvasLTo3D(topologyFile);
    const system3D = await this.loadCanvasLTo3D(systemFile);
    
    // Extract horizontal and vertical edges
    const horizontalEdges: Edge3D[] = [];
    const verticalEdges: Edge3D[] = [];
    
    // Combine edges from both partitions
    for (const edge of [...topology3D.edgeList, ...system3D.edgeList]) {
      if (edge.type === 'horizontal') {
        horizontalEdges.push(edge);
      } else if (edge.type === 'vertical') {
        verticalEdges.push(edge);
      }
    }
    
    return {
      topology: topology3D,
      system: system3D,
      horizontalEdges,
      verticalEdges
    };
  }

  /**
   * Render bipartite partition (topology or system)
   */
  renderBipartitePartition(canvas3D: Canvas3D, partition: 'topology' | 'system'): Canvas3D {
    const filteredNodes = new Map<string, Node3D>();
    const filteredEdges = new Map<string, Edge3D>();
    
    // Filter nodes by partition
    for (const node of canvas3D.nodeList) {
      const nodePartition = node.metadata?.frontmatter?.bipartite?.partition ||
                           (node.metadata?.type === 'topology' ? 'topology' : 'system');
      
      if (nodePartition === partition) {
        filteredNodes.set(node.id, node);
      }
    }
    
    // Filter edges connecting nodes in this partition
    for (const edge of canvas3D.edgeList) {
      const fromNode = filteredNodes.get(edge.from);
      const toNode = filteredNodes.get(edge.to);
      
      if (fromNode && toNode) {
        filteredEdges.set(edge.id, edge);
      }
    }
    
    // Recalculate bounds
    const positions = Array.from(filteredNodes.values()).map(n => n.position);
    const minX = Math.min(...positions.map(p => p[0]), 0);
    const minY = Math.min(...positions.map(p => p[1]), 0);
    const minZ = Math.min(...positions.map(p => p[2]), 0);
    const maxX = Math.max(...positions.map(p => p[0]), 0);
    const maxY = Math.max(...positions.map(p => p[1]), 0);
    const maxZ = Math.max(...positions.map(p => p[2]), 0);
    
    return {
      nodes: filteredNodes,
      edges: filteredEdges,
      nodeList: Array.from(filteredNodes.values()),
      edgeList: Array.from(filteredEdges.values()),
      bounds: {
        min: [minX, minY, minZ],
        max: [maxX, maxY, maxZ],
        center: [(minX + maxX) / 2, (minY + maxY) / 2, (minZ + maxZ) / 2]
      }
    };
  }

  /**
   * Extract bipartite structure from unified Canvas3D
   */
  extractBipartiteStructure(canvas3D: Canvas3D): BipartiteCanvas3D {
    const topologyNodes = new Map<string, Node3D>();
    const systemNodes = new Map<string, Node3D>();
    const horizontalEdges: Edge3D[] = [];
    const verticalEdges: Edge3D[] = [];
    
    // Separate nodes by partition
    for (const node of canvas3D.nodeList) {
      const partition = node.metadata?.frontmatter?.bipartite?.partition ||
                       (node.metadata?.type === 'topology' ? 'topology' : 'system');
      
      if (partition === 'topology') {
        topologyNodes.set(node.id, node);
      } else {
        systemNodes.set(node.id, node);
      }
    }
    
    // Extract edges
    for (const edge of canvas3D.edgeList) {
      if (edge.type === 'horizontal') {
        horizontalEdges.push(edge);
      } else if (edge.type === 'vertical') {
        verticalEdges.push(edge);
      }
    }
    
    // Create topology and system Canvas3D structures
    const createCanvas3D = (nodes: Map<string, Node3D>, edges: Edge3D[]): Canvas3D => {
      const filteredEdges = new Map<string, Edge3D>();
      for (const edge of edges) {
        if (nodes.has(edge.from) && nodes.has(edge.to)) {
          filteredEdges.set(edge.id, edge);
        }
      }
      
      const positions = Array.from(nodes.values()).map(n => n.position);
      const minX = Math.min(...positions.map(p => p[0]), 0);
      const minY = Math.min(...positions.map(p => p[1]), 0);
      const minZ = Math.min(...positions.map(p => p[2]), 0);
      const maxX = Math.max(...positions.map(p => p[0]), 0);
      const maxY = Math.max(...positions.map(p => p[1]), 0);
      const maxZ = Math.max(...positions.map(p => p[2]), 0);
      
      return {
        nodes,
        edges: filteredEdges,
        nodeList: Array.from(nodes.values()),
        edgeList: Array.from(filteredEdges.values()),
        bounds: {
          min: [minX, minY, minZ],
          max: [maxX, maxY, maxZ],
          center: [(minX + maxX) / 2, (minY + maxY) / 2, (minZ + maxZ) / 2]
        }
      };
    };
    
    return {
      topology: createCanvas3D(topologyNodes, verticalEdges.filter(e => 
        topologyNodes.has(e.from) && topologyNodes.has(e.to)
      )),
      system: createCanvas3D(systemNodes, verticalEdges.filter(e => 
        systemNodes.has(e.from) && systemNodes.has(e.to)
      )),
      horizontalEdges,
      verticalEdges
    };
  }
}

export const canvasl3DService: CanvasL3DService = new CanvasL3DServiceImpl();
