/**
 * Knowledge Graph Card Service
 * 
 * Manages 2D CanvasL knowledge graph cards for agent thought processes.
 */

import { Slide, ProvenanceNode } from './provenance-slide-service';

export interface CanvasLNode {
  id: string;
  type?: string;
  text?: string;
  x?: number;
  y?: number;
  [key: string]: any;
}

export interface CanvasLEdge {
  id: string;
  type: 'vertical' | 'horizontal' | 'transition' | 'self-ref' | 'r5rs-call';
  from: string;
  to: string;
  [key: string]: any;
}

export interface KnowledgeGraphCard {
  id: string;
  agentId: string;
  slideId: string;
  nodes: CanvasLNode[];
  edges: CanvasLEdge[];
  metadata: {
    dimension?: string;
    pattern?: string;
    timestamp: number;
    thoughtProcess: string;
  };
}

export class KnowledgeGraphCardService {
  /**
   * Extract thought process from agent node.
   * 
   * Extracts structured thought process information from a provenance node's
   * metadata and converts it into CanvasL node format. The thought process
   * includes Church encoding, pattern, dimension, and agent ID information,
   * each represented as a separate node positioned horizontally.
   * 
   * @param {ProvenanceNode} agentNode - Provenance node with agent metadata
   * @returns {CanvasLNode[]} Array of CanvasL nodes representing the thought process
   * 
   * @example
   * ```typescript
   * const thoughtNodes = knowledgeGraphCardService.extractThoughtProcess(agentNode);
   * // thoughtNodes contains nodes for Church encoding, pattern, dimension, agent ID
   * ```
   */
  extractThoughtProcess(agentNode: ProvenanceNode): CanvasLNode[] {
    const nodes: CanvasLNode[] = [];
    let xOffset = 0;
    const yOffset = 0;
    const spacing = 200;
    
    // Extract Church encoding
    if (agentNode.metadata.churchEncoding) {
      nodes.push({
        id: `${agentNode.id}-church-encoding`,
        type: 'node',
        text: `Church: ${agentNode.metadata.churchEncoding}`,
        x: xOffset,
        y: yOffset
      });
      xOffset += spacing;
    }
    
    // Extract pattern
    if (agentNode.metadata.pattern) {
      nodes.push({
        id: `${agentNode.id}-pattern`,
        type: 'node',
        text: `Pattern: ${agentNode.metadata.pattern}`,
        x: xOffset,
        y: yOffset
      });
      xOffset += spacing;
    }
    
    // Extract dimension
    if (agentNode.metadata.dimension) {
      nodes.push({
        id: `${agentNode.id}-dimension`,
        type: 'node',
        text: `Dimension: ${agentNode.metadata.dimension}`,
        x: xOffset,
        y: yOffset
      });
      xOffset += spacing;
    }
    
    // Extract agent ID
    if (agentNode.metadata.agentId) {
      nodes.push({
        id: `${agentNode.id}-agent`,
        type: 'node',
        text: `Agent: ${agentNode.metadata.agentId}`,
        x: xOffset,
        y: yOffset
      });
    }
    
    return nodes;
  }

  /**
   * Build knowledge graph from slide and agent ID.
   * 
   * Creates a knowledge graph card representing an agent's thought process within
   * a slide. The graph includes:
   * - Agent nodes from the slide's provenance chain
   * - Thought process nodes extracted from agent metadata
   * - Edges for dimensional progression (vertical)
   * - Edges for pattern relationships (horizontal)
   * 
   * Nodes are arranged in a grid layout, with thought process nodes offset to
   * the right for visual separation.
   * 
   * @param {Slide} slide - Slide containing provenance chain
   * @param {string} agentId - ID of the agent to build graph for
   * @returns {KnowledgeGraphCard} Knowledge graph card with nodes and edges
   * 
   * @example
   * ```typescript
   * const card = knowledgeGraphCardService.buildKnowledgeGraph(slide, '6D-Intelligence-Agent');
   * // card contains nodes and edges representing the agent's thought process
   * ```
   */
  buildKnowledgeGraph(slide: Slide, agentId: string): KnowledgeGraphCard {
    const agentNodes = slide.provenanceChain?.nodes.filter(
      n => n.metadata.agentId === agentId
    ) || [];
    
    const canvaslNodes: CanvasLNode[] = [];
    const canvaslEdges: CanvasLEdge[] = [];
    
    // Convert provenance nodes to CanvasL nodes
    agentNodes.forEach((node, index) => {
      const row = Math.floor(index / 3);
      const col = index % 3;
      
      canvaslNodes.push({
        id: node.id,
        type: 'node',
        text: node.metadata.agentId || node.id,
        x: col * 200,
        y: row * 150
      });
      
      // Create edges for dimensional progression
      if (index > 0) {
        const prevNode = agentNodes[index - 1];
        const fromDim = parseInt(prevNode.metadata.dimension?.replace('D', '') || '0');
        const toDim = parseInt(node.metadata.dimension?.replace('D', '') || '0');
        
        if (toDim === fromDim + 1 || (fromDim === 7 && toDim === 0)) {
          canvaslEdges.push({
            id: `edge-${prevNode.id}-${node.id}`,
            type: 'vertical',
            from: prevNode.id,
            to: node.id
          });
        }
      }
      
      // Create edges for patterns
      if (node.metadata.pattern) {
        const patternNode = canvaslNodes.find(n => 
          n.id === `${node.id}-pattern` || 
          (n.text && n.text.includes(`Pattern: ${node.metadata.pattern}`))
        );
        
        if (patternNode) {
          canvaslEdges.push({
            id: `edge-${node.id}-pattern`,
            type: 'horizontal',
            from: node.id,
            to: patternNode.id
          });
        }
      }
    });
    
    // Extract thought process nodes
    agentNodes.forEach(node => {
      const thoughtNodes = this.extractThoughtProcess(node);
      thoughtNodes.forEach(thoughtNode => {
        // Offset thought nodes to the right
        thoughtNode.x = (thoughtNode.x || 0) + 600;
        canvaslNodes.push(thoughtNode);
        
        // Connect to main node
        canvaslEdges.push({
          id: `edge-${node.id}-${thoughtNode.id}`,
          type: 'horizontal',
          from: node.id,
          to: thoughtNode.id
        });
      });
    });
    
    return {
      id: `kg-${agentId}-${slide.id}`,
      agentId,
      slideId: slide.id,
      nodes: canvaslNodes,
      edges: canvaslEdges,
      metadata: {
        dimension: slide.dimension,
        timestamp: Date.now(),
        thoughtProcess: `Thought process for ${agentId} in ${slide.dimension || 'unknown'} dimension`
      }
    };
  }

  /**
   * Render knowledge graph as SVG.
   * 
   * Converts a knowledge graph card into an SVG visualization. The SVG includes:
   * - Dark background (#1f2937)
   * - Edges rendered as lines (behind nodes)
   * - Nodes rendered as circles with text labels
   * - Color coding: blue for nodes, green for edges
   * 
   * The SVG is sized at 800x600 with a viewBox for scaling. This SVG can be
   * displayed in 2D panels or converted to textures for 3D rendering.
   * 
   * @param {KnowledgeGraphCard} card - Knowledge graph card to render
   * @returns {SVGElement} SVG element containing the rendered graph
   * 
   * @example
   * ```typescript
   * const svg = knowledgeGraphCardService.renderKnowledgeGraph(card);
   * document.body.appendChild(svg);
   * // SVG is displayed in the DOM
   * ```
   */
  renderKnowledgeGraph(card: KnowledgeGraphCard): SVGElement {
    const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
    svg.setAttribute('width', '800');
    svg.setAttribute('height', '600');
    svg.setAttribute('viewBox', '0 0 800 600');
    svg.setAttribute('xmlns', 'http://www.w3.org/2000/svg');
    
    // Background
    const bg = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
    bg.setAttribute('width', '800');
    bg.setAttribute('height', '600');
    bg.setAttribute('fill', '#1f2937');
    svg.appendChild(bg);
    
    // Render edges first (so they appear behind nodes)
    card.edges.forEach(edge => {
      const fromNode = card.nodes.find(n => n.id === edge.from);
      const toNode = card.nodes.find(n => n.id === edge.to);
      
      if (fromNode && toNode) {
        const line = document.createElementNS('http://www.w3.org/2000/svg', 'line');
        line.setAttribute('x1', String(fromNode.x || 0));
        line.setAttribute('y1', String(fromNode.y || 0));
        line.setAttribute('x2', String(toNode.x || 0));
        line.setAttribute('y2', String(toNode.y || 0));
        line.setAttribute('stroke', edge.type === 'vertical' ? '#3b82f6' : '#10b981');
        line.setAttribute('stroke-width', '2');
        line.setAttribute('opacity', '0.6');
        svg.appendChild(line);
      }
    });
    
    // Render nodes
    card.nodes.forEach(node => {
      const rect = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
      rect.setAttribute('x', String((node.x || 0) - 50));
      rect.setAttribute('y', String((node.y || 0) - 25));
      rect.setAttribute('width', '100');
      rect.setAttribute('height', '50');
      rect.setAttribute('fill', '#3b82f6');
      rect.setAttribute('rx', '5');
      rect.setAttribute('opacity', '0.9');
      svg.appendChild(rect);
      
      const text = document.createElementNS('http://www.w3.org/2000/svg', 'text');
      text.setAttribute('x', String(node.x || 0));
      text.setAttribute('y', String(node.y || 0));
      text.setAttribute('text-anchor', 'middle');
      text.setAttribute('fill', 'white');
      text.setAttribute('font-size', '12');
      text.setAttribute('font-family', 'Arial');
      text.textContent = node.text || node.id;
      svg.appendChild(text);
    });
    
    return svg;
  }

  /**
   * Build knowledge graphs for all agents in a slide.
   * 
   * Creates knowledge graph cards for all unique agents found in a slide's
   * provenance chain. This is useful for displaying multiple agent thought
   * processes simultaneously.
   * 
   * @param {Slide} slide - Slide containing provenance chain
   * @returns {KnowledgeGraphCard[]} Array of knowledge graph cards, one per agent
   * 
   * @example
   * ```typescript
   * const cards = knowledgeGraphCardService.buildKnowledgeGraphsForSlide(slide);
   * cards.forEach(card => {
   *   const svg = knowledgeGraphCardService.renderKnowledgeGraph(card);
   *   // Display SVG for each agent
   * });
   * ```
   */
  buildKnowledgeGraphsForSlide(slide: Slide): KnowledgeGraphCard[] {
    const agentIds = new Set<string>();
    
    slide.provenanceChain?.nodes.forEach(node => {
      if (node.metadata.agentId) {
        agentIds.add(node.metadata.agentId);
      }
    });
    
    return Array.from(agentIds).map(agentId => 
      this.buildKnowledgeGraph(slide, agentId)
    );
  }
}

export const knowledgeGraphCardService = new KnowledgeGraphCardService();

