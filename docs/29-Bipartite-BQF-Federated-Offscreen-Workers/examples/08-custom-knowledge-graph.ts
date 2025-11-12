/**
 * Example 8: Creating Custom Knowledge Graph Extractor
 * 
 * This example demonstrates how to create a custom knowledge graph
 * extractor with additional metadata and relationships.
 */

import { KnowledgeGraphCardService, KnowledgeGraphCard, CanvasLNode } from '@/services/knowledge-graph-card-service';
import { ProvenanceNode, Slide } from '@/services/provenance-slide-service';

class CustomKnowledgeGraphCardService extends KnowledgeGraphCardService {
  /**
   * Extract thought process with additional relationships
   */
  extractThoughtProcess(agentNode: ProvenanceNode): CanvasLNode[] {
    // Get base nodes
    const baseNodes = super.extractThoughtProcess(agentNode);

    // Add relationship nodes
    if (agentNode.data?.relationships) {
      agentNode.data.relationships.forEach((rel: any, index: number) => {
        baseNodes.push({
          id: `${agentNode.id}-relationship-${index}`,
          type: 'relationship',
          text: `Related: ${rel.target}`,
          x: (baseNodes.length + index) * 200,
          y: 0,
          metadata: {
            relationshipType: rel.type,
            target: rel.target
          }
        });
      });
    }

    // Add custom metadata nodes
    if (agentNode.metadata.timestamp) {
      baseNodes.push({
        id: `${agentNode.id}-timestamp`,
        type: 'metadata',
        text: `Time: ${new Date(agentNode.metadata.timestamp).toLocaleString()}`,
        x: baseNodes.length * 200,
        y: 0
      });
    }

    return baseNodes;
  }

  /**
   * Build knowledge graph with custom layout
   */
  buildKnowledgeGraph(slide: Slide, agentId: string): KnowledgeGraphCard {
    // Get base graph
    const baseGraph = super.buildKnowledgeGraph(slide, agentId);

    // Apply custom layout (force-directed)
    const layout = this.applyCustomLayout(baseGraph);

    return {
      ...baseGraph,
      nodes: layout.nodes,
      edges: layout.edges,
      metadata: {
        ...baseGraph.metadata,
        layout: 'force-directed',
        layoutVersion: '1.0'
      }
    };
  }

  /**
   * Apply custom force-directed layout
   */
  private applyCustomLayout(card: KnowledgeGraphCard): KnowledgeGraphCard {
    // Simplified force-directed layout
    const nodes = [...card.nodes];
    const edges = [...card.edges];

    // Initialize positions if not set
    nodes.forEach((node, index) => {
      if (node.x === undefined || node.y === undefined) {
        node.x = (index % 5) * 150;
        node.y = Math.floor(index / 5) * 150;
      }
    });

    // Apply force-directed algorithm (simplified)
    for (let iteration = 0; iteration < 50; iteration++) {
      nodes.forEach(node => {
        let fx = 0;
        let fy = 0;

        // Repulsion from other nodes
        nodes.forEach(other => {
          if (node.id !== other.id) {
            const dx = (node.x || 0) - (other.x || 0);
            const dy = (node.y || 0) - (other.y || 0);
            const distance = Math.sqrt(dx * dx + dy * dy) || 1;
            const force = 1000 / (distance * distance);
            fx += (dx / distance) * force;
            fy += (dy / distance) * force;
          }
        });

        // Attraction along edges
        edges.forEach(edge => {
          if (edge.from === node.id) {
            const toNode = nodes.find(n => n.id === edge.to);
            if (toNode) {
              const dx = (toNode.x || 0) - (node.x || 0);
              const dy = (toNode.y || 0) - (node.y || 0);
              const distance = Math.sqrt(dx * dx + dy * dy) || 1;
              const force = distance * 0.01;
              fx += (dx / distance) * force;
              fy += (dy / distance) * force;
            }
          }
        });

        // Update position
        node.x = (node.x || 0) + fx * 0.1;
        node.y = (node.y || 0) + fy * 0.1;
      });
    }

    return { ...card, nodes, edges };
  }
}

// Usage example
async function useCustomKnowledgeGraphService() {
  const customService = new CustomKnowledgeGraphCardService();

  // This would typically use a real slide
  // For demonstration, we'll create a minimal example
  const slide: Slide = {
    id: 'test-slide',
    type: 'slide',
    title: 'Test Slide',
    dimension: '0D',
    description: 'Test',
    content: '',
    provenanceChain: {
      nodes: [
        {
          id: 'node-1',
          type: 'agent',
          position: [0, 0, 0],
          metadata: {
            agentId: '6D-Intelligence-Agent',
            dimension: '0D',
            pattern: 'identity',
            timestamp: Date.now()
          },
          data: {
            relationships: [
              { type: 'depends', target: 'node-2' }
            ]
          }
        }
      ],
      edges: []
    },
    cards: []
  };

  const graph = customService.buildKnowledgeGraph(slide, '6D-Intelligence-Agent');
  console.log('Custom knowledge graph created:', graph.id);
  console.log('Nodes:', graph.nodes.length);
  console.log('Layout:', graph.metadata.layout);

  // Render as SVG
  const svg = customService.renderKnowledgeGraph(graph);
  console.log('SVG rendered');

  return { graph, svg };
}

// Run the example
useCustomKnowledgeGraphService()
  .then(({ graph, svg }) => {
    console.log('Custom knowledge graph example completed');
  })
  .catch(error => {
    console.error('Custom knowledge graph example failed:', error);
  });

