/**
 * Mock Provenance Chain Generator
 * Generates mock provenance chains for testing
 */

import { ProvenanceNode, ProvenanceEdge, ProvenanceChain } from '../../provenance-slide-service';

/**
 * Generate mock provenance nodes
 */
export function generateMockProvenanceNodes(
  count: number = 5,
  options: {
    dimension?: string;
    pattern?: string;
    file?: string;
    includeProvenanceHistory?: boolean;
  } = {}
): ProvenanceNode[] {
  const nodes: ProvenanceNode[] = [];
  const dimension = options.dimension || '0D';
  const pattern = options.pattern || 'identity';
  const file = options.file || 'evolution/test.jsonl';

  for (let i = 0; i < count; i++) {
    const node: ProvenanceNode = {
      id: `pattern-${dimension}-${i}`,
      type: 'evolution',
      position: calculatePosition(dimension, i),
      metadata: {
        timestamp: Date.now() + i * 1000,
        file,
        line: i + 1,
        agentId: `${dimension}-Agent`,
        dimension,
        churchEncoding: getChurchEncodingForDimension(dimension),
        pattern: `${pattern}-${i}`
      },
      data: {
        id: `pattern-${dimension}-${i}`,
        file,
        line: i + 1,
        pattern: `${pattern}-${i}`,
        dimension,
        timestamp: Date.now() + i * 1000,
        agentId: `${dimension}-Agent`,
        churchEncoding: getChurchEncodingForDimension(dimension),
        provenanceHistory: options.includeProvenanceHistory
          ? [
              {
                file,
                line: i,
                pattern: `${pattern}-${i - 1}`,
                timestamp: Date.now() + (i - 1) * 1000
              }
            ]
          : []
      }
    };
    nodes.push(node);
  }

  return nodes;
}

/**
 * Generate mock provenance edges
 */
export function generateMockProvenanceEdges(
  nodes: ProvenanceNode[],
  options: {
    includeDimensionalProgression?: boolean;
    includeCrossFileReferences?: boolean;
  } = {}
): ProvenanceEdge[] {
  const edges: ProvenanceEdge[] = [];
  const includeDimensional = options.includeDimensionalProgression !== false;
  const includeCrossFile = options.includeCrossFileReferences !== false;

  // Generate dimensional progression edges
  if (includeDimensional) {
    for (let i = 0; i < nodes.length - 1; i++) {
      const fromNode = nodes[i];
      const toNode = nodes[i + 1];

      const fromDim = parseInt(fromNode.metadata.dimension?.replace('D', '') || '0');
      const toDim = parseInt(toNode.metadata.dimension?.replace('D', '') || '0');

      if (toDim === fromDim + 1 || (fromDim === 7 && toDim === 0)) {
        edges.push({
          id: `edge-${fromNode.id}-${toNode.id}`,
          type: 'evolves',
          from: fromNode.id,
          to: toNode.id,
          metadata: {
            timestamp: Date.now(),
            weight: 1.0,
            context: `Dimensional progression: ${fromNode.metadata.dimension} → ${toNode.metadata.dimension}`
          }
        });
      }
    }
  }

  // Generate cross-file reference edges
  if (includeCrossFile) {
    for (let i = 0; i < nodes.length - 1; i++) {
      const fromNode = nodes[i];
      const toNode = nodes[i + 1];

      if (fromNode.metadata.file !== toNode.metadata.file) {
        edges.push({
          id: `ref-${fromNode.id}-${toNode.id}`,
          type: 'references',
          from: fromNode.id,
          to: toNode.id,
          metadata: {
            timestamp: Date.now(),
            weight: 0.5,
            context: `Cross-file reference: ${fromNode.metadata.file} → ${toNode.metadata.file}`
          }
        });
      }
    }
  }

  return edges;
}

/**
 * Generate complete mock provenance chain
 */
export function generateMockProvenanceChain(
  nodeCount: number = 5,
  options: {
    dimension?: string;
    pattern?: string;
    file?: string;
    includeProvenanceHistory?: boolean;
    includeDimensionalProgression?: boolean;
    includeCrossFileReferences?: boolean;
  } = {}
): ProvenanceChain {
  const nodes = generateMockProvenanceNodes(nodeCount, options);
  const edges = generateMockProvenanceEdges(nodes, {
    includeDimensionalProgression: options.includeDimensionalProgression,
    includeCrossFileReferences: options.includeCrossFileReferences
  });

  return { nodes, edges };
}

/**
 * Generate multi-dimensional provenance chain
 */
export function generateMultiDimensionalProvenanceChain(
  dimensions: string[] = ['0D', '1D', '2D', '3D']
): ProvenanceChain {
  const allNodes: ProvenanceNode[] = [];
  const allEdges: ProvenanceEdge[] = [];

  for (const dimension of dimensions) {
    const nodes = generateMockProvenanceNodes(2, {
      dimension,
      pattern: getPatternForDimension(dimension),
      file: `evolution/${dimension.toLowerCase()}.jsonl`,
      includeProvenanceHistory: true
    });
    allNodes.push(...nodes);

    // Add edges between dimensions
    if (allNodes.length > nodes.length) {
      const prevNode = allNodes[allNodes.length - nodes.length - 1];
      const currNode = nodes[0];
      allEdges.push({
        id: `edge-${prevNode.id}-${currNode.id}`,
        type: 'evolves',
        from: prevNode.id,
        to: currNode.id,
        metadata: {
          timestamp: Date.now(),
          weight: 1.0,
          context: `Dimensional progression: ${prevNode.metadata.dimension} → ${currNode.metadata.dimension}`
        }
      });
    }

    // Add edges within dimension
    const dimEdges = generateMockProvenanceEdges(nodes, {
      includeDimensionalProgression: true,
      includeCrossFileReferences: false
    });
    allEdges.push(...dimEdges);
  }

  return { nodes: allNodes, edges: allEdges };
}

/**
 * Calculate 3D position for node
 */
function calculatePosition(dimension: string, index: number): [number, number, number] {
  const dimNum = parseInt(dimension.replace('D', '')) || 0;
  const angle = (dimNum / 8) * Math.PI * 2;
  const radius = 5 + dimNum * 0.5;
  const x = Math.cos(angle) * radius + index * 0.5;
  const y = dimNum * 2;
  const z = Math.sin(angle) * radius + index * 0.5;
  return [x, y, z];
}

/**
 * Get Church encoding for dimension
 */
function getChurchEncodingForDimension(dimension: string): string {
  const encodings: Record<string, string> = {
    '0D': 'λf.λx.x',
    '1D': 'λn.λf.λx.f(nfx)',
    '2D': 'λx.λy.λf.fxy',
    '3D': 'λm.λn.λf.λx.mf(nfx)',
    '4D': 'λm.λn.λf.m(nf)',
    '5D': 'λm.λn.nm',
    '6D': 'λf.(λx.f(xx))(λx.f(xx))',
    '7D': 'λf.λx.f(f(f(f(f(f(f(fx)))))))'
  };
  return encodings[dimension] || 'λf.λx.x';
}

/**
 * Get pattern name for dimension
 */
function getPatternForDimension(dimension: string): string {
  const patterns: Record<string, string> = {
    '0D': 'identity',
    '1D': 'successor',
    '2D': 'pairing',
    '3D': 'algebra',
    '4D': 'network',
    '5D': 'consensus',
    '6D': 'intelligence',
    '7D': 'quantum'
  };
  return patterns[dimension] || 'unknown';
}

