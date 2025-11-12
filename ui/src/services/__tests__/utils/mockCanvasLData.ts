/**
 * Mock CanvasL Data Generator
 * Generates mock CanvasL entries for testing
 */

export interface MockCanvasLEntry {
  id: string;
  type: string;
  dimension?: string;
  selfReference?: {
    file: string;
    line: number;
    pattern: string;
    timestamp?: number;
  };
  metadata?: {
    selfReference?: {
      file: string;
      line: number;
      pattern: string;
      timestamp?: number;
    };
    timestamp?: number;
    agentId?: string;
    dimension?: string;
    churchEncoding?: string;
  };
  provenanceHistory?: any[];
  frontmatter?: {
    bipartite?: {
      partition: 'topology' | 'system';
      dimension: string;
      bqf?: {
        coefficients: number[];
        form: string;
        signature: string;
      };
    };
  };
  [key: string]: any;
}

/**
 * Generate mock CanvasL entries
 */
export function generateMockCanvasLEntries(
  count: number = 5,
  options: {
    dimension?: string;
    type?: string;
    partition?: 'topology' | 'system';
    includeSelfReference?: boolean;
    includeProvenanceHistory?: boolean;
    includeBipartite?: boolean;
  } = {}
): MockCanvasLEntry[] {
  const entries: MockCanvasLEntry[] = [];
  const dimension = options.dimension || '0D';
  const type = options.type || 'automaton';
  const partition = options.partition;
  const file = `evolution/${dimension.toLowerCase()}.jsonl`;

  for (let i = 0; i < count; i++) {
    const entry: MockCanvasLEntry = {
      id: `${type}-${dimension}-${i}`,
      type,
      dimension
    };

    if (options.includeSelfReference !== false) {
      entry.selfReference = {
        file,
        line: i + 1,
        pattern: getPatternForDimension(dimension),
        timestamp: Date.now() + i * 1000
      };
    }

    if (options.includeProvenanceHistory) {
      entry.provenanceHistory = [
        {
          file,
          line: i,
          pattern: getPatternForDimension(dimension),
          timestamp: Date.now() + (i - 1) * 1000
        }
      ];
    }

    if (options.includeBipartite && partition) {
      entry.frontmatter = {
        bipartite: {
          partition,
          dimension,
          bqf: {
            coefficients: getBQFCoefficients(dimension, partition),
            form: getBQFForm(dimension, partition),
            signature: getSignature(dimension)
          }
        }
      };
    }

    entries.push(entry);
  }

  return entries;
}

/**
 * Generate mock CanvasL JSONL string
 */
export function generateMockCanvasLJSONL(
  entries: MockCanvasLEntry[],
  includeDirectives: boolean = true
): string {
  const lines: string[] = [];

  if (includeDirectives) {
    lines.push('@version 1.0.0');
    lines.push('@schema automaton');
    lines.push('');
  }

  for (const entry of entries) {
    lines.push(JSON.stringify(entry));
  }

  return lines.join('\n');
}

/**
 * Generate bipartite CanvasL data (topology and system)
 */
export function generateBipartiteCanvasLData(
  dimension: string = '0D',
  topologyCount: number = 3,
  systemCount: number = 3
): {
  topology: MockCanvasLEntry[];
  system: MockCanvasLEntry[];
  topologyJSONL: string;
  systemJSONL: string;
} {
  const topology = generateMockCanvasLEntries(topologyCount, {
    dimension,
    type: 'topology',
    partition: 'topology',
    includeBipartite: true
  });

  const system = generateMockCanvasLEntries(systemCount, {
    dimension,
    type: 'system',
    partition: 'system',
    includeBipartite: true
  });

  return {
    topology,
    system,
    topologyJSONL: generateMockCanvasLJSONL(topology),
    systemJSONL: generateMockCanvasLJSONL(system)
  };
}

/**
 * Get BQF coefficients for dimension and partition
 */
function getBQFCoefficients(
  dimension: string,
  partition: 'topology' | 'system'
): number[] {
  const dimNum = parseInt(dimension.replace('D', '')) || 0;

  if (partition === 'topology') {
    return [1, 0, dimNum];
  } else {
    return [dimNum, 1, 1];
  }
}

/**
 * Get BQF form string
 */
function getBQFForm(dimension: string, partition: 'topology' | 'system'): string {
  const coeffs = getBQFCoefficients(dimension, partition);
  return `${coeffs[0]}x² + ${coeffs[1]}xy + ${coeffs[2]}y²`;
}

/**
 * Get signature for dimension
 */
function getSignature(dimension: string): string {
  const dimNum = parseInt(dimension.replace('D', '')) || 0;

  if (dimNum === 0) return 'identity';
  if (dimNum === 1) return 'successor';
  if (dimNum === 2) return 'pairing';
  if (dimNum === 3 || dimNum === 4) return 'lorentz';
  if (dimNum === 5) return 'consensus';
  if (dimNum === 6) return 'intelligence';
  if (dimNum === 7) return 'quantum';

  return 'euclidean';
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

