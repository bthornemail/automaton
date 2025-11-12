/**
 * Mock Automaton State Generator
 * Generates mock automaton states for testing
 */

import { AutomatonState } from '../../automaton-file-generator-service';

/**
 * Generate mock automaton state
 */
export function generateMockAutomatonState(
  options: {
    id?: string;
    dimension?: string;
    kernelCount?: number;
    topologyCount?: number;
    systemCount?: number;
    seedCount?: number;
  } = {}
): AutomatonState {
  const id = options.id || 'test-automaton';
  const dimension = options.dimension || '0D';
  const kernelCount = options.kernelCount || 3;
  const topologyCount = options.topologyCount || 2;
  const systemCount = options.systemCount || 2;
  const seedCount = options.seedCount || 1;

  return {
    id,
    dimension,
    kernel: generateKernelEntries(kernelCount, dimension),
    topology: generateTopologyEntries(topologyCount, dimension),
    system: generateSystemEntries(systemCount, dimension),
    seed: generateSeedEntries(seedCount, dimension)
  };
}

/**
 * Generate kernel entries
 */
function generateKernelEntries(count: number, dimension: string): any[] {
  const entries: any[] = [];
  for (let i = 0; i < count; i++) {
    entries.push({
      id: `kernel-${dimension}-${i}`,
      type: 'automaton',
      dimension,
      metadata: {
        timestamp: Date.now() + i * 1000,
        agentId: `${dimension}-Agent`
      },
      data: {
        churchEncoding: getChurchEncodingForDimension(dimension),
        pattern: getPatternForDimension(dimension)
      }
    });
  }
  return entries;
}

/**
 * Generate topology entries
 */
function generateTopologyEntries(count: number, dimension: string): any[] {
  const entries: any[] = [];
  for (let i = 0; i < count; i++) {
    entries.push({
      id: `topology-${dimension}-${i}`,
      type: 'topology',
      dimension,
      frontmatter: {
        bipartite: {
          partition: 'topology',
          dimension
        }
      },
      metadata: {
        timestamp: Date.now() + i * 1000
      }
    });
  }
  return entries;
}

/**
 * Generate system entries
 */
function generateSystemEntries(count: number, dimension: string): any[] {
  const entries: any[] = [];
  for (let i = 0; i < count; i++) {
    entries.push({
      id: `system-${dimension}-${i}`,
      type: 'system',
      dimension,
      frontmatter: {
        bipartite: {
          partition: 'system',
          dimension
        }
      },
      metadata: {
        timestamp: Date.now() + i * 1000
      }
    });
  }
  return entries;
}

/**
 * Generate seed entries
 */
function generateSeedEntries(count: number, dimension: string): any[] {
  const entries: any[] = [];
  for (let i = 0; i < count; i++) {
    entries.push({
      file: `evolution/${dimension.toLowerCase()}.jsonl`,
      line: i + 1,
      pattern: getPatternForDimension(dimension),
      timestamp: Date.now() + i * 1000
    });
  }
  return entries;
}

/**
 * Generate automaton states for all dimensions
 */
export function generateAllDimensionStates(): AutomatonState[] {
  const dimensions = ['0D', '1D', '2D', '3D', '4D', '5D', '6D', '7D'];
  return dimensions.map(dimension =>
    generateMockAutomatonState({
      id: `automaton-${dimension.toLowerCase()}`,
      dimension,
      kernelCount: 3,
      topologyCount: 2,
      systemCount: 2,
      seedCount: 1
    })
  );
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

