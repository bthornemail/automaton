/**
 * Mock Evolution Data Generator
 * Generates mock evolution file data for testing
 */

export interface MockEvolutionEntry {
  id: string;
  type?: string;
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
  text?: string;
  churchEncoding?: string;
  file?: string;
  rawLine?: string;
  rawEntry?: any;
  provenanceHistory?: any[];
}

/**
 * Generate mock evolution entries with self-reference patterns
 */
export function generateMockEvolutionEntries(
  count: number = 5,
  options: {
    dimension?: string;
    pattern?: string;
    file?: string;
    includeProvenance?: boolean;
  } = {}
): MockEvolutionEntry[] {
  const entries: MockEvolutionEntry[] = [];
  const dimension = options.dimension || '0D';
  const pattern = options.pattern || 'identity';
  const file = options.file || 'evolution/test.jsonl';

  for (let i = 0; i < count; i++) {
    const line = i + 1;
    const entry: MockEvolutionEntry = {
      id: `entry-${dimension}-${i}`,
      type: 'automaton',
      selfReference: {
        file,
        line,
        pattern: `${pattern}-${i}`,
        timestamp: Date.now() + i * 1000
      },
      metadata: {
        timestamp: Date.now() + i * 1000,
        agentId: `${dimension}-Agent`,
        dimension,
        churchEncoding: getChurchEncodingForDimension(dimension)
      },
      text: `Evolution entry ${i} for ${dimension}`,
      file,
      rawLine: JSON.stringify({
        id: `entry-${dimension}-${i}`,
        type: 'automaton',
        selfReference: {
          file,
          line,
          pattern: `${pattern}-${i}`
        }
      }),
      rawEntry: {
        id: `entry-${dimension}-${i}`,
        type: 'automaton',
        selfReference: {
          file,
          line,
          pattern: `${pattern}-${i}`
        }
      }
    };

    if (options.includeProvenance) {
      entry.provenanceHistory = [
        {
          file,
          line: line - 1 || 1,
          pattern: `${pattern}-${i - 1}`,
          timestamp: Date.now() + (i - 1) * 1000
        }
      ];
    }

    entries.push(entry);
  }

  return entries;
}

/**
 * Generate mock evolution data for multiple dimensions
 */
export function generateMultiDimensionEvolution(
  dimensions: string[] = ['0D', '1D', '2D', '3D']
): MockEvolutionEntry[] {
  const entries: MockEvolutionEntry[] = [];

  for (const dimension of dimensions) {
    const dimEntries = generateMockEvolutionEntries(3, {
      dimension,
      pattern: getPatternForDimension(dimension),
      file: `evolution/${dimension.toLowerCase()}.jsonl`,
      includeProvenance: true
    });
    entries.push(...dimEntries);
  }

  return entries;
}

/**
 * Generate mock evolution file content as JSONL string
 */
export function generateMockEvolutionJSONL(
  entries: MockEvolutionEntry[]
): string {
  return entries.map(entry => JSON.stringify(entry)).join('\n');
}

/**
 * Generate mock database query results for evolution files
 */
export function generateMockDatabaseResults(
  entries: MockEvolutionEntry[]
): any[] {
  const fileMap = new Map<string, MockEvolutionEntry[]>();

  for (const entry of entries) {
    const file = entry.file || entry.selfReference?.file || 'unknown.jsonl';
    if (!fileMap.has(file)) {
      fileMap.set(file, []);
    }
    fileMap.get(file)!.push(entry);
  }

  return Array.from(fileMap.entries()).map(([file, fileEntries]) => ({
    file,
    content: generateMockEvolutionJSONL(fileEntries)
  }));
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

