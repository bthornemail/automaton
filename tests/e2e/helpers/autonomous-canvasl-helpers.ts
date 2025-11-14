import { readFileSync } from 'fs';
import { join } from 'path';

/**
 * Test helpers for Autonomous CanvasL E2E tests
 */

export interface CanvasLDirectives {
  version?: string;
  schema?: string;
  [key: string]: any;
}

export interface CanvasLEntry {
  id?: string;
  type?: string;
  [key: string]: any;
}

export interface ParsedCanvasL {
  directives: CanvasLDirectives;
  entries: CanvasLEntry[];
}

/**
 * Parse a CanvasL file and extract directives and entries
 */
export function parseCanvasL(filePath: string): ParsedCanvasL | null {
  try {
    const content = readFileSync(filePath, 'utf-8');
    const lines = content.split('\n').filter(line => line.trim());
    
    const directives: CanvasLDirectives = {};
    const entries: CanvasLEntry[] = [];
    
    for (const line of lines) {
      const trimmed = line.trim();
      if (!trimmed) continue;
      
      if (trimmed.startsWith('@')) {
        const match = trimmed.match(/^@(\w+)\s+(.+)$/);
        if (match) {
          directives[match[1]] = match[2];
        }
      } else {
        try {
          const entry = JSON.parse(trimmed);
          entries.push(entry);
        } catch (e) {
          // Skip invalid JSON
        }
      }
    }
    
    return { directives, entries };
  } catch (error) {
    return null;
  }
}

/**
 * Find a CanvasL file in multiple possible locations
 */
export function findCanvasLFile(filename: string): string | null {
  const possiblePaths = [
    join(__dirname, '../../../ui/public/jsonl', filename),
    join(__dirname, '../../../evolutions', filename),
    join(__dirname, '../..', filename)
  ];
  
  for (const filePath of possiblePaths) {
    try {
      readFileSync(filePath, 'utf-8');
      return filePath;
    } catch {
      // Continue to next path
    }
  }
  
  return null;
}

/**
 * Load all autonomous CanvasL files
 */
export function loadAutonomousFiles(): Map<string, ParsedCanvasL> {
  const files = [
    'metaverse.shape.canvasl',
    'metaverse.centroid.canvasl',
    'automaton.kernel.seed.canvasl',
    'autonomous.basis.canvasl',
    'unified.automaton.canvasl'
  ];
  
  const fileData = new Map<string, ParsedCanvasL>();
  
  for (const file of files) {
    const filePath = findCanvasLFile(file);
    if (filePath) {
      const parsed = parseCanvasL(filePath);
      if (parsed) {
        fileData.set(file, parsed);
      }
    }
  }
  
  return fileData;
}

/**
 * Validate CanvasL structure
 */
export function validateCanvasLStructure(parsed: ParsedCanvasL): {
  valid: boolean;
  errors: string[];
} {
  const errors: string[] = [];
  
  // Check directives
  if (!parsed.directives.version) {
    errors.push('Missing @version directive');
  }
  if (!parsed.directives.schema) {
    errors.push('Missing @schema directive');
  }
  
  // Check entries
  if (parsed.entries.length === 0) {
    errors.push('No entries found');
  }
  
  // Check entry structure
  for (const entry of parsed.entries) {
    if (!entry.id) {
      errors.push('Entry missing id field');
    }
    if (!entry.type) {
      errors.push(`Entry ${entry.id || 'unknown'} missing type field`);
    }
  }
  
  return {
    valid: errors.length === 0,
    errors
  };
}

/**
 * Check if entry has bipartite metadata
 */
export function hasBipartiteMetadata(entry: CanvasLEntry): boolean {
  return !!entry.bipartite;
}

/**
 * Check if entry has BQF encoding
 */
export function hasBQFEncoding(entry: CanvasLEntry): boolean {
  return !!(entry.bipartite && entry.bipartite.bqf);
}

/**
 * Get entries by partition
 */
export function getEntriesByPartition(
  entries: CanvasLEntry[],
  partition: 'topology' | 'system'
): CanvasLEntry[] {
  return entries.filter(e => 
    e.bipartite && e.bipartite.partition === partition
  );
}

/**
 * Get entries by dimension
 */
export function getEntriesByDimension(
  entries: CanvasLEntry[],
  dimension: string
): CanvasLEntry[] {
  return entries.filter(e => e.dimension === dimension);
}

/**
 * Get entries with regeneration metadata
 */
export function getEntriesWithRegeneration(entries: CanvasLEntry[]): CanvasLEntry[] {
  return entries.filter(e => 
    e.metadata && e.metadata.regenerate
  );
}

/**
 * Get all dimensions present in entries
 */
export function getDimensions(entries: CanvasLEntry[]): Set<string> {
  const dimensions = new Set<string>();
  entries.forEach(e => {
    if (e.dimension) {
      dimensions.add(e.dimension);
    }
  });
  return dimensions;
}

/**
 * Get vertical edges (dimensional progression)
 */
export function getVerticalEdges(entries: CanvasLEntry[]): CanvasLEntry[] {
  return entries.filter(e => 
    e.type === 'edge' && 
    e.bipartite && 
    e.bipartite.type === 'vertical'
  );
}

/**
 * Get horizontal edges (topology↔system)
 */
export function getHorizontalEdges(entries: CanvasLEntry[]): CanvasLEntry[] {
  return entries.filter(e => 
    e.type === 'edge' && 
    e.bipartite && 
    e.bipartite.type === 'horizontal'
  );
}

