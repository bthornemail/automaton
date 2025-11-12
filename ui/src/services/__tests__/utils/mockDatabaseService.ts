/**
 * Mock Database Service
 * Provides mock implementations for database service methods
 */

import { vi } from 'vitest';

export interface MockDatabaseService {
  query: ReturnType<typeof vi.fn>;
  readJSONL: ReturnType<typeof vi.fn>;
  writeJSONL: ReturnType<typeof vi.fn>;
}

/**
 * Create mock database service
 */
export function createMockDatabaseService(): MockDatabaseService {
  return {
    query: vi.fn(),
    readJSONL: vi.fn(),
    writeJSONL: vi.fn()
  };
}

/**
 * Setup default mock responses for database service
 */
export function setupMockDatabaseService(
  mockService: MockDatabaseService,
  options: {
    evolutionFiles?: any[];
    canvasLEntries?: any[];
  } = {}
): void {
  // Mock query for evolution files
  mockService.query.mockImplementation(async (query: string, type: string) => {
    if (type === 'sparql' && query.includes('evolution:EvolutionFile')) {
      return options.evolutionFiles || [];
    }
    return [];
  });

  // Mock readJSONL
  mockService.readJSONL.mockImplementation(async (filename: string) => {
    return options.canvasLEntries || [];
  });

  // Mock writeJSONL
  mockService.writeJSONL.mockImplementation(async (filename: string, entries: any[]) => {
    return true;
  });
}

