/**
 * Mock Meta-Log API Service
 * Provides mock implementations for Meta-Log API service methods
 */

import { vi } from 'vitest';

export interface MockMetaLogApiService {
  isAvailable: ReturnType<typeof vi.fn>;
  loadCanvas: ReturnType<typeof vi.fn>;
  prologQuery: ReturnType<typeof vi.fn>;
  datalogQuery: ReturnType<typeof vi.fn>;
  sparqlQuery: ReturnType<typeof vi.fn>;
}

/**
 * Create mock Meta-Log API service
 */
export function createMockMetaLogApiService(): MockMetaLogApiService {
  return {
    isAvailable: vi.fn(() => true),
    loadCanvas: vi.fn().mockResolvedValue(undefined),
    prologQuery: vi.fn().mockResolvedValue({ bindings: [] }),
    datalogQuery: vi.fn().mockResolvedValue({ facts: [] }),
    sparqlQuery: vi.fn().mockResolvedValue({
      results: {
        bindings: []
      }
    })
  };
}

/**
 * Setup default mock responses for Meta-Log API service
 */
export function setupMockMetaLogApiService(
  mockService: MockMetaLogApiService,
  options: {
    available?: boolean;
    prologResults?: any;
    datalogResults?: any;
    sparqlResults?: any;
  } = {}
): void {
  mockService.isAvailable.mockReturnValue(options.available !== false);

  if (options.prologResults) {
    mockService.prologQuery.mockResolvedValue(options.prologResults);
  }

  if (options.datalogResults) {
    mockService.datalogQuery.mockResolvedValue(options.datalogResults);
  }

  if (options.sparqlResults) {
    mockService.sparqlQuery.mockResolvedValue(options.sparqlResults);
  }
}

