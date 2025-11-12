/**
 * Error Handling Utility Functions
 */

import { TimeoutError, ValidationError } from './error-types';

export interface RetryOptions {
  maxRetries?: number;
  initialDelay?: number;
  maxDelay?: number;
  backoffStrategy?: 'exponential' | 'linear' | 'fixed';
  shouldRetry?: (error: Error) => boolean;
}

export interface RetryResult<T> {
  result: T;
  attempts: number;
}

/**
 * Retry a function with configurable backoff strategy
 */
export async function retryWithBackoff<T>(
  fn: () => Promise<T>,
  options: RetryOptions = {}
): Promise<RetryResult<T>> {
  const {
    maxRetries = 3,
    initialDelay = 1000,
    maxDelay = 10000,
    backoffStrategy = 'exponential',
    shouldRetry = (error: Error) => {
      // Default: retry on network/timeout errors, not validation errors
      return (
        error.name === 'NetworkError' ||
        error.name === 'TimeoutError' ||
        error.message.includes('network') ||
        error.message.includes('timeout') ||
        error.message.includes('fetch')
      );
    }
  } = options;

  let lastError: Error | null = null;
  let attempts = 0;

  for (let attempt = 0; attempt <= maxRetries; attempt++) {
    attempts = attempt + 1;
    try {
      const result = await fn();
      return { result, attempts };
    } catch (error) {
      lastError = error instanceof Error ? error : new Error(String(error));

      // Don't retry if it's not a retryable error
      if (!shouldRetry(lastError)) {
        throw lastError;
      }

      // Don't retry on last attempt
      if (attempt === maxRetries) {
        break;
      }

      // Calculate delay based on strategy
      let delay: number;
      switch (backoffStrategy) {
        case 'exponential':
          delay = Math.min(initialDelay * Math.pow(2, attempt), maxDelay);
          break;
        case 'linear':
          delay = Math.min(initialDelay * (attempt + 1), maxDelay);
          break;
        case 'fixed':
          delay = initialDelay;
          break;
        default:
          delay = initialDelay;
      }

      await new Promise(resolve => setTimeout(resolve, delay));
    }
  }

  throw lastError || new Error('Retry failed');
}

/**
 * Add timeout to a promise
 */
export async function withTimeout<T>(
  promise: Promise<T>,
  timeout: number,
  operation?: string
): Promise<T> {
  return new Promise<T>((resolve, reject) => {
    const timeoutId = setTimeout(() => {
      reject(
        new TimeoutError(
          `Operation timed out after ${timeout}ms${operation ? `: ${operation}` : ''}`,
          timeout,
          operation
        )
      );
    }, timeout);

    promise
      .then(result => {
        clearTimeout(timeoutId);
        resolve(result);
      })
      .catch(error => {
        clearTimeout(timeoutId);
        reject(error);
      });
  });
}

/**
 * Format technical error to user-friendly message
 */
export function formatUserErrorMessage(error: Error): string {
  const errorName = error.name;
  const errorMessage = error.message;

  // Map common error types to user-friendly messages
  const errorMappings: Record<string, string> = {
    TimeoutError: 'The operation took too long to complete. Please try again.',
    NetworkError: 'Network connection failed. Please check your internet connection and try again.',
    ValidationError: 'Invalid data provided. Please check your input and try again.',
    WorkerError: '3D rendering is not available. Showing 2D view instead.',
    FileSystemError: 'File operation failed. Please check file permissions and try again.',
    'Failed to fetch': 'Unable to connect to the server. Please check your network connection.',
    'Network request failed': 'Network connection failed. Please check your internet connection.'
  };

  // Check for specific error patterns
  if (errorMessage.includes('timeout') || errorMessage.includes('timed out')) {
    return 'The operation took too long. Please try again.';
  }

  if (errorMessage.includes('network') || errorMessage.includes('fetch')) {
    return 'Network connection failed. Please check your internet connection and try again.';
  }

  if (errorMessage.includes('permission') || errorMessage.includes('access denied')) {
    return 'Permission denied. Please check file permissions and try again.';
  }

  if (errorMessage.includes('not found') || errorMessage.includes('404')) {
    return 'The requested resource was not found. Please check the path and try again.';
  }

  // Use mapped message if available
  if (errorMappings[errorName]) {
    return errorMappings[errorName];
  }

  // Fallback to generic message
  return 'An error occurred. Please try again or contact support if the problem persists.';
}

/**
 * Validate provenance chain structure
 */
export function validateProvenanceChain(chain: {
  nodes?: any[];
  edges?: any[];
}): void {
  const errors: Record<string, string[]> = {};

  if (!chain) {
    throw new ValidationError('Provenance chain is required', { chain: ['Chain object is missing'] });
  }

  // Validate nodes
  if (!Array.isArray(chain.nodes)) {
    errors.nodes = ['Nodes must be an array'];
  } else {
    chain.nodes.forEach((node, index) => {
      const nodeErrors: string[] = [];

      if (!node.id) {
        nodeErrors.push('id is required');
      }
      if (!node.type) {
        nodeErrors.push('type is required');
      }
      if (!node.position || !Array.isArray(node.position) || node.position.length !== 3) {
        nodeErrors.push('position must be an array of 3 numbers');
      }
      if (!node.metadata) {
        nodeErrors.push('metadata is required');
      } else {
        if (typeof node.metadata.timestamp !== 'number') {
          nodeErrors.push('metadata.timestamp must be a number');
        }
        if (!node.metadata.file) {
          nodeErrors.push('metadata.file is required');
        }
        if (typeof node.metadata.line !== 'number') {
          nodeErrors.push('metadata.line must be a number');
        }
        if (!node.metadata.agentId) {
          nodeErrors.push('metadata.agentId is required');
        }
      }

      if (nodeErrors.length > 0) {
        errors[`nodes[${index}]`] = nodeErrors;
      }
    });
  }

  // Validate edges
  if (!Array.isArray(chain.edges)) {
    errors.edges = ['Edges must be an array'];
  } else {
    chain.edges.forEach((edge, index) => {
      const edgeErrors: string[] = [];

      if (!edge.id) {
        edgeErrors.push('id is required');
      }
      if (!edge.type) {
        edgeErrors.push('type is required');
      }
      if (!edge.from) {
        edgeErrors.push('from is required');
      }
      if (!edge.to) {
        edgeErrors.push('to is required');
      }
      if (!edge.metadata) {
        edgeErrors.push('metadata is required');
      } else {
        if (typeof edge.metadata.timestamp !== 'number') {
          edgeErrors.push('metadata.timestamp must be a number');
        }
        if (typeof edge.metadata.weight !== 'number') {
          edgeErrors.push('metadata.weight must be a number');
        }
        if (!edge.metadata.context) {
          edgeErrors.push('metadata.context is required');
        }
      }

      if (edgeErrors.length > 0) {
        errors[`edges[${index}]`] = edgeErrors;
      }
    });
  }

  if (Object.keys(errors).length > 0) {
    throw new ValidationError('Provenance chain validation failed', errors, 'ProvenanceChain');
  }
}

/**
 * Validate automaton state structure
 */
export function validateAutomatonState(state: any): void {
  const errors: Record<string, string[]> = {};

  if (!state) {
    throw new ValidationError('Automaton state is required', { state: ['State object is missing'] });
  }

  // Validate required fields
  if (!state.id || typeof state.id !== 'string') {
    errors.id = ['id is required and must be a string'];
  }

  if (!state.dimension || typeof state.dimension !== 'string') {
    errors.dimension = ['dimension is required and must be a string'];
  } else {
    // Validate dimension format (0D-7D)
    const dimMatch = state.dimension.match(/^(\d+)D$/);
    if (!dimMatch) {
      errors.dimension = ['dimension must be in format "0D" through "7D"'];
    } else {
      const dimNum = parseInt(dimMatch[1]);
      if (dimNum < 0 || dimNum > 7) {
        errors.dimension = ['dimension must be between 0D and 7D'];
      }
    }
  }

  // Validate arrays
  const arrayFields = ['kernel', 'seed', 'topology', 'system'];
  for (const field of arrayFields) {
    if (!Array.isArray(state[field])) {
      errors[field] = [`${field} must be an array`];
    }
  }

  if (Object.keys(errors).length > 0) {
    throw new ValidationError('Automaton state validation failed', errors, 'AutomatonState');
  }
}

/**
 * Write content to temporary file
 * Note: In browser environment, this uses databaseService.writeCanvasL with .tmp extension
 */
export async function writeToTempFile(
  content: string,
  finalPath: string,
  writeFunction: (path: string, content: string) => Promise<void>
): Promise<string> {
  const tempPath = `${finalPath}.tmp`;
  await writeFunction(tempPath, content);
  return tempPath;
}

/**
 * Move temporary file to final location atomically
 * Note: In browser environment, this uses databaseService operations
 */
export async function moveTempFile(
  tempPath: string,
  finalPath: string,
  readFunction: (path: string) => Promise<string>,
  writeFunction: (path: string, content: string) => Promise<void>,
  deleteFunction: (path: string) => Promise<void>
): Promise<void> {
  try {
    // Read temp file
    const content = await readFunction(tempPath);
    
    // Validate content (basic check)
    if (!content || content.trim().length === 0) {
      throw new Error('Temporary file is empty');
    }
    
    // Write to final location
    await writeFunction(finalPath, content);
    
    // Delete temp file
    await deleteFunction(tempPath);
  } catch (error) {
    // Clean up temp file on error
    try {
      await deleteFunction(tempPath);
    } catch (cleanupError) {
      // Ignore cleanup errors
    }
    throw error;
  }
}

