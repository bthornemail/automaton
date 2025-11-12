/**
 * Example 9: Error Handling Patterns
 * 
 * This example demonstrates proper error handling patterns
 * for all service operations.
 */

import { provenanceSlideService } from '@/services/provenance-slide-service';
import { ProvenanceCanvasWorkerService } from '@/services/provenance-canvas-worker-service';
import { WorkerError } from '@/utils/error-types';
import { errorLoggingService } from '@/services/error-logging-service';

async function handleErrors() {
  // Pattern 1: Service initialization errors
  try {
    await provenanceSlideService.init();
  } catch (error) {
    const errorObj = error instanceof Error ? error : new Error(String(error));
    errorLoggingService.logError(errorObj, {
      service: 'ProvenanceSlideService',
      action: 'init',
      severity: 'error'
    });
    console.error('Service initialization failed:', errorObj.message);
    throw errorObj;
  }

  // Pattern 2: Provenance chain building errors
  try {
    const chain = await provenanceSlideService.buildProvenanceChain('/evolutions/advanced-automaton');
    return chain;
  } catch (error) {
    const errorObj = error instanceof Error ? error : new Error(String(error));
    
    // Check for specific error types
    if (errorObj.message.includes('Failed to load evolution files')) {
      console.error('Evolution files not found');
      // Try alternative path or show user message
      return null;
    }
    
    if (errorObj.message.includes('Failed to extract patterns')) {
      console.error('Pattern extraction failed');
      // Validate file format
      return null;
    }
    
    // Log and rethrow
    errorLoggingService.logError(errorObj, {
      service: 'ProvenanceSlideService',
      action: 'buildProvenanceChain',
      severity: 'error'
    });
    throw errorObj;
  }
}

async function handleWorkerErrors() {
  // Pattern 3: Worker initialization errors
  const canvas = document.getElementById('canvas') as HTMLCanvasElement;
  if (!canvas) {
    throw new Error('Canvas element not found');
  }

  try {
    const offscreenCanvas = canvas.transferControlToOffscreen();
    const workerService = new ProvenanceCanvasWorkerService();
    
    await workerService.init(offscreenCanvas, {
      width: canvas.width,
      height: canvas.height,
      antialias: true
    });
  } catch (error) {
    if (error instanceof WorkerError) {
      console.error('Worker error:', error.message);
      console.log('Fallback mode:', error.fallbackMode);
      
      // Handle fallback mode
      if (error.fallbackMode === '2d-only') {
        console.log('Using 2D fallback rendering');
        // Implement 2D rendering
      }
    } else {
      const errorObj = error instanceof Error ? error : new Error(String(error));
      errorLoggingService.logError(errorObj, {
        service: 'ProvenanceCanvasWorkerService',
        action: 'init',
        severity: 'error'
      });
      throw errorObj;
    }
  }
}

async function handleAsyncErrors() {
  // Pattern 4: Async operation errors with retry
  const maxRetries = 3;
  let lastError: Error | null = null;

  for (let attempt = 0; attempt < maxRetries; attempt++) {
    try {
      const chain = await provenanceSlideService.buildProvenanceChain('/evolutions/advanced-automaton');
      return chain;
    } catch (error) {
      lastError = error instanceof Error ? error : new Error(String(error));
      
      if (attempt < maxRetries - 1) {
        console.warn(`Attempt ${attempt + 1} failed, retrying...`);
        await new Promise(resolve => setTimeout(resolve, 1000 * (attempt + 1)));
      }
    }
  }

  // All retries failed
  if (lastError) {
    errorLoggingService.logError(lastError, {
      service: 'ProvenanceSlideService',
      action: 'buildProvenanceChain',
      metadata: { retries: maxRetries },
      severity: 'error'
    });
    throw lastError;
  }
}

// Pattern 5: Validation errors
function validateChain(chain: any): void {
  if (!chain) {
    throw new Error('Chain is null or undefined');
  }

  if (!Array.isArray(chain.nodes)) {
    throw new Error('Chain nodes must be an array');
  }

  if (!Array.isArray(chain.edges)) {
    throw new Error('Chain edges must be an array');
  }

  // Check for duplicate node IDs
  const nodeIds = new Set(chain.nodes.map((n: any) => n.id));
  if (nodeIds.size !== chain.nodes.length) {
    throw new Error('Duplicate node IDs found');
  }
}

// Usage example
async function demonstrateErrorHandling() {
  try {
    // Handle service errors
    await handleErrors();
    
    // Handle worker errors
    await handleWorkerErrors();
    
    // Handle async errors with retry
    const chain = await handleAsyncErrors();
    
    // Validate chain
    if (chain) {
      validateChain(chain);
      console.log('Chain validated successfully');
    }
  } catch (error) {
    const errorObj = error instanceof Error ? error : new Error(String(error));
    console.error('Error handling demonstration failed:', errorObj.message);
  }
}

// Run the example
demonstrateErrorHandling()
  .then(() => {
    console.log('Error handling example completed');
  })
  .catch(error => {
    console.error('Error handling example failed:', error);
  });

