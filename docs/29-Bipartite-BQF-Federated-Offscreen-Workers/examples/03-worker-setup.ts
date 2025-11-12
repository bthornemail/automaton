/**
 * Example 3: Setting Up Offscreen Canvas Worker
 * 
 * This example demonstrates how to set up and use the offscreen
 * canvas worker for 3D rendering.
 */

import { ProvenanceCanvasWorkerService } from '@/services/provenance-canvas-worker-service';
import { provenanceSlideService } from '@/services/provenance-slide-service';

async function setupWorker() {
  // Step 1: Check browser support
  if (!ProvenanceCanvasWorkerService.isSupported()) {
    console.warn('OffscreenCanvas not supported, using 2D fallback');
    return;
  }

  // Step 2: Get canvas element
  const canvas = document.getElementById('provenance-canvas') as HTMLCanvasElement;
  if (!canvas) {
    throw new Error('Canvas element not found');
  }

  const offscreenCanvas = canvas.transferControlToOffscreen();

  // Step 3: Create and initialize worker service
  const workerService = new ProvenanceCanvasWorkerService();

  try {
    await workerService.init(offscreenCanvas, {
      width: canvas.width,
      height: canvas.height,
      antialias: true
    });
    console.log('Worker initialized successfully');
  } catch (error) {
    console.error('Worker initialization failed:', error);
    return;
  }

  // Step 4: Build and load provenance chain
  await provenanceSlideService.init();
  const chain = await provenanceSlideService.buildProvenanceChain('/evolutions/advanced-automaton');
  
  workerService.loadProvenanceChain(chain);
  console.log('Provenance chain loaded into worker');

  // Step 5: Set up message handlers
  workerService.onMessage('nodeSelected', (node) => {
    console.log('Node selected:', node.id);
  });

  workerService.onMessage('error', (error) => {
    console.error('Worker error:', error);
  });

  // Step 6: Handle user interactions
  canvas.addEventListener('click', async (event) => {
    const rect = canvas.getBoundingClientRect();
    const x = event.clientX - rect.left;
    const y = event.clientY - rect.top;

    const node = await workerService.handleInteraction(
      x,
      y,
      canvas.width,
      canvas.height,
      'click'
    );

    if (node) {
      console.log('Selected node:', node.id);
    }
  });

  // Step 7: Handle resize
  window.addEventListener('resize', () => {
    workerService.resize(canvas.width, canvas.height);
  });

  // Step 8: Cleanup on unmount
  return () => {
    workerService.dispose();
  };
}

// Run the example
setupWorker()
  .then(cleanup => {
    console.log('Worker setup completed');
    // Store cleanup function for later use
    if (cleanup) {
      // Call cleanup when component unmounts
    }
  })
  .catch(error => {
    console.error('Worker setup failed:', error);
  });

