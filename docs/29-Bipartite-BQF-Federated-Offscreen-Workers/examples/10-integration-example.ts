/**
 * Example 10: Complete Integration Example
 * 
 * This example demonstrates a complete integration of all services
 * for visualizing provenance chains with avatars, thought cards, and
 * knowledge graphs.
 */

import { provenanceSlideService } from '@/services/provenance-slide-service';
import { ProvenanceCanvasWorkerService } from '@/services/provenance-canvas-worker-service';
import { avatarLoaderService } from '@/services/avatar-loader-service';
import { thoughtCardService } from '@/services/thought-card-service';
import { knowledgeGraphCardService } from '@/services/knowledge-graph-card-service';
import { performanceMonitoringService } from '@/services/performance-monitoring-service';
import * as THREE from 'three';

interface VisualizationResult {
  chain: any;
  slides: any[];
  scene: THREE.Scene;
  thoughtCards: any[];
  knowledgeGraphs: any[];
}

async function createCompleteVisualization(
  evolutionPath: string,
  canvas: HTMLCanvasElement
): Promise<VisualizationResult> {
  // Step 1: Initialize all services
  console.log('Initializing services...');
  await provenanceSlideService.init();
  await avatarLoaderService.preloadTemplates();
  performanceMonitoringService.startMonitoring();

  // Step 2: Build provenance chain
  console.log('Building provenance chain...');
  const chain = await provenanceSlideService.buildProvenanceChain(evolutionPath);
  console.log(`Chain built: ${chain.nodes.length} nodes, ${chain.edges.length} edges`);

  // Step 3: Generate slides
  console.log('Generating slides...');
  const slides = await provenanceSlideService.generateSlidesFromEvolution(evolutionPath);
  console.log(`Generated ${slides.length} slides`);

  // Step 4: Initialize worker (if supported)
  let workerService: ProvenanceCanvasWorkerService | null = null;
  if (ProvenanceCanvasWorkerService.isSupported()) {
    try {
      const offscreenCanvas = canvas.transferControlToOffscreen();
      workerService = new ProvenanceCanvasWorkerService();
      await workerService.init(offscreenCanvas, {
        width: canvas.width,
        height: canvas.height,
        antialias: true
      });
      workerService.loadProvenanceChain(chain);
      console.log('Worker initialized and chain loaded');
    } catch (error) {
      console.warn('Worker initialization failed, using fallback:', error);
    }
  }

  // Step 5: Create 3D scene
  console.log('Creating 3D scene...');
  const scene = new THREE.Scene();
  const camera = new THREE.PerspectiveCamera(75, canvas.width / canvas.height, 0.1, 1000);
  const renderer = new THREE.WebGLRenderer({ canvas });

  // Step 6: Load avatars
  console.log('Loading avatars...');
  const avatarPromises = chain.nodes
    .filter(node => node.avatar)
    .map(async (node) => {
      const model = await avatarLoaderService.loadAvatar(node.avatar!);
      model.position.set(...node.position);
      scene.add(model);
      return { node, model };
    });

  const avatars = await Promise.all(avatarPromises);
  console.log(`Loaded ${avatars.length} avatars`);

  // Step 7: Create thought cards
  console.log('Creating thought cards...');
  const thoughtCards = chain.nodes
    .filter(node => node.avatar)
    .map(node => thoughtCardService.createThoughtCardFromNode(node))
    .filter(card => card !== null);

  // Add thought cards to scene
  thoughtCards.forEach(card => {
    const texture = thoughtCardService.getCardTexture(card!.id);
    if (texture) {
      const material = new THREE.MeshBasicMaterial({
        map: new THREE.CanvasTexture(texture),
        transparent: true,
        opacity: card!.opacity
      });
      const geometry = new THREE.PlaneGeometry(...card!.size);
      const mesh = new THREE.Mesh(geometry, material);

      const node = chain.nodes.find(n => n.id === card!.avatarId);
      if (node) {
        const [offsetX, offsetY, offsetZ] = card!.offset;
        mesh.position.set(
          node.position[0] + offsetX,
          node.position[1] + offsetY,
          node.position[2] + offsetZ
        );
        scene.add(mesh);
      }
    }
  });
  console.log(`Created ${thoughtCards.length} thought cards`);

  // Step 8: Create knowledge graphs
  console.log('Creating knowledge graphs...');
  const agentIds = new Set<string>();
  slides.forEach(slide => {
    slide.provenanceChain?.nodes.forEach(node => {
      if (node.metadata.agentId) {
        agentIds.add(node.metadata.agentId);
      }
    });
  });

  const knowledgeGraphs = slides.flatMap(slide =>
    Array.from(agentIds).map(agentId =>
      knowledgeGraphCardService.buildKnowledgeGraph(slide, agentId)
    )
  );
  console.log(`Created ${knowledgeGraphs.length} knowledge graphs`);

  // Step 9: Set up camera
  camera.position.set(10, 10, 10);
  camera.lookAt(0, 0, 0);

  // Step 10: Render loop
  function animate() {
    requestAnimationFrame(animate);
    renderer.render(scene, camera);
  }
  animate();

  // Step 11: Monitor performance
  setInterval(() => {
    const metrics = performanceMonitoringService.getMetrics();
    if (metrics.fps < 30) {
      console.warn('Low FPS detected:', metrics.fps);
    }
  }, 5000);

  return {
    chain,
    slides,
    scene,
    thoughtCards,
    knowledgeGraphs
  };
}

// Usage example
async function runCompleteIntegration() {
  const canvas = document.getElementById('provenance-canvas') as HTMLCanvasElement;
  if (!canvas) {
    throw new Error('Canvas element not found');
  }

  try {
    const result = await createCompleteVisualization('/evolutions/advanced-automaton', canvas);
    console.log('Complete visualization created successfully');
    console.log('Result:', {
      nodes: result.chain.nodes.length,
      slides: result.slides.length,
      thoughtCards: result.thoughtCards.length,
      knowledgeGraphs: result.knowledgeGraphs.length
    });
    return result;
  } catch (error) {
    console.error('Complete integration failed:', error);
    throw error;
  }
}

// Run the example
runCompleteIntegration()
  .then(result => {
    console.log('Complete integration example completed');
  })
  .catch(error => {
    console.error('Complete integration example failed:', error);
  });

