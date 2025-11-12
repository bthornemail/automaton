/**
 * Provenance Canvas Worker
 * OffscreenCanvas worker for rendering 3D provenance chains without blocking main thread
 * 
 * Note: This worker uses Three.js directly. In a browser environment, Three.js must be
 * available in the worker context. Vite will handle bundling this correctly.
 */

// Import Three.js - Vite will bundle this for the worker
// @ts-ignore - Three.js types may not be available in worker context
import * as THREE from 'three';

// Worker message types
interface WorkerMessage {
  type: 'init' | 'load' | 'query' | 'render' | 'interact' | 'updateCamera' | 'resize' | 'dispose';
  payload: any;
}

interface CanvasOptions {
  width: number;
  height: number;
  antialias?: boolean;
}

interface ProvenanceNode {
  id: string;
  type: 'agent' | 'document' | 'code' | 'interaction' | 'evolution';
  position: [number, number, number];
  metadata: {
    timestamp: number;
    file: string;
    line: number;
    agentId: string;
    dimension?: string;
    churchEncoding?: string;
  };
  data: any;
}

interface ProvenanceEdge {
  id: string;
  type: 'consumes' | 'produces' | 'references' | 'evolves' | 'interacts';
  from: string;
  to: string;
  metadata: {
    timestamp: number;
    weight: number;
    context: string;
  };
}

interface ProvenanceChain {
  nodes: ProvenanceNode[];
  edges: ProvenanceEdge[];
}

class ProvenanceCanvasRenderer {
  private scene: THREE.Scene;
  private camera: THREE.PerspectiveCamera;
  private renderer: THREE.WebGLRenderer;
  private nodes: Map<string, THREE.Mesh> = new Map();
  private edges: Map<string, THREE.Line> = new Map();
  private selectedNode: ProvenanceNode | null = null;
  private chain: ProvenanceChain | null = null;

  constructor(canvas: OffscreenCanvas, options: CanvasOptions) {
    // Initialize Three.js scene
    this.scene = new THREE.Scene();
    this.scene.background = new THREE.Color(0x1a1a1a);

    // Camera
    this.camera = new THREE.PerspectiveCamera(
      75,
      options.width / options.height,
      0.1,
      1000
    );
    this.camera.position.set(10, 10, 10);
    this.camera.lookAt(0, 0, 0);

    // Renderer
    this.renderer = new THREE.WebGLRenderer({
      canvas,
      antialias: options.antialias ?? true
    });
    this.renderer.setSize(options.width, options.height);
    // Note: devicePixelRatio is not available in worker context
    // Use default pixel ratio or pass it as option
    try {
      // @ts-ignore - devicePixelRatio may not be available in worker
      this.renderer.setPixelRatio(self.devicePixelRatio || 1);
    } catch (e) {
      this.renderer.setPixelRatio(1);
    }

    // Lights
    const ambientLight = new THREE.AmbientLight(0x404040, 0.6);
    this.scene.add(ambientLight);

    const directionalLight = new THREE.DirectionalLight(0xffffff, 0.8);
    directionalLight.position.set(10, 10, 5);
    this.scene.add(directionalLight);

    const pointLight = new THREE.PointLight(0x6366f1, 0.5);
    pointLight.position.set(-10, -10, -10);
    this.scene.add(pointLight);

    // Grid helper
    const gridHelper = new THREE.GridHelper(20, 20, 0x4b5563, 0x1f2937);
    this.scene.add(gridHelper);

    // Start render loop
    this.animate();
  }

  private animate = () => {
    try {
      // @ts-ignore - requestAnimationFrame may not be available in worker
      const raf = self.requestAnimationFrame || ((cb: () => void) => setTimeout(cb, 16));
      raf(this.animate);
      this.renderer.render(this.scene, this.camera);
    } catch (error) {
      console.error('Error in render loop:', error);
      // Fallback: use setTimeout if requestAnimationFrame fails
      setTimeout(this.animate, 16);
    }
  };

  loadProvenanceChain(chain: ProvenanceChain): void {
    this.chain = chain;
    this.clearScene();
    this.renderChain(chain);
  }

  private clearScene(): void {
    // Remove existing nodes
    for (const node of this.nodes.values()) {
      this.scene.remove(node);
      node.geometry.dispose();
      if (node.material instanceof THREE.Material) {
        node.material.dispose();
      }
    }
    this.nodes.clear();

    // Remove existing edges
    for (const edge of this.edges.values()) {
      this.scene.remove(edge);
      edge.geometry.dispose();
      if (edge.material instanceof THREE.Material) {
        edge.material.dispose();
      }
    }
    this.edges.clear();
  }

  private renderChain(chain: ProvenanceChain): void {
    // Render nodes
    for (const node of chain.nodes) {
      const mesh = this.createNodeMesh(node);
      this.scene.add(mesh);
      this.nodes.set(node.id, mesh);
    }

    // Render edges
    for (const edge of chain.edges) {
      const line = this.createEdgeLine(edge, chain.nodes);
      if (line) {
        this.scene.add(line);
        this.edges.set(edge.id, line);
      }
    }
  }

  private createNodeMesh(node: ProvenanceNode): THREE.Mesh {
    const geometry = new THREE.SphereGeometry(0.3, 16, 16);
    const color = this.getNodeColor(node.type);
    const material = new THREE.MeshStandardMaterial({
      color,
      emissive: color,
      emissiveIntensity: 0.3,
      roughness: 0.2,
      metalness: 0.8
    });

    const mesh = new THREE.Mesh(geometry, material);
    mesh.position.set(...node.position);
    mesh.userData = { node };

    return mesh;
  }

  private createEdgeLine(
    edge: ProvenanceEdge,
    nodes: ProvenanceNode[]
  ): THREE.Line | null {
    const fromNode = nodes.find(n => n.id === edge.from);
    const toNode = nodes.find(n => n.id === edge.to);

    if (!fromNode || !toNode) {
      return null;
    }

    const geometry = new THREE.BufferGeometry().setFromPoints([
      new THREE.Vector3(...fromNode.position),
      new THREE.Vector3(...toNode.position)
    ]);

    const color = this.getEdgeColor(edge.type);
    const material = new THREE.LineBasicMaterial({
      color,
      linewidth: 2,
      opacity: 0.6,
      transparent: true
    });

    return new THREE.Line(geometry, material);
  }

  private getNodeColor(type: ProvenanceNode['type']): number {
    const colors: Record<ProvenanceNode['type'], number> = {
      agent: 0x6366f1,
      document: 0x10b981,
      code: 0xf59e0b,
      interaction: 0xec4899,
      evolution: 0x06b6d4
    };
    return colors[type] || 0xffffff;
  }

  private getEdgeColor(type: ProvenanceEdge['type']): number {
    const colors: Record<ProvenanceEdge['type'], number> = {
      consumes: 0x10b981,
      produces: 0xf59e0b,
      references: 0x6366f1,
      evolves: 0x06b6d4,
      interacts: 0xec4899
    };
    return colors[type] || 0xffffff;
  }

  handleClick(x: number, y: number, width: number, height: number): ProvenanceNode | null {
    // Convert screen coordinates to normalized device coordinates
    const mouse = new THREE.Vector2();
    mouse.x = (x / width) * 2 - 1;
    mouse.y = -(y / height) * 2 + 1;

    const raycaster = new THREE.Raycaster();
    raycaster.setFromCamera(mouse, this.camera);

    const intersects = raycaster.intersectObjects(Array.from(this.nodes.values()));

    if (intersects.length > 0) {
      const mesh = intersects[0].object as THREE.Mesh;
      const node = mesh.userData.node as ProvenanceNode;
      this.selectedNode = node;
      return node;
    }

    return null;
  }

  handleHover(x: number, y: number, width: number, height: number): ProvenanceNode | null {
    const mouse = new THREE.Vector2();
    mouse.x = (x / width) * 2 - 1;
    mouse.y = -(y / height) * 2 + 1;

    const raycaster = new THREE.Raycaster();
    raycaster.setFromCamera(mouse, this.camera);

    const intersects = raycaster.intersectObjects(Array.from(this.nodes.values()));

    if (intersects.length > 0) {
      const mesh = intersects[0].object as THREE.Mesh;
      return mesh.userData.node as ProvenanceNode;
    }

    return null;
  }

  updateCamera(position: [number, number, number], target: [number, number, number]): void {
    this.camera.position.set(...position);
    this.camera.lookAt(...target);
  }

  resize(width: number, height: number): void {
    this.camera.aspect = width / height;
    this.camera.updateProjectionMatrix();
    this.renderer.setSize(width, height);
  }

  dispose(): void {
    this.clearScene();
    this.renderer.dispose();
  }
}

// Worker global scope
let renderer: ProvenanceCanvasRenderer | null = null;

// Error handling wrapper
function handleWorkerMessage(event: MessageEvent<WorkerMessage>) {
  try {
    const { type, payload } = event.data;

    switch (type) {
      case 'init':
        try {
          const { canvas, options } = payload;
          if (!canvas || !options) {
            throw new Error('Invalid init payload: canvas and options required');
          }
          renderer = new ProvenanceCanvasRenderer(canvas, options);
          self.postMessage({ type: 'initialized' });
        } catch (error) {
          console.error('Worker initialization error:', error);
          self.postMessage({ 
            type: 'error', 
            payload: { 
              message: error instanceof Error ? error.message : 'Unknown initialization error',
              type: 'init'
            } 
          });
        }
        break;

      case 'load':
        try {
          if (!renderer) {
            throw new Error('Renderer not initialized');
          }
          if (!payload.chain) {
            throw new Error('Invalid load payload: chain required');
          }
          renderer.loadProvenanceChain(payload.chain);
          self.postMessage({ type: 'loaded', payload: { nodeCount: payload.chain.nodes.length } });
        } catch (error) {
          console.error('Worker load error:', error);
          self.postMessage({ 
            type: 'error', 
            payload: { 
              message: error instanceof Error ? error.message : 'Unknown load error',
              type: 'load'
            } 
          });
        }
        break;

      case 'interact':
        try {
          if (!renderer) {
            throw new Error('Renderer not initialized');
          }
          const { x, y, width, height, interactionType } = payload;
          if (typeof x !== 'number' || typeof y !== 'number' || typeof width !== 'number' || typeof height !== 'number') {
            throw new Error('Invalid interact payload: x, y, width, height must be numbers');
          }
          let result: ProvenanceNode | null = null;
          
          if (interactionType === 'click') {
            result = renderer.handleClick(x, y, width, height);
          } else if (interactionType === 'hover') {
            result = renderer.handleHover(x, y, width, height);
          } else {
            throw new Error(`Invalid interaction type: ${interactionType}`);
          }
          
          if (result) {
            self.postMessage({ type: 'nodeSelected', payload: { node: result } });
          } else {
            self.postMessage({ type: 'nodeSelected', payload: { node: null } });
          }
        } catch (error) {
          console.error('Worker interact error:', error);
          self.postMessage({ 
            type: 'error', 
            payload: { 
              message: error instanceof Error ? error.message : 'Unknown interact error',
              type: 'interact'
            } 
          });
        }
        break;

      case 'updateCamera':
        try {
          if (!renderer) {
            throw new Error('Renderer not initialized');
          }
          if (!payload.position || !payload.target) {
            throw new Error('Invalid updateCamera payload: position and target required');
          }
          renderer.updateCamera(payload.position, payload.target);
        } catch (error) {
          console.error('Worker updateCamera error:', error);
          self.postMessage({ 
            type: 'error', 
            payload: { 
              message: error instanceof Error ? error.message : 'Unknown updateCamera error',
              type: 'updateCamera'
            } 
          });
        }
        break;

      case 'resize':
        try {
          if (!renderer) {
            throw new Error('Renderer not initialized');
          }
          if (typeof payload.width !== 'number' || typeof payload.height !== 'number') {
            throw new Error('Invalid resize payload: width and height must be numbers');
          }
          renderer.resize(payload.width, payload.height);
        } catch (error) {
          console.error('Worker resize error:', error);
          self.postMessage({ 
            type: 'error', 
            payload: { 
              message: error instanceof Error ? error.message : 'Unknown resize error',
              type: 'resize'
            } 
          });
        }
        break;

      case 'dispose':
        try {
          if (renderer) {
            renderer.dispose();
            renderer = null;
          }
          self.postMessage({ type: 'disposed' });
        } catch (error) {
          console.error('Worker dispose error:', error);
          // Still try to clean up
          renderer = null;
        }
        break;

      default:
        console.warn(`Unknown worker message type: ${type}`);
        self.postMessage({ 
          type: 'error', 
          payload: { 
            message: `Unknown message type: ${type}`,
            type: 'unknown'
          } 
        });
    }
  } catch (error) {
    console.error('Unhandled worker error:', error);
    self.postMessage({ 
      type: 'error', 
      payload: { 
        message: error instanceof Error ? error.message : 'Unknown error',
        type: 'unhandled'
      } 
    });
  }
}

self.onmessage = handleWorkerMessage;

// Handle uncaught errors
self.onerror = (error) => {
  console.error('Worker uncaught error:', error);
  self.postMessage({ 
    type: 'error', 
    payload: { 
      message: error.message || 'Uncaught error in worker',
      type: 'uncaught'
    } 
  });
  return true; // Prevent default error handling
};
