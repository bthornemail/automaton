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
  
  // Performance optimizations
  private instancedMeshes: Map<string, THREE.InstancedMesh> = new Map();
  private nodeGroups: Map<string, THREE.Group> = new Map();
  private lodNodes: Map<string, THREE.LOD> = new Map();
  private frustum: THREE.Frustum = new THREE.Frustum();
  private matrix: THREE.Matrix4 = new THREE.Matrix4();
  private enableLOD: boolean = true;
  private enableFrustumCulling: boolean = true;
  private enableInstancing: boolean = true;

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
      // Update LOD based on camera distance
      if (this.enableLOD && this.chain) {
        this.updateLOD();
      }

      // Update frustum culling
      if (this.enableFrustumCulling) {
        this.updateFrustum();
        this.cullNodes();
      }

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

  /**
   * Update LOD for all nodes based on camera distance
   */
  private updateLOD(): void {
    if (!this.chain) return;

    for (const node of this.chain.nodes) {
      const position = new THREE.Vector3(...node.position);
      const distance = this.camera.position.distanceTo(position);

      // Update LOD if exists
      const lod = this.lodNodes.get(node.id);
      if (lod) {
        lod.update(this.camera);
      }
    }
  }

  /**
   * Cull nodes outside frustum
   */
  private cullNodes(): void {
    if (!this.chain) return;

    for (const node of this.chain.nodes) {
      const mesh = this.nodes.get(node.id);
      if (mesh) {
        mesh.visible = this.isNodeVisible(node);
      }
    }
  }

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

    // Remove instanced meshes
    for (const instancedMesh of this.instancedMeshes.values()) {
      this.scene.remove(instancedMesh);
      instancedMesh.geometry.dispose();
      if (instancedMesh.material instanceof THREE.Material) {
        instancedMesh.material.dispose();
      }
    }
    this.instancedMeshes.clear();

    // Remove LOD nodes
    for (const lod of this.lodNodes.values()) {
      this.scene.remove(lod);
      lod.children.forEach(child => {
        if (child instanceof THREE.Mesh) {
          child.geometry.dispose();
          if (child.material instanceof THREE.Material) {
            child.material.dispose();
          }
        }
      });
    }
    this.lodNodes.clear();

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
    // Update frustum for culling
    if (this.enableFrustumCulling) {
      this.updateFrustum();
    }

    // Group nodes by type for instancing
    if (this.enableInstancing && chain.nodes.length > 100) {
      this.renderInstancedNodes(chain.nodes);
    } else {
      // Render individual nodes
      for (const node of chain.nodes) {
        if (this.enableFrustumCulling && !this.isNodeVisible(node)) {
          continue;
        }
        
        const mesh = this.createNodeMesh(node);
        this.scene.add(mesh);
        this.nodes.set(node.id, mesh);
      }
    }

    // Render optimized edges
    this.renderOptimizedEdges(chain.edges, chain.nodes);
  }

  /**
   * Render nodes using instancing for better performance
   */
  private renderInstancedNodes(nodes: ProvenanceNode[]): void {
    // Group nodes by type
    const nodesByType = new Map<string, ProvenanceNode[]>();
    for (const node of nodes) {
      const type = node.type;
      if (!nodesByType.has(type)) {
        nodesByType.set(type, []);
      }
      nodesByType.get(type)!.push(node);
    }

    // Create instanced mesh for each type
    for (const [type, typeNodes] of nodesByType) {
      if (typeNodes.length === 0) continue;

      const geometry = new THREE.SphereGeometry(0.3, 16, 16);
      const color = this.getNodeColor(type as ProvenanceNode['type']);
      const material = new THREE.MeshStandardMaterial({
        color,
        emissive: color,
        emissiveIntensity: 0.3,
        roughness: 0.2,
        metalness: 0.8
      });

      const instancedMesh = new THREE.InstancedMesh(geometry, material, typeNodes.length);
      const matrix = new THREE.Matrix4();

      for (let i = 0; i < typeNodes.length; i++) {
        const node = typeNodes[i];
        matrix.setPosition(...node.position);
        instancedMesh.setMatrixAt(i, matrix);
        instancedMesh.setColorAt(i, new THREE.Color(color));
      }

      instancedMesh.instanceMatrix.needsUpdate = true;
      this.scene.add(instancedMesh);
      this.instancedMeshes.set(type, instancedMesh);
    }
  }

  /**
   * Render optimized edges with shared geometry
   */
  private renderOptimizedEdges(edges: ProvenanceEdge[], nodes: ProvenanceNode[]): void {
    // Group edges by type
    const edgesByType = new Map<string, ProvenanceEdge[]>();
    for (const edge of edges) {
      if (!edgesByType.has(edge.type)) {
        edgesByType.set(edge.type, []);
      }
      edgesByType.get(edge.type)!.push(edge);
    }

    // Create line segments for each edge type
    for (const [type, typeEdges] of edgesByType) {
      const points: THREE.Vector3[] = [];
      const colors: number[] = [];
      const color = this.getEdgeColor(type as ProvenanceEdge['type']);

      for (const edge of typeEdges) {
        const fromNode = nodes.find(n => n.id === edge.from);
        const toNode = nodes.find(n => n.id === edge.to);

        if (!fromNode || !toNode) continue;

        // Check distance for edge culling
        const distance = new THREE.Vector3(...fromNode.position).distanceTo(
          new THREE.Vector3(...toNode.position)
        );
        if (distance > 50) continue; // Cull distant edges

        points.push(new THREE.Vector3(...fromNode.position));
        points.push(new THREE.Vector3(...toNode.position));
        colors.push(color, color);
      }

      if (points.length === 0) continue;

      const geometry = new THREE.BufferGeometry().setFromPoints(points);
      const colorAttribute = new THREE.Float32BufferAttribute(colors, 3);
      geometry.setAttribute('color', colorAttribute);

      const material = new THREE.LineBasicMaterial({
        vertexColors: true,
        opacity: 0.6,
        transparent: true
      });

      const lineSegments = new THREE.LineSegments(geometry, material);
      this.scene.add(lineSegments);
      this.edges.set(`type-${type}`, lineSegments);
    }
  }

  /**
   * Update camera frustum for culling
   */
  private updateFrustum(): void {
    this.matrix.multiplyMatrices(
      this.camera.projectionMatrix,
      this.camera.matrixWorldInverse
    );
    this.frustum.setFromProjectionMatrix(this.matrix);
  }

  /**
   * Check if node is visible in frustum
   */
  private isNodeVisible(node: ProvenanceNode): boolean {
    const position = new THREE.Vector3(...node.position);
    const distance = this.camera.position.distanceTo(position);

    // Distance culling
    if (distance > 200) return false;

    // Frustum culling
    const box = new THREE.Box3().setFromCenterAndSize(position, new THREE.Vector3(0.6, 0.6, 0.6));
    return this.frustum.intersectsBox(box);
  }

  /**
   * Create LOD node with multiple detail levels
   */
  private createLODNode(node: ProvenanceNode, distance: number): THREE.LOD {
    const lod = new THREE.LOD();

    // High detail (close)
    if (distance < 20) {
      const highDetail = this.createNodeMesh(node);
      lod.addLevel(highDetail, 0);
    }

    // Medium detail
    if (distance < 50) {
      const mediumGeometry = new THREE.SphereGeometry(0.3, 8, 8);
      const color = this.getNodeColor(node.type);
      const mediumMaterial = new THREE.MeshStandardMaterial({ color });
      const mediumDetail = new THREE.Mesh(mediumGeometry, mediumMaterial);
      mediumDetail.position.set(...node.position);
      lod.addLevel(mediumDetail, 20);
    }

    // Low detail (far)
    const lowGeometry = new THREE.SphereGeometry(0.3, 4, 4);
    const color = this.getNodeColor(node.type);
    const lowMaterial = new THREE.MeshBasicMaterial({ color });
    const lowDetail = new THREE.Mesh(lowGeometry, lowMaterial);
    lowDetail.position.set(...node.position);
    lod.addLevel(lowDetail, 50);

    return lod;
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
