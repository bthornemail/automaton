---
id: implementation-guide
title: "Three.js/WebGL Implementation Guide"
level: practical
type: guide
tags: [implementation, three.js, webgl, visualization, interactive-geometry]
keywords: [three.js, webgl, implementation, visualization, interactive, geometric-rendering]
prerequisites: [platonic-solids, computational-mapping]
enables: [3d-visualization, interactive-geometry]
related: [computational-mapping, bqf-encoding]
readingTime: 80
difficulty: 4
---

# Three.js/WebGL Implementation Guide

## Overview

This guide provides complete implementation examples for rendering regular polyhedra in WebGL using Three.js, including interactive transformations, BQF operations, and consensus visualization.

## Basic Setup

### Scene Initialization

```typescript
import * as THREE from 'three';
import { OrbitControls } from 'three/examples/jsm/controls/OrbitControls';
import { bqfTransformationService } from '../services/bqf-transformation-service';
import type { BQF } from '../services/bqf-transformation-service';

class PolyhedraScene {
  private scene: THREE.Scene;
  private camera: THREE.PerspectiveCamera;
  private renderer: THREE.WebGLRenderer;
  private controls: OrbitControls;
  private polyhedra: Map<string, THREE.Mesh> = new Map();

  constructor(container: HTMLElement) {
    // Scene
    this.scene = new THREE.Scene();
    this.scene.background = new THREE.Color(0x1a1a2e);

    // Camera
    this.camera = new THREE.PerspectiveCamera(
      75,
      container.clientWidth / container.clientHeight,
      0.1,
      1000
    );
    this.camera.position.set(0, 5, 10);

    // Renderer
    this.renderer = new THREE.WebGLRenderer({ antialias: true });
    this.renderer.setSize(container.clientWidth, container.clientHeight);
    container.appendChild(this.renderer.domElement);

    // Controls
    this.controls = new OrbitControls(this.camera, this.renderer.domElement);
    this.controls.enableDamping = true;
    this.controls.dampingFactor = 0.05;

    // Lighting
    const ambientLight = new THREE.AmbientLight(0xffffff, 0.6);
    this.scene.add(ambientLight);

    const directionalLight = new THREE.DirectionalLight(0xffffff, 0.8);
    directionalLight.position.set(5, 10, 5);
    this.scene.add(directionalLight);

    // Animation loop
    this.animate();
  }

  private animate = () => {
    requestAnimationFrame(this.animate);
    this.controls.update();
    this.renderer.render(this.scene, this.camera);
  };
}
```

## Creating Polyhedra

### Tetrahedron

```typescript
createTetrahedron(position: [number, number, number] = [0, 0, 0]): THREE.Mesh {
  const geometry = new THREE.TetrahedronGeometry(1);
  const material = new THREE.MeshPhongMaterial({
    color: 0xff0000,
    wireframe: false,
    transparent: true,
    opacity: 0.8
  });
  
  const mesh = new THREE.Mesh(geometry, material);
  mesh.position.set(...position);
  mesh.userData.bqf = [4, 6, 4] as BQF;
  mesh.userData.type = 'tetrahedron';
  
  this.scene.add(mesh);
  this.polyhedra.set('tetrahedron', mesh);
  
  return mesh;
}
```

### Cube

```typescript
createCube(position: [number, number, number] = [0, 0, 0]): THREE.Mesh {
  const geometry = new THREE.BoxGeometry(1, 1, 1);
  const material = new THREE.MeshPhongMaterial({
    color: 0x00ff00,
    wireframe: false,
    transparent: true,
    opacity: 0.8
  });
  
  const mesh = new THREE.Mesh(geometry, material);
  mesh.position.set(...position);
  mesh.userData.bqf = [8, 12, 6] as BQF;
  mesh.userData.type = 'cube';
  
  this.scene.add(mesh);
  this.polyhedra.set('cube', mesh);
  
  return mesh;
}
```

### Octahedron

```typescript
createOctahedron(position: [number, number, number] = [0, 0, 0]): THREE.Mesh {
  const geometry = new THREE.OctahedronGeometry(1);
  const material = new THREE.MeshPhongMaterial({
    color: 0x0000ff,
    wireframe: false,
    transparent: true,
    opacity: 0.8
  });
  
  const mesh = new THREE.Mesh(geometry, material);
  mesh.position.set(...position);
  mesh.userData.bqf = [6, 12, 8] as BQF;
  mesh.userData.type = 'octahedron';
  
  this.scene.add(mesh);
  this.polyhedra.set('octahedron', mesh);
  
  return mesh;
}
```

### Icosahedron

```typescript
createIcosahedron(position: [number, number, number] = [0, 0, 0]): THREE.Mesh {
  const geometry = new THREE.IcosahedronGeometry(1);
  const material = new THREE.MeshPhongMaterial({
    color: 0xffff00,
    wireframe: false,
    transparent: true,
    opacity: 0.8
  });
  
  const mesh = new THREE.Mesh(geometry, material);
  mesh.position.set(...position);
  mesh.userData.bqf = [12, 30, 20] as BQF;
  mesh.userData.type = 'icosahedron';
  
  this.scene.add(mesh);
  this.polyhedra.set('icosahedron', mesh);
  
  return mesh;
}
```

### Dodecahedron

```typescript
createDodecahedron(position: [number, number, number] = [0, 0, 0]): THREE.Mesh {
  const geometry = new THREE.DodecahedronGeometry(1);
  const material = new THREE.MeshPhongMaterial({
    color: 0xff00ff,
    wireframe: false,
    transparent: true,
    opacity: 0.8
  });
  
  const mesh = new THREE.Mesh(geometry, material);
  mesh.position.set(...position);
  mesh.userData.bqf = [20, 30, 12] as BQF;
  mesh.userData.type = 'dodecahedron';
  
  this.scene.add(mesh);
  this.polyhedra.set('dodecahedron', mesh);
  
  return mesh;
}
```

## BQF Transformations

### Apply BQF Transformation

```typescript
applyBQFTransformation(name: string, operation: 'apply' | 'abstract' | 'dual-swap'): void {
  const mesh = this.polyhedra.get(name);
  if (!mesh) {
    console.warn(`Polyhedron ${name} not found`);
    return;
  }

  const currentBQF = mesh.userData.bqf as BQF;
  let newBQF: BQF;

  switch (operation) {
    case 'apply':
      newBQF = bqfTransformationService.apply(currentBQF);
      this.updateMeshFromBQF(mesh, newBQF, 'apply');
      break;
    case 'abstract':
      newBQF = bqfTransformationService.abstract(currentBQF);
      this.updateMeshFromBQF(mesh, newBQF, 'abstract');
      break;
    case 'dual-swap':
      newBQF = bqfTransformationService.dualSwap(currentBQF);
      this.updateMeshFromBQF(mesh, newBQF, 'dual-swap');
      break;
  }

  mesh.userData.bqf = newBQF;
}

private updateMeshFromBQF(mesh: THREE.Mesh, bqf: BQF, operation: string): void {
  const [a, b, c] = bqf;

  // Update color based on BQF
  const hue = ((a + b + c) % 360) / 360;
  const color = new THREE.Color().setHSL(hue, 0.7, 0.5);
  (mesh.material as THREE.MeshPhongMaterial).color = color;

  // Update scale based on operation
  switch (operation) {
    case 'apply':
      mesh.scale.multiplyScalar(1.1); // Exponential growth
      break;
    case 'abstract':
      mesh.scale.multiplyScalar(0.9); // Linear decay
      break;
    case 'dual-swap':
      // Create dual mesh
      this.createDualMesh(mesh, bqf);
      break;
  }
}
```

### Dual Pair Visualization

```typescript
createDualPair(cubePosition: [number, number, number], octaPosition: [number, number, number]): void {
  const cube = this.createCube(cubePosition);
  const octa = this.createOctahedron(octaPosition);

  // Link dual relationship
  cube.userData.dual = octa;
  octa.userData.dual = cube;

  // Animate dual relationship
  this.animateDualPair(cube, octa);
}

private animateDualPair(cube: THREE.Mesh, octa: THREE.Mesh): void {
  const animate = () => {
    // Cube rotates (affine)
    cube.rotation.x += 0.01;
    cube.rotation.y += 0.01;

    // Octahedron points toward cube (projective constraint)
    octa.lookAt(cube.position);
    
    requestAnimationFrame(animate);
  };
  animate();
}
```

## Interactive Controls

### Mouse Interaction

```typescript
setupMouseInteraction(): void {
  const raycaster = new THREE.Raycaster();
  const mouse = new THREE.Vector2();

  this.renderer.domElement.addEventListener('click', (event) => {
    mouse.x = (event.clientX / window.innerWidth) * 2 - 1;
    mouse.y = -(event.clientY / window.innerHeight) * 2 + 1;

    raycaster.setFromCamera(mouse, this.camera);
    const intersects = raycaster.intersectObjects(Array.from(this.polyhedra.values()));

    if (intersects.length > 0) {
      const mesh = intersects[0].object as THREE.Mesh;
      this.handlePolyhedronClick(mesh);
    }
  });
}

private handlePolyhedronClick(mesh: THREE.Mesh): void {
  const type = mesh.userData.type;
  const bqf = mesh.userData.bqf as BQF;

  console.log(`Clicked ${type} with BQF:`, bqf);

  // Apply dual swap on click
  this.applyBQFTransformation(type, 'dual-swap');
}
```

## Consensus Visualization

### Rotational Consensus

```typescript
visualizeConsensus(polyhedron: THREE.Mesh, consensusValue: number): void {
  // Rotate based on consensus
  const rotationSpeed = consensusValue * 0.01;
  
  const animate = () => {
    polyhedron.rotation.x += rotationSpeed;
    polyhedron.rotation.y += rotationSpeed;
    requestAnimationFrame(animate);
  };
  animate();

  // Change color based on consensus
  const hue = (consensusValue % 360) / 360;
  const color = new THREE.Color().setHSL(hue, 0.8, 0.6);
  (polyhedron.material as THREE.MeshPhongMaterial).color = color;
}
```

## Complete Example

```typescript
// Initialize scene
const container = document.getElementById('polyhedra-container');
const scene = new PolyhedraScene(container!);

// Create all Platonic solids
scene.createTetrahedron([-4, 0, 0]);
scene.createCube([-2, 0, 0]);
scene.createOctahedron([0, 0, 0]);
scene.createIcosahedron([2, 0, 0]);
scene.createDodecahedron([4, 0, 0]);

// Create dual pairs
scene.createDualPair([-2, 2, 0], [0, 2, 0]); // Cube-Octahedron
scene.createDualPair([2, 2, 0], [4, 2, 0]); // Icosahedron-Dodecahedron

// Setup interactions
scene.setupMouseInteraction();

// Apply transformations
setTimeout(() => {
  scene.applyBQFTransformation('cube', 'dual-swap');
}, 2000);
```

## Related Documentation

- **`01-PLATONIC-SOLIDS.md`**: Complete polyhedra specifications
- **`04-COMPUTATIONAL-MAPPING.md`**: Integration patterns
- **`05-BQF-ENCODING.md`**: BQF operations

---

**Last Updated**: 2025-01-07  
**Status**: Complete Implementation Guide

