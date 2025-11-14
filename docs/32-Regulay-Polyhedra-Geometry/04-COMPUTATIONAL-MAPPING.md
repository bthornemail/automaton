---
id: computational-mapping
title: "Computational Mapping of Regular Polyhedra"
level: intermediate
type: guide
tags: [computational-mapping, r5rs-types, bipartite-bqf, vector-clocks, consensus-patterns]
keywords: [r5rs-mapping, type-system, bipartite-bqf, vector-clocks, consensus, constraints]
prerequisites: [platonic-solids, geometric-properties]
enables: [geometric-reasoning, system-integration]
related: [bqf-encoding, consensus-patterns, constraint-pointers]
readingTime: 70
difficulty: 4
---

# Computational Mapping of Regular Polyhedra

## Overview

This document describes how regular polyhedra map to computational structures in the automaton system, including R5RS types, Bipartite-BQF encoding, vector clocks, and consensus patterns.

## R5RS Type System Mapping

### 8-Tuple to Polyhedra Vertices

The 8-tuple R5RS types map to polyhedra vertices:

```
8-Tuple: [Boolean, Pair, Symbol, Number, Char, String, Vector, Procedure]
         ↓
Cube: 8 vertices = 8 types
```

### Type Classification

- **Binary Types (Affine)**: Boolean, Char, Number, Pair → Cube vertices
- **Float Types (Projective)**: String, Vector, Procedure → Octahedron faces

### Dimensional Mapping

| Type | Dimension | Polyhedron | BQF |
|------|-----------|------------|-----|
| Boolean | 0D | Point | [1,0,0] |
| Char | 1D | Line | [2,1,0] |
| Number | 2D | Plane | [4,4,1] |
| Pair | 3D | Tetrahedron | [4,6,4] |
| String | 4D | Cube | [8,12,6] |
| Vector | 5D | Octahedron | [6,12,8] |
| Procedure | 6D | Icosahedron | [12,30,20] |

## Bipartite-BQF Encoding

### BQF Structure

BQF = [a, b, c] where:
- **a**: Affine points (binary, discrete, values/facts)
- **b**: Interaction lines (shared bipartitely, ports/hashes)
- **c**: Projective planes (float, continuous, functions/rules)

### Polyhedra BQF Encoding

| Polyhedron | BQF | a (vertices) | b (edges) | c (faces) |
|------------|-----|--------------|-----------|-----------|
| Tetrahedron | [4,6,4] | 4 points | 6 lines | 4 planes |
| Cube | [8,12,6] | 8 points | 12 lines | 6 planes |
| Octahedron | [6,12,8] | 6 points | 12 lines | 8 planes |
| Icosahedron | [12,30,20] | 12 points | 30 lines | 20 planes |
| Dodecahedron | [20,30,12] | 20 points | 30 lines | 12 planes |

### BQF Transformations

```scheme
;; Apply BQF (forward, exponential)
(apply-bqf '(8 12 6))  ; Cube → [8, 12, 5]

;; Abstract BQF (backward, linear)
(abstract-bqf '(6 12 8))  ; Octahedron → [6, 12, 9]

;; Dual swap
(dual-swap '(8 12 6))  ; Cube → [6, 12, 8] (Octahedron)
```

## Vector Clock Integration

### Causal Ordering via Geometry

Vector clocks track causal history using geometric structures:

```typescript
// Create vector clock from polyhedron metadata
const vectorClock = vectorClockService.create(
  'cube.jsonl',      // file
  42,                // line
  Date.now(),        // timestamp
  'cube-consensus'   // pattern
);

// Merge vector clocks for dual pairs
const cubeVC = vectorClockService.create('cube.jsonl', 42, t1, 'cube');
const octaVC = vectorClockService.create('octa.jsonl', 43, t2, 'octa');
const merged = vectorClockService.merge(cubeVC, octaVC);
```

### Geometric Causality

- **Tetrahedron**: Minimal causality (4 components)
- **Cube/Octahedron**: Federated causality (8/6 components)
- **Icosahedron/Dodecahedron**: Global causality (12/20 components)

## Consensus Patterns

### Symmetry-Based Consensus

- **Tetrahedron**: Rotational invariance = 4-point agreement
- **Cube**: O_h symmetry = 8-point federated consensus
- **Icosahedron**: I_h symmetry = 12-point global consensus

### Implementation

```scheme
;; Tetrahedron consensus (4-point agreement)
(define (tetra-consensus facts)
  (if (= (length facts) 4)
      (reduce gcd facts)  ; Affine consensus (GCD)
      (error "Need 4 facts")))

;; Cube consensus (8-point federated)
(define (cube-consensus types)
  (if (= (length types) 8)
      (apply lcm types)  ; Projective consensus (LCM)
      (error "Need 8 types")))
```

## Constraint Pointers

### Dual Pair Constraints

- **Cube → Octahedron**: Affine to projective constraint
- **Icosahedron → Dodecahedron**: Exponential constraint fan-out

### BQF Constraint Transformation

```scheme
;; Constraint pointer via dual swap
(define (create-constraint bqf)
  (dual-swap bqf))  ; Swap affine ↔ projective

;; Apply constraint
(define cube-constraint (create-constraint '(8 12 6)))
;; → (6 12 8) = Octahedron constraint
```

## Integration with Provenance System

### Node Metadata

```typescript
interface ProvenanceNode {
  id: string;
  type: 'agent' | 'document' | 'code';
  position: [number, number, number];
  metadata: {
    polyhedron?: 'tetrahedron' | 'cube' | 'octahedron' | 'icosahedron' | 'dodecahedron';
    bqf?: [number, number, number];
    vectorClock?: VectorClock;
    consensus?: number;  // τ value
  };
}
```

### Edge Relationships

```typescript
interface ProvenanceEdge {
  id: string;
  type: 'consumes' | 'produces' | 'references' | 'evolves';
  from: string;
  to: string;
  metadata: {
    dualPair?: 'cube-octahedron' | 'icosahedron-dodecahedron';
    bqfTransformation?: 'apply' | 'abstract' | 'dual-swap';
  };
}
```

## WebGL Visualization

### Three.js Integration

```typescript
import * as THREE from 'three';
import { bqfTransformationService } from '../services/bqf-transformation-service';

class PolyhedraVisualization {
  private scene: THREE.Scene;
  private polyhedra: Map<string, THREE.Mesh> = new Map();

  createPolyhedron(name: string, bqf: [number, number, number]) {
    let geometry: THREE.BufferGeometry;
    
    switch (name) {
      case 'tetrahedron':
        geometry = new THREE.TetrahedronGeometry(1);
        break;
      case 'cube':
        geometry = new THREE.BoxGeometry(1, 1, 1);
        break;
      case 'octahedron':
        geometry = new THREE.OctahedronGeometry(1);
        break;
      case 'icosahedron':
        geometry = new THREE.IcosahedronGeometry(1);
        break;
      case 'dodecahedron':
        geometry = new THREE.DodecahedronGeometry(1);
        break;
    }

    const material = new THREE.MeshBasicMaterial({ 
      wireframe: true,
      color: this.getColorForBQF(bqf)
    });
    
    const mesh = new THREE.Mesh(geometry, material);
    this.scene.add(mesh);
    this.polyhedra.set(name, mesh);
    
    return mesh;
  }

  applyBQFTransformation(name: string, operation: 'apply' | 'abstract' | 'dual-swap') {
    const mesh = this.polyhedra.get(name);
    if (!mesh) return;

    // Get current BQF from metadata
    const currentBQF = mesh.userData.bqf as [number, number, number];
    
    let newBQF: [number, number, number];
    switch (operation) {
      case 'apply':
        newBQF = bqfTransformationService.apply(currentBQF);
        break;
      case 'abstract':
        newBQF = bqfTransformationService.abstract(currentBQF);
        break;
      case 'dual-swap':
        newBQF = bqfTransformationService.dualSwap(currentBQF);
        break;
    }

    // Update mesh based on new BQF
    mesh.userData.bqf = newBQF;
    this.updateMeshFromBQF(mesh, newBQF);
  }

  private getColorForBQF(bqf: [number, number, number]): number {
    // Color based on BQF coefficients
    const [a, b, c] = bqf;
    const hue = (a + b + c) % 360;
    return new THREE.Color().setHSL(hue / 360, 0.7, 0.5).getHex();
  }
}
```

## Related Documentation

- **`01-PLATONIC-SOLIDS.md`**: Complete polyhedra specifications
- **`05-BQF-ENCODING.md`**: Detailed BQF encoding
- **`06-CONSENSUS-PATTERNS.md`**: Consensus pattern applications
- **`08-IMPLEMENTATION-GUIDE.md`**: Complete implementation examples

---

**Last Updated**: 2025-01-07  
**Status**: Complete Guide

