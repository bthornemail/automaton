---
id: platonic-solids
title: "Platonic Solids - Complete Specification"
level: foundational
type: specification
tags: [platonic-solids, regular-polyhedra, geometric-structures, 3d-geometry]
keywords: [tetrahedron, cube, octahedron, icosahedron, dodecahedron, schläfli-symbols, euler-characteristic]
prerequisites: [dimensional-geometric-mapping]
enables: [geometric-reasoning, consensus-patterns, constraint-pointers]
related: [dual-pairs-isomorphisms, archimedean-solids]
readingTime: 60
difficulty: 3
---

# Platonic Solids - Complete Specification

## Overview

The five Platonic solids are the only regular polyhedra in 3D space. They serve as geometric foundations for computational structures, consensus patterns, and constraint mechanisms.

## The Five Platonic Solids

### 1. Tetrahedron (Self-Dual)

**Geometric Properties**:
- **Vertices**: 4
- **Edges**: 6
- **Faces**: 4 (triangles)
- **Schläfli Symbol**: {3,3}
- **Euler Characteristic**: V - E + F = 4 - 6 + 4 = 2
- **Symmetry Group**: T_d (tetrahedral group, 24 elements)

**Computational Properties**:
- **Type**: Self-dual (vertices ↔ faces)
- **Consensus Pattern**: Minimal consensus (4 equivalent points)
- **Euler's φ(V)**: φ(4) = 2
- **Inner Dimension**: d_inner = 1.0
- **BQF Encoding**: [4, 6, 4]

**R5RS Mapping**:
- Maps to minimal 4-tuple: Boolean, Char, Number, Pair
- Self-consistency: All views agree (no asymmetry)
- Consensus: Rotational invariance = distributed agreement

**Implementation**:
```scheme
;; Tetrahedron BQF
(define tetra-bqf '(4 6 4))

;; Self-dual check
(equal? tetra-bqf (dual-swap tetra-bqf))  ; → #t
```

```typescript
import * as THREE from 'three';

const geometry = new THREE.TetrahedronGeometry(1);
const material = new THREE.MeshBasicMaterial({ 
  color: 0xff0000, 
  wireframe: true 
});
const tetrahedron = new THREE.Mesh(geometry, material);
```

### 2. Cube

**Geometric Properties**:
- **Vertices**: 8
- **Edges**: 12
- **Faces**: 6 (squares)
- **Schläfli Symbol**: {4,3}
- **Euler Characteristic**: V - E + F = 8 - 12 + 6 = 2
- **Symmetry Group**: O_h (octahedral group, 48 elements)

**Computational Properties**:
- **Type**: Dual pair (with Octahedron)
- **Consensus Pattern**: Federated consensus (τ = 0.50)
- **Euler's φ(V)**: φ(8) = 4
- **Inner Dimension**: d_inner = 1.5
- **BQF Encoding**: [8, 12, 6]

**R5RS Mapping**:
- Maps to 8-tuple: Boolean, Pair, Symbol, Number, Char, String, Vector, Procedure
- Affine space: Vertices as data points
- Consensus: 8 vertices = 8 types hashing to consensus

**Dual Relationship**:
- Cube vertices ↔ Octahedron faces
- Cube faces ↔ Octahedron vertices
- Asymmetry creates constraint pointers

**Implementation**:
```scheme
;; Cube BQF
(define cube-bqf '(8 12 6))

;; Dual swap to octahedron
(dual-swap cube-bqf)  ; → (6 12 8)
```

```typescript
const geometry = new THREE.BoxGeometry(1, 1, 1);
const material = new THREE.MeshBasicMaterial({ 
  color: 0x00ff00, 
  wireframe: true 
});
const cube = new THREE.Mesh(geometry, material);
```

### 3. Octahedron

**Geometric Properties**:
- **Vertices**: 6
- **Edges**: 12
- **Faces**: 8 (triangles)
- **Schläfli Symbol**: {3,4}
- **Euler Characteristic**: V - E + F = 6 - 12 + 8 = 2
- **Symmetry Group**: O_h (octahedral group, 48 elements)

**Computational Properties**:
- **Type**: Dual pair (with Cube)
- **Consensus Pattern**: Federated consensus (τ = 0.50)
- **Euler's φ(V)**: φ(6) = 2
- **Inner Dimension**: d_inner = 1.33
- **BQF Encoding**: [6, 12, 8]

**R5RS Mapping**:
- Projective space: Faces as functional lines
- Constraint pointer: Triangular faces point directions
- Dual asymmetry: Points from affine (cube) to projective (octa)

**Implementation**:
```scheme
;; Octahedron BQF
(define octa-bqf '(6 12 8))

;; Dual swap to cube
(dual-swap octa-bqf)  ; → (8 12 6)
```

```typescript
const geometry = new THREE.OctahedronGeometry(1);
const material = new THREE.MeshBasicMaterial({ 
  color: 0x0000ff, 
  wireframe: true 
});
const octahedron = new THREE.Mesh(geometry, material);
```

### 4. Icosahedron

**Geometric Properties**:
- **Vertices**: 12
- **Edges**: 30
- **Faces**: 20 (triangles)
- **Schläfli Symbol**: {3,5}
- **Euler Characteristic**: V - E + F = 12 - 30 + 20 = 2
- **Symmetry Group**: I_h (icosahedral group, 120 elements)

**Computational Properties**:
- **Type**: Dual pair (with Dodecahedron)
- **Consensus Pattern**: Global consensus (τ = 0.25)
- **Euler's φ(V)**: φ(12) = 4
- **Inner Dimension**: d_inner = 2.0
- **BQF Encoding**: [12, 30, 20]

**R5RS Mapping**:
- Higher complexity: 12 vertices for scaled consensus
- Triangular faces: Exponential action bifurcation
- Constraint fan-out: Pentagonal dual creates directional constraints

**Implementation**:
```scheme
;; Icosahedron BQF
(define icosa-bqf '(12 30 20))

;; Dual swap to dodecahedron
(dual-swap icosa-bqf)  ; → (20 30 12)
```

```typescript
const geometry = new THREE.IcosahedronGeometry(1);
const material = new THREE.MeshBasicMaterial({ 
  color: 0xffff00, 
  wireframe: true 
});
const icosahedron = new THREE.Mesh(geometry, material);
```

### 5. Dodecahedron

**Geometric Properties**:
- **Vertices**: 20
- **Edges**: 30
- **Faces**: 12 (pentagons)
- **Schläfli Symbol**: {5,3}
- **Euler Characteristic**: V - E + F = 20 - 30 + 12 = 2
- **Symmetry Group**: I_h (icosahedral group, 120 elements)

**Computational Properties**:
- **Type**: Dual pair (with Icosahedron)
- **Consensus Pattern**: Global consensus (τ = 0.25)
- **Euler's φ(V)**: φ(20) = 8
- **Inner Dimension**: d_inner = 1.6
- **BQF Encoding**: [20, 30, 12]

**R5RS Mapping**:
- Highest complexity: 20 vertices for complex consensus
- Pentagonal faces: Stable constraint structures
- Dual relationship: Creates exponential transformation patterns

**Implementation**:
```scheme
;; Dodecahedron BQF
(define dodeca-bqf '(20 30 12))

;; Dual swap to icosahedron
(dual-swap dodeca-bqf)  ; → (12 30 20)
```

```typescript
const geometry = new THREE.DodecahedronGeometry(1);
const material = new THREE.MeshBasicMaterial({ 
  color: 0xff00ff, 
  wireframe: true 
});
const dodecahedron = new THREE.Mesh(geometry, material);
```

## Summary Table

| Solid | V | E | F | Schläfli | BQF | Dual | Consensus τ |
|-------|---|---|---|----------|-----|------|-------------|
| Tetrahedron | 4 | 6 | 4 | {3,3} | [4,6,4] | Self | 0.75 |
| Cube | 8 | 12 | 6 | {4,3} | [8,12,6] | Octahedron | 0.50 |
| Octahedron | 6 | 12 | 8 | {3,4} | [6,12,8] | Cube | 0.50 |
| Icosahedron | 12 | 30 | 20 | {3,5} | [12,30,20] | Dodecahedron | 0.25 |
| Dodecahedron | 20 | 30 | 12 | {5,3} | [20,30,12] | Icosahedron | 0.25 |

## Computational Applications

### Consensus Patterns

- **Tetrahedron**: Minimal consensus (4 points agree)
- **Cube/Octahedron**: Federated consensus (8/6 points agree)
- **Icosahedron/Dodecahedron**: Global consensus (12/20 points agree)

### Constraint Pointers

- **Dual pairs**: Create directional constraints
- **Asymmetry**: Points from affine to projective space
- **BQF transformations**: Enable geometric transformations

### Dimensional Transformers

- **Self-dual (Tetrahedron)**: Stable transformation base
- **Dual pairs**: Enable 3D → 4D lifts via BQF composition
- **Higher dimensions**: Connect to 4D polytopes (5-cell, 24-cell, 120-cell, 600-cell)

## Related Documentation

- **`02-ARCHIMEDEAN-SOLIDS.md`**: Quasi-regular polyhedra
- **`03-GEOMETRIC-PROPERTIES.md`**: Detailed geometric properties
- **`04-COMPUTATIONAL-MAPPING.md`**: Integration with computational system

---

**Last Updated**: 2025-01-07  
**Status**: Complete Specification

