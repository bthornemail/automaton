---
id: archimedean-solids
title: "Archimedean Solids - Constraint Mechanisms"
level: intermediate
type: specification
tags: [archimedean-solids, quasi-regular-polyhedra, constraint-mechanisms, consensus-patterns]
keywords: [archimedean-solids, cuboctahedron, rhombicuboctahedron, constraint-pointers, quasi-symmetry]
prerequisites: [platonic-solids]
enables: [constraint-mechanisms, expanded-consensus]
related: [platonic-solids, geometric-properties]
readingTime: 50
difficulty: 4
---

# Archimedean Solids - Constraint Mechanisms

## Overview

The 13 Archimedean solids are quasi-regular polyhedra that bridge Platonic purity with expanded symmetry. They serve as constraint mechanisms and expanded consensus patterns in the computational system.

## Key Archimedean Solids

### Archimedes 6: Cuboctahedron

**Geometric Properties**:
- **Vertices**: 12
- **Edges**: 24
- **Faces**: 14 (8 triangles + 6 squares)
- **Schläfli Symbol**: Quasi-regular
- **Symmetry**: Blends cube/octahedron dual

**Computational Properties**:
- **Type**: Consensus pattern (balanced expansion)
- **Consensus**: 6 squares for stable agreement, 8 triangles for multi-view consensus
- **BQF Encoding**: [12, 24, 14]
- **Application**: Models 8-tuple as midpoints (vector averages for centroids)

**Implementation**:
```scheme
;; Cuboctahedron consensus (quasi-symmetric GCD/LCM blend)
(define (arch6-consensus facts rules)
  (lcm (gcd facts) rules))  ; Quasi-consensus: Blend affine/projective
```

```typescript
// Create cuboctahedron geometry (approximation)
const geometry = new THREE.BufferGeometry();
// ... vertex/face data for cuboctahedron
const material = new THREE.MeshBasicMaterial({ 
  color: 0x00ffff, 
  wireframe: true 
});
const cuboctahedron = new THREE.Mesh(geometry, material);
```

### Archimedes 7: Rhombicuboctahedron

**Geometric Properties**:
- **Vertices**: 24
- **Edges**: 48
- **Faces**: 26 (8 triangles + 18 squares)
- **Schläfli Symbol**: Expanded quasi-regular
- **Symmetry**: Expanded form with rhombic twists

**Computational Properties**:
- **Type**: Constraint pointer (expanded symmetry)
- **Constraint**: Rhombic twists create directional pointers
- **BQF Encoding**: [24, 48, 26]
- **Application**: Constraint mechanisms via expanded symmetry

**Implementation**:
```scheme
;; Rhombicuboctahedron constraint pointer
(define (arch7-constraint direction)
  (apply-bqf '(24 48 26)))  ; Forward transformation for constraints
```

## Complete List of 13 Archimedean Solids

1. **Truncated Tetrahedron**: 12 vertices, 18 edges, 8 faces (4 triangles + 4 hexagons)
2. **Cuboctahedron** (Archimedes 6): 12 vertices, 24 edges, 14 faces (8 triangles + 6 squares)
3. **Truncated Cube**: 24 vertices, 36 edges, 14 faces (8 triangles + 6 octagons)
4. **Truncated Octahedron**: 24 vertices, 36 edges, 14 faces (6 squares + 8 hexagons)
5. **Rhombicuboctahedron** (Archimedes 7): 24 vertices, 48 edges, 26 faces (8 triangles + 18 squares)
6. **Truncated Cuboctahedron**: 48 vertices, 72 edges, 26 faces (12 squares + 8 hexagons + 6 octagons)
7. **Snub Cube**: 24 vertices, 60 edges, 38 faces (32 triangles + 6 squares)
8. **Icosidodecahedron**: 30 vertices, 60 edges, 32 faces (20 triangles + 12 pentagons)
9. **Truncated Icosahedron**: 60 vertices, 90 edges, 32 faces (12 pentagons + 20 hexagons)
10. **Truncated Dodecahedron**: 60 vertices, 90 edges, 32 faces (20 triangles + 12 decagons)
11. **Rhombicosidodecahedron**: 60 vertices, 120 edges, 62 faces (20 triangles + 30 squares + 12 pentagons)
12. **Truncated Icosidodecahedron**: 120 vertices, 180 edges, 62 faces (30 squares + 20 hexagons + 12 decagons)
13. **Snub Dodecahedron**: 60 vertices, 150 edges, 92 faces (80 triangles + 12 pentagons)

## Extended 7 Shapes (Including Snubs)

The "extended 7" includes the snub polyhedra as additional constraint mechanisms:

- **Snub Cube**: Chiral constraint mechanism
- **Snub Dodecahedron**: Complex chiral constraint

## Computational Applications

### Consensus Patterns

- **Archimedes 6 (Cuboctahedron)**: Balanced consensus via quasi-symmetry
- **Archimedes 7 (Rhombicuboctahedron)**: Expanded consensus with constraint pointers

### Constraint Mechanisms

- **Quasi-regularity**: Creates directional constraints
- **Rhombic twists**: Enable constraint pointers
- **Snub forms**: Chiral constraint mechanisms

### Integration with Platonic Solids

- **Bridge structure**: Connects Platonic purity to expanded forms
- **BQF transformations**: Enable geometric transitions
- **Dimensional lifts**: Support 3D → 4D transformations

## BQF Encoding Table

| Solid | V | E | F | BQF | Application |
|-------|---|---|---|-----|-------------|
| Cuboctahedron | 12 | 24 | 14 | [12,24,14] | Consensus |
| Rhombicuboctahedron | 24 | 48 | 26 | [24,48,26] | Constraints |
| Snub Cube | 24 | 60 | 38 | [24,60,38] | Chiral constraints |
| Icosidodecahedron | 30 | 60 | 32 | [30,60,32] | Expanded consensus |
| Snub Dodecahedron | 60 | 150 | 92 | [60,150,92] | Complex constraints |

## Related Documentation

- **`01-PLATONIC-SOLIDS.md`**: Foundation regular polyhedra
- **`07-CONSTRAINT-POINTERS.md`**: Detailed constraint mechanisms
- **`06-CONSENSUS-PATTERNS.md`**: Consensus pattern applications

---

**Last Updated**: 2025-01-07  
**Status**: Complete Specification

