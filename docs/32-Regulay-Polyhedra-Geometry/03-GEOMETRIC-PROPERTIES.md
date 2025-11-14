---
id: geometric-properties
title: "Geometric Properties of Regular Polyhedra"
level: intermediate
type: reference
tags: [geometric-properties, schläfli-symbols, euler-characteristic, betti-numbers, symmetry-groups]
keywords: [schläfli-symbols, euler-characteristic, betti-numbers, symmetry-groups, dihedral-angles, golden-ratio]
prerequisites: [platonic-solids]
enables: [geometric-calculations, symmetry-analysis]
related: [computational-mapping, bqf-encoding]
readingTime: 40
difficulty: 3
---

# Geometric Properties of Regular Polyhedra

## Overview

This document provides detailed geometric properties of regular polyhedra, including Schläfli symbols, Euler characteristic, Betti numbers, symmetry groups, and computational mappings.

## Schläfli Symbols

Schläfli symbols provide a compact notation for regular polyhedra:

### Format: {p, q}

- **p**: Number of edges per face
- **q**: Number of faces meeting at each vertex

### Platonic Solids

| Solid | Schläfli | p | q | Meaning |
|-------|----------|---|---|---------|
| Tetrahedron | {3,3} | 3 | 3 | 3 edges per face, 3 faces per vertex |
| Cube | {4,3} | 4 | 3 | 4 edges per face, 3 faces per vertex |
| Octahedron | {3,4} | 3 | 4 | 3 edges per face, 4 faces per vertex |
| Icosahedron | {3,5} | 3 | 5 | 3 edges per face, 5 faces per vertex |
| Dodecahedron | {5,3} | 5 | 3 | 5 edges per face, 3 faces per vertex |

## Euler Characteristic

Euler's formula: **V - E + F = 2**

For all regular polyhedra, the Euler characteristic is 2.

### Verification

| Solid | V | E | F | V - E + F |
|-------|---|---|---|------------|
| Tetrahedron | 4 | 6 | 4 | 2 |
| Cube | 8 | 12 | 6 | 2 |
| Octahedron | 6 | 12 | 8 | 2 |
| Icosahedron | 12 | 30 | 20 | 2 |
| Dodecahedron | 20 | 30 | 12 | 2 |

## Betti Numbers

Betti numbers track topological connectivity:

- **β₀**: Connected components (always 1 for polyhedra)
- **β₁**: Cycles/loops (holes in the structure)
- **β₂**: Voids/cavities (enclosed spaces)

### Computational Significance

- **β₀ = 1**: Single connected structure
- **β₁ = 0**: No cycles (simply connected)
- **β₂ = 1**: One enclosed volume

## Symmetry Groups

### Tetrahedral Group (T_d)

- **Order**: 24 elements
- **Polyhedra**: Tetrahedron
- **Operations**: Rotations and reflections

### Octahedral Group (O_h)

- **Order**: 48 elements
- **Polyhedra**: Cube, Octahedron
- **Operations**: Full octahedral symmetry

### Icosahedral Group (I_h)

- **Order**: 120 elements
- **Polyhedra**: Icosahedron, Dodecahedron
- **Operations**: Full icosahedral symmetry

## Euler's Totient Function φ(V)

Euler's totient function φ(V) counts numbers coprime to V:

| V | φ(V) | Computational Meaning |
|---|------|----------------------|
| 4 | 2 | Tetrahedron: Minimal consensus |
| 6 | 2 | Octahedron: Federated consensus |
| 8 | 4 | Cube: Federated consensus |
| 12 | 4 | Icosahedron: Global consensus |
| 20 | 8 | Dodecahedron: Global consensus |

## Inner Dimension (d_inner)

Inner dimension measures structural complexity:

| Solid | d_inner | Meaning |
|-------|---------|---------|
| Tetrahedron | 1.0 | Minimal structure |
| Octahedron | 1.33 | Moderate structure |
| Cube | 1.5 | Balanced structure |
| Dodecahedron | 1.6 | Complex structure |
| Icosahedron | 2.0 | Maximum 3D complexity |

## Dihedral Angles

Dihedral angles between faces:

| Solid | Dihedral Angle | Computational Significance |
|-------|----------------|---------------------------|
| Tetrahedron | ~70.53° | Minimal separation |
| Cube | 90° | Orthogonal constraints |
| Octahedron | ~109.47° | Expanded separation |
| Icosahedron | ~138.19° | Wide separation |
| Dodecahedron | ~116.57° | Pentagonal separation |

## Golden Ratio (Φ) Connections

The golden ratio appears in icosahedron and dodecahedron:

- **Icosahedron**: Edge length ratios involve Φ
- **Dodecahedron**: Face relationships use Φ
- **Computational**: Exponential growth factors

## Computational Mappings

### R5RS Type Mapping

| Solid | R5RS Types | Dimension |
|-------|------------|-----------|
| Tetrahedron | Boolean, Char, Number, Pair | 0D-3D |
| Cube | All 8 types | 3D |
| Octahedron | Projective types | 3D |
| Icosahedron | Extended types | 3D+ |
| Dodecahedron | Complex types | 3D+ |

### BQF Coefficient Mapping

| Solid | BQF [a,b,c] | a (affine) | b (interaction) | c (projective) |
|-------|-------------|------------|-----------------|----------------|
| Tetrahedron | [4,6,4] | 4 points | 6 lines | 4 planes |
| Cube | [8,12,6] | 8 points | 12 lines | 6 planes |
| Octahedron | [6,12,8] | 6 points | 12 lines | 8 planes |
| Icosahedron | [12,30,20] | 12 points | 30 lines | 20 planes |
| Dodecahedron | [20,30,12] | 20 points | 30 lines | 12 planes |

## Related Documentation

- **`01-PLATONIC-SOLIDS.md`**: Complete polyhedra specifications
- **`04-COMPUTATIONAL-MAPPING.md`**: Integration with computational system
- **`05-BQF-ENCODING.md`**: Detailed BQF encoding

---

**Last Updated**: 2025-01-07  
**Status**: Complete Reference

