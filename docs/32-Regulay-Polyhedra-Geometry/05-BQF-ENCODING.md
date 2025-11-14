---
id: bqf-encoding
title: "BQF Encoding of Regular Polyhedra"
level: intermediate
type: specification
tags: [bqf-encoding, binary-quadratic-forms, geometric-encoding, transformations]
keywords: [bqf, binary-quadratic-forms, geometric-encoding, apply-bqf, abstract-bqf, dual-swap]
prerequisites: [platonic-solids, computational-mapping]
enables: [geometric-transformations, bqf-operations]
related: [bipartite-bqf-extension, computational-mapping]
readingTime: 50
difficulty: 4
---

# BQF Encoding of Regular Polyhedra

## Overview

Binary Quadratic Forms (BQF) provide a mathematical encoding of regular polyhedra that enables geometric transformations and computational operations.

## BQF Structure

### Format: [a, b, c]

- **a**: Affine points (binary, discrete, values/facts) = Vertices
- **b**: Interaction lines (shared bipartitely, ports/hashes) = Edges
- **c**: Projective planes (float, continuous, functions/rules) = Faces

### Complete BQF Table

| Polyhedron | BQF | Vertices (a) | Edges (b) | Faces (c) |
|------------|-----|---------------|-----------|-----------|
| Tetrahedron | [4,6,4] | 4 | 6 | 4 |
| Cube | [8,12,6] | 8 | 12 | 6 |
| Octahedron | [6,12,8] | 6 | 12 | 8 |
| Icosahedron | [12,30,20] | 12 | 30 | 20 |
| Dodecahedron | [20,30,12] | 20 | 30 | 12 |

## BQF Transformations

### 1. Apply BQF (Forward, Exponential)

**Direction**: Affine → Projective  
**Effect**: Forward propagation, exponential growth  
**Operation**: [a, b, c] → [a, b, c-1]

```scheme
;; Apply BQF transformation
(apply-bqf '(8 12 6))  ; Cube → [8, 12, 5]
(apply-bqf '(6 12 8))   ; Octahedron → [6, 12, 7]
```

```typescript
// TypeScript implementation
const cubeBQF: BQF = [8, 12, 6];
const applied = bqfTransformationService.apply(cubeBQF);
// → [8, 12, 5]
```

### 2. Abstract BQF (Backward, Linear)

**Direction**: Projective → Affine  
**Effect**: Backward propagation, linear collapse  
**Operation**: [a, b, c] → [a, b, c+1]

```scheme
;; Abstract BQF transformation
(abstract-bqf '(8 12 6))  ; Cube → [8, 12, 7]
(abstract-bqf '(6 12 8))   ; Octahedron → [6, 12, 9]
```

```typescript
// TypeScript implementation
const octaBQF: BQF = [6, 12, 8];
const abstracted = bqfTransformationService.abstract(octaBQF);
// → [6, 12, 9]
```

### 3. Dual Swap

**Effect**: Geometric inversion  
**Operation**: [a, b, c] → [c, b, a]

```scheme
;; Dual swap transformations
(dual-swap '(8 12 6))   ; Cube → [6, 12, 8] (Octahedron)
(dual-swap '(6 12 8))   ; Octahedron → [8, 12, 6] (Cube)
(dual-swap '(12 30 20)) ; Icosahedron → [20, 30, 12] (Dodecahedron)
(dual-swap '(20 30 12)) ; Dodecahedron → [12, 30, 20] (Icosahedron)
(dual-swap '(4 6 4))    ; Tetrahedron → [4, 6, 4] (Self-dual)
```

```typescript
// TypeScript implementation
const cubeBQF: BQF = [8, 12, 6];
const dual = bqfTransformationService.dualSwap(cubeBQF);
// → [6, 12, 8] (Octahedron)
```

### 4. Compose BQF

**Effect**: Sequential composition  
**Operation**: Q₁ ∘ Q₂ (matrix multiplication)

```scheme
;; Compose two BQF transformations
(compose-bqf '(8 12 6) '(6 12 8))  ; Cube ∘ Octahedron
;; → [x² coefficient, xy coefficient, y² coefficient]
```

```typescript
// TypeScript implementation
const q1: BQF = [8, 12, 6];
const q2: BQF = [6, 12, 8];
const composed = bqfTransformationService.compose(q1, q2);
// → [composed BQF]
```

## Self-Dual Verification

### Tetrahedron Self-Dual

```scheme
;; Tetrahedron is self-dual
(define tetra-bqf '(4 6 4))
(equal? tetra-bqf (dual-swap tetra-bqf))  ; → #t
```

```typescript
// TypeScript verification
const tetraBQF: BQF = [4, 6, 4];
const isSelfDual = bqfTransformationService.isSelfDual(tetraBQF);
// → true
```

## Dual Pair Relationships

### Cube ↔ Octahedron

```scheme
;; Cube and Octahedron are duals
(define cube-bqf '(8 12 6))
(define octa-bqf '(6 12 8))

(equal? (dual-swap cube-bqf) octa-bqf)  ; → #t
(equal? (dual-swap octa-bqf) cube-bqf)  ; → #t
```

### Icosahedron ↔ Dodecahedron

```scheme
;; Icosahedron and Dodecahedron are duals
(define icosa-bqf '(12 30 20))
(define dodeca-bqf '(20 30 12))

(equal? (dual-swap icosa-bqf) dodeca-bqf)  ; → #t
(equal? (dual-swap dodeca-bqf) icosa-bqf)  ; → #t
```

## BQF Validation

### Valid BQF Checks

```scheme
;; Check if BQF can be applied
(> (caddr '(8 12 6)) 0)  ; → #t (c > 0)

;; Check if self-dual
(equal? (car bqf) (caddr bqf))  ; → #t for tetrahedron
```

```typescript
// TypeScript validation
const bqf: BQF = [8, 12, 6];
const canApply = bqfTransformationService.canApply(bqf);
// → true (c = 6 > 0)

const isSelfDual = bqfTransformationService.isSelfDual(bqf);
// → false (8 ≠ 6)
```

## Computational Applications

### Consensus Patterns

- **Tetrahedron [4,6,4]**: Minimal consensus (4 points)
- **Cube [8,12,6]**: Federated consensus (8 points)
- **Icosahedron [12,30,20]**: Global consensus (12 points)

### Constraint Pointers

- **Dual swap**: Creates constraint direction
- **Apply/Abstract**: Forward/backward transformations
- **Compose**: Sequential constraint chains

### Dimensional Transformers

- **BQF composition**: Enables 3D → 4D lifts
- **Self-dual**: Stable transformation base
- **Dual pairs**: Enable geometric inversions

## Related Documentation

- **`01-PLATONIC-SOLIDS.md`**: Complete polyhedra specifications
- **`04-COMPUTATIONAL-MAPPING.md`**: Integration with computational system
- **`docs/28-Canvasl-Frontmatter-Knowledge-Model/01-BIPARTITE-BQF-EXTENSION-RFC2119.md`**: Bipartite-BQF specification

---

**Last Updated**: 2025-01-07  
**Status**: Complete Specification

