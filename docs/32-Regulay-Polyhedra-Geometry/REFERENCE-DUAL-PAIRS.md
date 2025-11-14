---
id: reference-dual-pairs
title: "Dual Pairs Quick Reference"
level: reference
type: reference
tags: [dual-pairs, quick-reference, geometric-duality]
keywords: [dual-pairs, duality, quick-reference, transformations]
prerequisites: []
enables: [quick-lookup, dual-transformations]
related: [platonic-solids, bqf-encoding]
readingTime: 10
difficulty: 1
---

# Dual Pairs Quick Reference

## 3D Dual Pairs

| Solid | Dual | BQF | Dual BQF |
|-------|------|-----|----------|
| Cube | Octahedron | [8,12,6] | [6,12,8] |
| Octahedron | Cube | [6,12,8] | [8,12,6] |
| Icosahedron | Dodecahedron | [12,30,20] | [20,30,12] |
| Dodecahedron | Icosahedron | [20,30,12] | [12,30,20] |
| Tetrahedron | Tetrahedron (self-dual) | [4,6,4] | [4,6,4] |

## Dual Swap Operation

```scheme
;; Dual swap: [a,b,c] → [c,b,a]
(dual-swap '(8 12 6))  ; Cube → (6 12 8) Octahedron
(dual-swap '(6 12 8))  ; Octahedron → (8 12 6) Cube
```

```typescript
// TypeScript
const cubeBQF: BQF = [8, 12, 6];
const octaBQF = bqfTransformationService.dualSwap(cubeBQF);
// → [6, 12, 8]
```

## Related Documentation

- **`01-PLATONIC-SOLIDS.md`**: Complete polyhedra specifications
- **`05-BQF-ENCODING.md`**: BQF transformation operations
- **`07-CONSTRAINT-POINTERS.md`**: Constraint mechanisms

---

**Last Updated**: 2025-01-07

