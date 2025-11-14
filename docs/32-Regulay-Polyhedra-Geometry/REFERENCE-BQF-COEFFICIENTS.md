---
id: reference-bqf-coefficients
title: "BQF Coefficients Quick Reference"
level: reference
type: reference
tags: [bqf-coefficients, quick-reference, binary-quadratic-forms]
keywords: [bqf, coefficients, quick-reference, vertices-edges-faces]
prerequisites: []
enables: [quick-lookup, bqf-operations]
related: [bqf-encoding, platonic-solids]
readingTime: 10
difficulty: 1
---

# BQF Coefficients Quick Reference

## BQF Format

**[a, b, c]** where:
- **a**: Vertices (affine points)
- **b**: Edges (interaction lines)
- **c**: Faces (projective planes)

## Complete BQF Table

| Polyhedron | BQF | Vertices | Edges | Faces |
|------------|-----|----------|-------|-------|
| Tetrahedron | [4,6,4] | 4 | 6 | 4 |
| Cube | [8,12,6] | 8 | 12 | 6 |
| Octahedron | [6,12,8] | 6 | 12 | 8 |
| Icosahedron | [12,30,20] | 12 | 30 | 20 |
| Dodecahedron | [20,30,12] | 20 | 30 | 12 |

## Transformations

### Apply (Forward)
```
[8,12,6] → [8,12,5]  (c-1)
```

### Abstract (Backward)
```
[6,12,8] → [6,12,9]  (c+1)
```

### Dual Swap
```
[8,12,6] → [6,12,8]  ([a,b,c] → [c,b,a])
```

## Related Documentation

- **`05-BQF-ENCODING.md`**: Detailed BQF encoding
- **`01-PLATONIC-SOLIDS.md`**: Complete polyhedra specifications

---

**Last Updated**: 2025-01-07

