---
id: canvasl-translation-quick-reference
title: "CanvasL M/S-Expression Translation Quick Reference"
level: practical
type: guide
tags: [canvasl, quick-reference, translation, m-expressions, s-expressions]
keywords: [canvasl, translation, m-expression, s-expression, quick-reference]
prerequisites: [canvasl-m-s-expression-grammar]
enables: [canvasl-translation]
related: [canvasl-m-s-expression-grammar, canvasl-rfc2119-spec]
readingTime: 10
difficulty: 2
---

# CanvasL M/S-Expression Translation Quick Reference

**Quick reference for translating between M-expressions, S-expressions, and CanvasL**

## Translation Cheat Sheet

### Basic Operations

| M-Expression | S-Expression | CanvasL |
|--------------|--------------|---------|
| `x = 42` | `(define x 42)` | `{"id": "x", "type": "node", "value": 42}` |
| `f(x) = x + 1` | `(lambda (x) (+ x 1))` | `{"type": "r5rs-call", "function": "r5rs:lambda", "args": [["x"], {"function": "r5rs:add", "args": ["x", 1]}]}` |
| `result = f(5)` | `(f 5)` | `{"type": "r5rs-call", "function": "r5rs:apply", "args": ["#f", 5]}` |

### BQF Operations

| M-Expression | S-Expression | CanvasL |
|--------------|--------------|---------|
| `bqf = [8, 12, 6]` | `(define bqf '(8 12 6))` | `{"id": "bqf", "bqf": [8, 12, 6]}` |
| `dual = dual_swap(bqf)` | `(dual-swap bqf)` | `{"type": "r5rs-call", "function": "r5rs:dual-swap", "args": [{"fromNode": "#bqf", "property": "bqf"}]}` |
| `applied = apply_bqf(bqf)` | `(apply-bqf bqf)` | `{"type": "r5rs-call", "function": "r5rs:apply-bqf", "args": [{"fromNode": "#bqf"}]}` |

### Consensus Patterns

| M-Expression | S-Expression | CanvasL |
|--------------|--------------|---------|
| `consensus = gcd(a, b)` | `(gcd a b)` | `{"type": "r5rs-call", "function": "r5rs:gcd", "args": ["a", "b"]}` |
| `consensus = lcm(a, b)` | `(lcm a b)` | `{"type": "r5rs-call", "function": "r5rs:lcm", "args": ["a", "b"]}` |
| `result = tetra_consensus([2,4,6,8])` | `(tetra-consensus '(2 4 6 8))` | `{"type": "r5rs-call", "function": "r5rs:tetra-consensus", "args": [[2,4,6,8]]}` |

### Type System

| M-Expression | S-Expression | CanvasL |
|--------------|--------------|---------|
| `vertex = type_to_cube_vertex(type)` | `(type-to-cube-vertex type)` | `{"type": "r5rs-call", "function": "r5rs:type-to-cube-vertex", "args": ["procedure"]}` |
| `type = cube_vertex_to_type(7)` | `(cube-vertex-to-type 7)` | `{"type": "r5rs-call", "function": "r5rs:cube-vertex-to-type", "args": [7]}` |

## Common Patterns

### Pattern 1: Function Definition

**M**: `f(x, y) = x + y`  
**S**: `(define f (lambda (x y) (+ x y)))`  
**CanvasL**:
```json
{
  "id": "f",
  "type": "r5rs-call",
  "function": "r5rs:lambda",
  "args": [["x", "y"], {"function": "r5rs:add", "args": ["x", "y"]}]
}
```

### Pattern 2: Polyhedron BQF

**M**: `cube = [8, 12, 6]`  
**S**: `(define cube '(8 12 6))`  
**CanvasL**:
```json
{
  "id": "cube",
  "type": "node",
  "polyhedron": "cube",
  "bqf": [8, 12, 6]
}
```

### Pattern 3: Dual Swap

**M**: `octa = dual_swap(cube)`  
**S**: `(define octa (dual-swap cube))`  
**CanvasL**:
```json
{
  "id": "octa",
  "type": "r5rs-call",
  "function": "r5rs:dual-swap",
  "args": [{"fromNode": "#cube", "property": "bqf"}]
}
```

## BQF Quick Reference

| Expression Type | BQF | Meaning |
|----------------|-----|---------|
| Pure M-Expression | `[1, 0, 0]` | Affine value (x²) |
| Pure S-Expression | `[0, 0, 1]` | Projective function (y²) |
| Complete | `[1, 1, 1]` | Both (x² + xy + y²) |

## Polyhedra BQF Reference

| Polyhedron | BQF | M-Expression | S-Expression |
|------------|-----|--------------|--------------|
| Tetrahedron | `[4, 6, 4]` | `tetra = [4, 6, 4]` | `(define tetra '(4 6 4))` |
| Cube | `[8, 12, 6]` | `cube = [8, 12, 6]` | `(define cube '(8 12 6))` |
| Octahedron | `[6, 12, 8]` | `octa = [6, 12, 8]` | `(define octa '(6 12 8))` |
| Icosahedron | `[12, 30, 20]` | `icosa = [12, 30, 20]` | `(define icosa '(12 30 20))` |
| Dodecahedron | `[20, 30, 12]` | `dodeca = [20, 30, 12]` | `(define dodeca '(20 30 12))` |

## Translation Tips

1. **M-Expressions** → Static values, use `"type": "node"` with `"value"` or `"bqf"`
2. **S-Expressions** → Functions, use `"type": "r5rs-call"` with `"function"` or `"expression"`
3. **Always include metadata** with both M and S expressions for human readability
4. **Use node references** (`#node-id`) to reference other nodes
5. **BQF encoding** should be included in metadata for geometric operations

---

**See Also**: `09-CANVASL-M-S-EXPRESSION-GRAMMAR.md` for complete grammar specification

