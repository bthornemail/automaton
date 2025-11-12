---
id: r5rs-functions-reference
title: "R5RS Functions for Bipartite-BQF and Polynomial Operations"
level: foundational
type: reference
tags: [r5rs, scheme, functions, bipartite-bqf, polynomial, implementation]
keywords: [r5rs-functions, bqf-eval, bqf-transform, poly-to-bqf, bqf-to-procedure, poly-add, poly-mult, poly-compose, poly-eval]
prerequisites: [bipartite-bqf-canvasl-extension-rfc2119-spec, protocol-specification-rfc2119]
enables: []
related: [r5rs-canvas-engine]
readingTime: 10
difficulty: 3
version: "1.0.0"
gitTag: "v1.0.0"
immutableTag: "v1.0.0-immutable"
versionDirectory: "versions/v1.0.0/"
blackboard:
  status: active
  assignedAgent: "0D-Topology-Agent"
  lastUpdate: "2025-01-07"
  dependencies: []
  watchers: []
  r5rsEngine: "r5rs-canvas-engine.scm"
  selfBuilding:
    enabled: true
    source: "r5rs-canvas-engine.scm"
    pattern: "r5rs-function-generation"
    regeneration:
      function: "r5rs:generate-r5rs-docs"
      args: ["r5rs-canvas-engine.scm"]
  versionConjoining:
    package: "@automaton/bipartite-bqf-canvasl-spec@1.0.0"
    extensionSpec: "01-BIPARTITE-BQF-EXTENSION-RFC2119.md@1.0.0"
    protocolSpec: "02-PROTOCOL-SPECIFICATION-RFC2119.md@1.0.0"
    immutableSnapshot: "versions/v1.0.0/"
---

# R5RS Function Extensions for Bipartite-BQF

**Version**: 1.0.0  
**Date**: 2025-01-07  
**Package**: `@automaton/bipartite-bqf-canvasl-spec@1.0.0` | **Git Tags**: `v1.0.0`, `v1.0.0-immutable`

## Overview

This document defines the R5RS function extensions required to support Bipartite Binary Quadratic Polynomial Form (Bipartite-BQF) operations.

## Function Registry

All BQF-related R5RS functions MUST be registered in `r5rs-functions-trie.jsonl` and MUST be invocable via `r5rs:invoke-from-jsonl`.

## BQF Functions

### r5rs:bqf-eval

**Purpose**: Evaluate BQF at given point

**Signature**: `(bqf-eval bqf values)`

**Parameters**:
- `bqf`: BQF object with `form`, `coefficients`, `variables`
- `values`: Array of values matching BQF variables

**Returns**: Number (evaluated BQF value)

**Example**:
```scheme
(bqf-eval 
  '((form . "Q(x,y) = x² + y²")
    (coefficients . (1 0 1))
    (variables . (x y)))
  '(2 3))
;; => 13  (2² + 3² = 4 + 9 = 13)
```

**Implementation Requirements**:
- MUST parse BQF form
- MUST substitute values for variables
- MUST evaluate polynomial expression
- MUST return numeric result

### r5rs:bqf-transform

**Purpose**: Transform BQF using transformation function

**Signature**: `(bqf-transform bqf transformation)`

**Parameters**:
- `bqf`: BQF object
- `transformation`: Transformation name or function

**Returns**: Transformed BQF object

**Example**:
```scheme
(bqf-transform 
  '((form . "Q(x) = x²")
    (variables . (x)))
  "sin")
;; => ((form . "Q(x,y) = x² + y²")
;;     (variables . (x y)))
```

**Supported Transformations**:
- `tan`: 0D → 1D transformation
- `sin`: 1D → 2D transformation
- `cos`: 2D → 3D transformation

### r5rs:poly-to-bqf

**Purpose**: Convert polynomial to BQF form

**Signature**: `(poly-to-bqf polynomial)`

**Parameters**:
- `polynomial`: Polynomial object or expression string

**Returns**: BQF object

**Example**:
```scheme
(poly-to-bqf "x² + y²")
;; => ((form . "Q(x,y) = x² + y²")
;;     (coefficients . (1 0 1))
;;     (variables . (x y)))
```

**Implementation Requirements**:
- MUST parse polynomial expression
- MUST extract quadratic terms
- MUST generate BQF form
- MUST extract coefficients
- MUST extract variables

### r5rs:bqf-to-procedure

**Purpose**: Convert BQF to R5RS procedure

**Signature**: `(bqf-to-procedure bqf)`

**Parameters**:
- `bqf`: BQF object

**Returns**: R5RS procedure (lambda expression)

**Example**:
```scheme
(bqf-to-procedure 
  '((form . "Q(x,y) = x² + y²")
    (variables . (x y))))
;; => (lambda (x y) (+ (* x x) (* y y)))
```

**Implementation Requirements**:
- MUST generate lambda expression
- MUST use BQF variables as parameters
- MUST generate procedure body from BQF form
- MUST return valid R5RS procedure

## Polynomial Functions

### r5rs:poly-add

**Purpose**: Add two polynomial vectors

**Signature**: `(poly-add v1 v2)`

**Parameters**:
- `v1`: Polynomial vector (8-element array)
- `v2`: Polynomial vector (8-element array)

**Returns**: Sum polynomial vector

**Example**:
```scheme
(poly-add 
  '(1 0 0 0 0 0 0 0)
  '(0 1 0 0 0 0 0 0))
;; => (1 1 0 0 0 0 0 0)
```

**Implementation Requirements**:
- MUST perform component-wise addition
- MUST return 8-element vector
- MUST handle all numeric types

### r5rs:poly-mult

**Purpose**: Multiply two polynomial vectors

**Signature**: `(poly-mult v1 v2)`

**Parameters**:
- `v1`: Polynomial vector (8-element array)
- `v2`: Polynomial vector (8-element array)

**Returns**: Product polynomial vector

**Example**:
```scheme
(poly-mult 
  '(1 0 0 0 0 0 0 0)
  '(0 1 0 0 0 0 0 0))
;; => Polynomial multiplication result
```

**Implementation Requirements**:
- MUST perform polynomial multiplication
- MUST return 8-element vector
- MUST handle polynomial algebra correctly

### r5rs:poly-compose

**Purpose**: Compose two polynomials

**Signature**: `(poly-compose p1 p2)`

**Parameters**:
- `p1`: First polynomial
- `p2`: Second polynomial

**Returns**: Composed polynomial

**Example**:
```scheme
(poly-compose 
  "x²"
  "x + 1")
;; => "(x + 1)²"
```

**Implementation Requirements**:
- MUST perform function composition
- MUST return valid polynomial
- MUST handle substitution correctly

### r5rs:poly-eval

**Purpose**: Evaluate polynomial at point

**Signature**: `(poly-eval polynomial point)`

**Parameters**:
- `polynomial`: Polynomial object or expression
- `point`: Point values (array)

**Returns**: Evaluated value

**Example**:
```scheme
(poly-eval 
  "x² + y²"
  '(2 3))
;; => 13
```

**Implementation Requirements**:
- MUST substitute point values
- MUST evaluate expression
- MUST return numeric result

## Function Registration

### Registry Format

All functions MUST be registered in `r5rs-functions-trie.jsonl`:

```json
{"id": "r5rs:bqf-eval", "type": "r5rs-function", "name": "bqf-eval", "signature": "(bqf-eval bqf values)", "description": "Evaluate BQF at point", "module": "MODULE 7: Bipartite-BQF Operations"}
```

### Function Invocation

Functions MUST be invocable via CanvasL:

```json
{
  "id": "bqf-compute",
  "type": "r5rs-call",
  "function": "r5rs:bqf-eval",
  "args": [
    {"form": "Q(x,y) = x² + y²", "coefficients": [1, 0, 1], "variables": ["x", "y"]},
    [2, 3]
  ]
}
```

## Implementation Requirements

### Function Implementation

All functions MUST:

- Be implemented in R5RS Scheme
- Be registered in function registry
- Support CanvasL invocation
- Return JSON-serializable results
- Handle errors gracefully

### Error Handling

Functions MUST:

- Validate input parameters
- Return error objects for invalid input
- Use standard error codes
- Provide descriptive error messages

## Related Documentation

- **`01-BIPARTITE-BQF-EXTENSION-RFC2119.md`**: Main extension specification
- **`r5rs-canvas-engine.scm`**: R5RS function implementations
- **`r5rs-functions-trie.jsonl`**: Function registry

---

**End of R5RS Functions Reference**

