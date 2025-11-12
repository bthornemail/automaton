---
id: grammar-extension-reference
title: "CanvasL Grammar Extension for Bipartite-BQF"
level: foundational
type: reference
tags: [canvasl, grammar, lezer, bipartite-bqf, syntax]
keywords: [grammar-extension, canvasl-syntax, bipartite-object, bqf-object, polynomial-object]
prerequisites: [canvasl-rfc2119-spec, bipartite-bqf-canvasl-extension-rfc2119-spec]
enables: []
related: [ui-src-grammars-canvasl-grammar]
readingTime: 10
difficulty: 4
version: "1.0.0"
gitTag: "v1.0.0"
immutableTag: "v1.0.0-immutable"
versionDirectory: "versions/v1.0.0/"
blackboard:
  status: active
  assignedAgent: "2D-Structural-Agent"
  lastUpdate: "2025-01-07"
  dependencies: []
  watchers: []
  r5rsEngine: "r5rs-canvas-engine.scm"
  selfBuilding:
    enabled: true
    source: "r5rs-canvas-engine.scm"
    pattern: "grammar-generation"
    regeneration:
      function: "r5rs:generate-grammar-docs"
      args: ["ui/src/grammars/canvasl.grammar"]
  versionConjoining:
    package: "@automaton/bipartite-bqf-canvasl-spec@1.0.0"
    extensionSpec: "01-BIPARTITE-BQF-EXTENSION-RFC2119.md@1.0.0"
    immutableSnapshot: "versions/v1.0.0/"
---

# CanvasL Grammar Extension for Bipartite-BQF

**Version**: 1.0.0  
**Date**: 2025-01-07  
**Package**: `@automaton/bipartite-bqf-canvasl-spec@1.0.0` | **Git Tags**: `v1.0.0`, `v1.0.0-immutable`

## Overview

This document defines the grammar extensions required to support Bipartite Binary Quadratic Polynomial Form (Bipartite-BQF) in the CanvasL grammar.

## Base Grammar

The CanvasL grammar is defined in `ui/src/grammars/canvasl.grammar` and extends JSONL with CanvasL-specific features.

## Grammar Extensions

### Bipartite Metadata Token

```grammar
BipartiteMetadata {
  "bipartite" jsonColon BipartiteObject
}
```

### Bipartite Object Rule

```grammar
BipartiteObject {
  "partition" jsonColon BipartitePartition jsonComma
  ("bqf" jsonColon BQFObject)?
  ("polynomial" jsonColon PolynomialObject)?
  ("progression" jsonColon ProgressionString)?
  ("mapping" jsonColon MappingString)?
}
```

### Bipartite Partition Token

```grammar
BipartitePartition {
  "topology" | "system" | "topology-system" | "topology-topology" | "system-system"
}
```

### BQF Object Rule

```grammar
BQFObject {
  "form" jsonColon jsonString jsonComma
  "coefficients" jsonColon JSONLArray jsonComma
  "signature" jsonColon jsonString jsonComma
  "variables" jsonColon JSONLArray jsonComma
  ("polynomial" jsonColon jsonString)?
  ("symbol" jsonColon jsonString)?
  ("procedure" jsonColon jsonString)?
}
```

### Polynomial Object Rule

```grammar
PolynomialObject {
  "monad" jsonColon JSONLArray jsonComma
  "functor" jsonColon JSONLArray jsonComma
  "perceptron" jsonColon JSONLArray
}
```

### Progression String Token

```grammar
ProgressionString {
  jsonString  // Format: "0D → 1D", "1D → 2D", etc.
}
```

### Mapping String Token

```grammar
MappingString {
  jsonString  // Format: "identity → church-zero", etc.
}
```

## Integration with Base Grammar

### Node Rule Extension

The base `CanvasLNode` rule MUST be extended:

```grammar
CanvasLNode {
  "id" jsonColon jsonString jsonComma
  "type" jsonColon canvaslType jsonComma
  JSONLProperty*
  BipartiteMetadata?  // NEW: Bipartite metadata
}
```

### Edge Rule Extension

The base `CanvasLEdge` rule MUST be extended:

```grammar
CanvasLEdge {
  "id" jsonColon jsonString jsonComma
  "type" jsonColon canvaslEdgeType jsonComma
  ("from" | "fromNode") jsonColon (jsonString | canvaslReference) jsonComma
  ("to" | "toNode") jsonColon (jsonString | canvaslReference) jsonComma
  JSONLProperty*
  BipartiteMetadata?  // NEW: Bipartite metadata
}
```

## Token Definitions

### New Tokens Required

- `bipartitePartition`: Partition identifier token
- `bqfForm`: BQF form string token
- `bqfSignature`: BQF signature token
- `progressionString`: Dimensional progression string token
- `mappingString`: Topology-system mapping string token

## Validation Rules

### Grammar Validation

- Bipartite metadata MUST be valid JSON object
- Partition values MUST match `BipartitePartition` rule
- BQF objects MUST have required fields
- Polynomial objects MUST have 8-element arrays
- Progression strings MUST match format "XD → YD"

## Implementation Notes

### Parser Generation

The grammar MUST be processed by Lezer to generate parser:

1. Add new token definitions
2. Add new grammar rules
3. Extend existing rules
4. Generate parser code

### AST Structure

The AST MUST include:

- `BipartiteMetadata` node type
- `BQFObject` node type
- `PolynomialObject` node type
- Position information for all nodes

## Examples

### Valid Bipartite Node

```json
{
  "id": "2D-topology",
  "type": "text",
  "dimension": "2D",
  "bipartite": {
    "partition": "topology",
    "bqf": {
      "form": "Q(x,y) = x² + y²",
      "coefficients": [1, 0, 1],
      "signature": "euclidean",
      "variables": ["x", "y"]
    }
  }
}
```

### Valid Bipartite Edge

```json
{
  "id": "v:1D→2D",
  "type": "vertical",
  "fromNode": "1D-topology",
  "toNode": "2D-topology",
  "bipartite": {
    "progression": "1D → 2D",
    "bqf": {
      "from": {"form": "Q(x) = x²", "variables": ["x"]},
      "to": {"form": "Q(x,y) = x² + y²", "variables": ["x", "y"]}
    }
  }
}
```

## Related Documentation

- **`01-BIPARTITE-BQF-EXTENSION-RFC2119.md`**: Main extension specification
- **`docs/04-CanvasL/CANVASL-RFC2119-SPEC.md`**: Base CanvasL grammar specification
- **`ui/src/grammars/canvasl.grammar`**: Base grammar file

---

**End of Grammar Extension Reference**

