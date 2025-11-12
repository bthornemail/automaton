---
id: validation-rules-reference
title: "Validation Rules for Bipartite-BQF CanvasL Extension"
level: foundational
type: reference
tags: [canvasl, validation, rfc2119, bipartite-bqf, frontmatter, protocol]
keywords: [validation-rules, bqf-validation, bipartite-validation, polynomial-validation, frontmatter-validation, dimensional-consistency]
prerequisites: [bipartite-bqf-canvasl-extension-rfc2119-spec, protocol-specification-rfc2119, frontmatter-integration-rfc2119]
enables: []
related: [lsp-integration-protocol]
readingTime: 15
difficulty: 3
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
    pattern: "validation-rule-generation"
    regeneration:
      function: "r5rs:generate-validation-docs"
      args: ["docs/28-Canvasl-Frontmatter-Knowledge-Model/"]
  versionConjoining:
    package: "@automaton/bipartite-bqf-canvasl-spec@1.0.0"
    extensionSpec: "01-BIPARTITE-BQF-EXTENSION-RFC2119.md@1.0.0"
    protocolSpec: "02-PROTOCOL-SPECIFICATION-RFC2119.md@1.0.0"
    frontmatterSpec: "03-FRONTMATTER-INTEGRATION-RFC2119.md@1.0.0"
    immutableSnapshot: "versions/v1.0.0/"
---

# Bipartite-BQF Validation Rules Reference

**Version**: 1.0.0  
**Date**: 2025-01-07  
**Package**: `@automaton/bipartite-bqf-canvasl-spec@1.0.0` | **Git Tags**: `v1.0.0`, `v1.0.0-immutable`

## Overview

This document provides a complete reference for all validation rules for the Bipartite Binary Quadratic Polynomial Form (Bipartite-BQF) extension.

## Validation Categories

### 1. BQF Validation

#### 1.1 BQF Form Validation

- **Rule**: BQF form MUST match dimensional progression
- **Check**: Compare BQF form against expected form for dimension
- **Error Code**: `BQF_INVALID_PROGRESSION`

**Expected Forms**:
- 0D: `Q() = 0`
- 1D: `Q(x) = x²`
- 2D: `Q(x,y) = x² + y²`
- 3D: `Q(x,y,z,t) = x² + y² + z² - t²`
- 4D: `Q(w,x,y,z,t) = w² + x² + y² + z² - t²`
- 5D: `Q(...) = Σᵢ xᵢ² - t²`
- 6D: `Q(...) = Σᵢ xᵢ² - t² + higher terms`
- 7D: `Q(...) = Σᵢ xᵢ² - t² + quantum terms`

#### 1.2 BQF Coefficients Validation

- **Rule**: BQF coefficients MUST be valid numbers
- **Check**: All coefficients must be numbers (not NaN, not Infinity)
- **Error Code**: `BQF_INVALID_COEFFICIENTS`

**Format**: Array of numbers `[a, b, c, ...]`

#### 1.3 BQF Variables Validation

- **Rule**: BQF variables MUST match dimension count
- **Check**: Variable count must match dimension (0D: 0 vars, 1D: 1 var, etc.)
- **Error Code**: `BQF_INVALID_VARIABLES`

**Expected Variable Counts**:
- 0D: 0 variables
- 1D: 1 variable (`x`)
- 2D: 2 variables (`x`, `y`)
- 3D: 4 variables (`x`, `y`, `z`, `t`)
- 4D: 5 variables (`w`, `x`, `y`, `z`, `t`)
- 5D+: Variable count matches dimension

#### 1.4 BQF Signature Validation

- **Rule**: BQF signatures MUST be valid
- **Check**: Signature must be one of: `euclidean`, `lorentz`, `consensus`, `intelligence`, `quantum`, `custom`
- **Error Code**: `BQF_INVALID_SIGNATURE`

**Valid Signatures**:
- `euclidean`: Positive definite (all coefficients positive)
- `lorentz`: Minkowski signature (one negative coefficient)
- `consensus`: Consensus topology signature
- `intelligence`: Intelligence topology signature
- `quantum`: Quantum topology signature
- `custom`: Custom signature (requires documentation)

### 2. Bipartite Structure Validation

#### 2.1 Partition Validation

- **Rule**: Partition values MUST be valid
- **Check**: Partition must be one of: `topology`, `system`, `topology-system`, `topology-topology`, `system-system`
- **Error Code**: `BIPARTITE_INVALID_PARTITION`

**Valid Partitions**:
- `topology`: Left partition (mathematical foundations)
- `system`: Right partition (computational implementations)
- `topology-system`: Horizontal edge (topology ↔ system)
- `topology-topology`: Vertical edge (topology progression)
- `system-system`: Vertical edge (system progression)

#### 2.2 Horizontal Edge Validation

- **Rule**: Horizontal edges MUST connect topology ↔ system
- **Check**: `fromNode` must be topology, `toNode` must be system (or vice versa)
- **Error Code**: `BIPARTITE_INVALID_EDGE`

**Valid Patterns**:
- `topology → system` (horizontal mapping)
- `system → topology` (reverse horizontal mapping)

#### 2.3 Vertical Edge Validation

- **Rule**: Vertical edges MUST connect same partition
- **Check**: Both `fromNode` and `toNode` must be same partition (topology-topology or system-system)
- **Error Code**: `BIPARTITE_INVALID_EDGE`

**Valid Patterns**:
- `topology → topology` (dimensional progression)
- `system → system` (system progression)

#### 2.4 Bipartite Consistency Validation

- **Rule**: Bipartite structure MUST be consistent
- **Check**: All nodes must have valid partition assignments, all edges must follow bipartite rules
- **Error Code**: `BIPARTITE_INCONSISTENT`

**Consistency Checks**:
- All topology nodes in left partition
- All system nodes in right partition
- No topology-topology edges crossing to system
- No system-system edges crossing to topology

### 3. Polynomial Validation

#### 3.1 Polynomial Vector Validation

- **Rule**: Polynomial vectors MUST have 8 components
- **Check**: `monad`, `functor`, `perceptron` arrays must have exactly 8 elements
- **Error Code**: `POLY_INVALID_OPERAND`

**Format**: `[v₁, v₂, v₃, v₄, v₅, v₆, v₇, v₈]`

**8-Type Polynomial Basis**:
1. Boolean
2. Pair
3. Symbol
4. Number
5. Char
6. String
7. Vector
8. Procedure/Port

#### 3.2 Polynomial Operation Validation

- **Rule**: Polynomial operations MUST be valid
- **Check**: Operands must be valid polynomial vectors, operation must be supported
- **Error Code**: `POLY_OPERATION_FAILED`

**Supported Operations**:
- `poly-add`: Component-wise addition
- `poly-mult`: Polynomial multiplication
- `poly-compose`: Function composition
- `poly-eval`: Evaluation at point

#### 3.3 Polynomial → BQF Mapping Validation

- **Rule**: Polynomial → BQF mapping MUST be consistent
- **Check**: Polynomial must map to valid BQF form
- **Error Code**: `POLY_INVALID_MAPPING`

### 4. Frontmatter Validation

#### 4.1 Frontmatter Schema Validation

- **Rule**: Frontmatter `bipartite` section MUST match schema
- **Check**: Required fields present, types correct
- **Error Code**: `FRONTMATTER_INVALID_FORMAT`

**Required Fields**:
- `partition`: string ("topology" | "system")
- `dimension`: string ("0D" | "1D" | ... | "7D")

**Optional Fields**:
- `bqf`: BQF object
- `polynomial`: Polynomial object
- `relationships`: Relationships object

#### 4.2 CanvasL ↔ Frontmatter Sync Validation

- **Rule**: CanvasL `bipartite` object MUST match frontmatter `bipartite` section
- **Check**: Compare structures, report mismatches
- **Error Code**: `FRONTMATTER_SYNC_MISMATCH`

**Sync Checks**:
- Partition values match
- Dimension values match
- BQF objects match (if present)
- Polynomial objects match (if present)

#### 4.3 Frontmatter Relationship Validation

- **Rule**: Frontmatter relationships MUST be valid
- **Check**: Referenced nodes must exist
- **Error Code**: `FRONTMATTER_INVALID_RELATIONSHIP`

**Relationship Checks**:
- `topology` reference must point to valid topology node
- `system` reference must point to valid system node

### 5. Dimensional Progression Validation

#### 5.1 Progression Consistency

- **Rule**: BQF progression MUST be consistent
- **Check**: Each dimension must progress correctly (0D → 1D → 2D → ...)
- **Error Code**: `BQF_INVALID_PROGRESSION`

**Progression Rules**:
- 0D → 1D: `Q() = 0` → `Q(x) = x²`
- 1D → 2D: `Q(x) = x²` → `Q(x,y) = x² + y²`
- 2D → 3D: `Q(x,y) = x² + y²` → `Q(x,y,z,t) = x² + y² + z² - t²`
- And so on...

#### 5.2 Symbol → Polynomial → BQF → Procedure Validation

- **Rule**: Mapping chain MUST be valid
- **Check**: Symbol must map to polynomial, polynomial to BQF, BQF to procedure
- **Error Code**: `MAPPING_INVALID_CHAIN`

**Mapping Validation**:
- Symbol format valid
- Polynomial expression valid
- BQF form matches polynomial
- Procedure syntax valid (R5RS)

## Error Reporting

### Error Format

```json
{
  "code": "ERROR_CODE",
  "message": "Human-readable error message",
  "path": "path.to.field",
  "severity": "error" | "warning",
  "details": { /* Additional error details */ }
}
```

### Error Severity

- **Error**: Validation failure, must be fixed
- **Warning**: Potential issue, should be reviewed

## Validation Tools

### Command Line

```bash
# Validate CanvasL file
tsx validate-bipartite-bqf.ts file.canvasl

# Validate frontmatter
tsx validate-frontmatter.ts file.md

# Validate synchronization
tsx validate-sync.ts file.canvasl file.md
```

### Programmatic

```typescript
import { BipartiteBQFValidator } from './bipartite-bqf-validator';

const validator = new BipartiteBQFValidator();
const result = await validator.validate(canvaslNode);

if (!result.valid) {
  result.errors.forEach(error => {
    console.error(`${error.code}: ${error.message} at ${error.path}`);
  });
}
```

## Related Documentation

- **`01-BIPARTITE-BQF-EXTENSION-RFC2119.md`**: Main extension specification
- **`02-PROTOCOL-SPECIFICATION-RFC2119.md`**: Protocol specification with error codes

---

**End of Validation Rules Reference**

