---
id: bipartite-bqf-protocol-specification-rfc2119
title: "Bipartite-BQF Protocol Specification (RFC 2119)"
level: foundational
type: specification
tags: [bipartite-bqf, rfc2119, protocol, message-format, operations]
keywords: [bipartite-bqf-protocol, message-format, operation-sequences, error-handling, compatibility]
prerequisites: [bipartite-bqf-extension-rfc2119-spec]
enables: [bipartite-bqf-implementation]
related: [bipartite-bqf-extension-rfc2119-spec, canvasl-rfc2119-spec]
readingTime: 60
difficulty: 4
blackboard:
  status: active
  assignedAgent: "4D-Network-Agent"
  lastUpdate: "2025-01-07"
  dependencies: [bipartite-bqf-extension-rfc2119-spec]
  watchers: ["2D-Structural-Agent"]
  r5rsEngine: "r5rs-canvas-engine.scm"
---

# Bipartite-BQF Protocol Specification (RFC 2119)

**Status**: Draft  
**Version**: 1.0.0  
**Date**: 2025-01-07  
**Authors**: Automaton System

## Abstract

This specification defines the protocol for Bipartite-BQF operations, including message formats, operation sequences, error handling, and compatibility requirements. The protocol enables communication between CanvasL parsers, frontmatter processors, and knowledge model builders.

## Table of Contents

1. [Introduction](#1-introduction)
2. [Protocol Overview](#2-protocol-overview)
3. [Message Formats](#3-message-formats)
4. [Operation Sequences](#4-operation-sequences)
5. [Error Handling](#5-error-handling)
6. [Compatibility Requirements](#6-compatibility-requirements)
7. [Implementation Requirements](#7-implementation-requirements)
8. [References](#8-references)

---

## 1. Introduction

### 1.1 Purpose

This protocol specification defines:

- Message formats for Bipartite-BQF operations
- Operation sequences for common workflows
- Error handling and reporting
- Compatibility requirements between versions
- Protocol versioning

### 1.2 Scope

This specification covers:

- Protocol versioning
- Message formats (CanvasL ↔ Frontmatter sync)
- Operation sequences (BQF validation, polynomial operations)
- Error handling
- Compatibility matrix

### 1.3 RFC 2119 Keywords

The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT", "SHOULD", "SHOULD NOT", "RECOMMENDED", "MAY", and "OPTIONAL" in this document are to be interpreted as described in RFC 2119.

### 1.4 Related Documentation

- **`01-BIPARTITE-BQF-EXTENSION-RFC2119.md`**: Main extension specification
- **`03-FRONTMATTER-INTEGRATION-RFC2119.md`**: Frontmatter integration specification
- **`docs/04-CanvasL/CANVASL-RFC2119-SPEC.md`**: Base CanvasL specification

---

## 2. Protocol Overview

### 2.1 Protocol Version

The protocol version MUST be specified in protocol messages:

```json
{
  "protocol": "bipartite-bqf",
  "version": "1.0.0"
}
```

### 2.2 Protocol Components

The protocol consists of:

1. **Message Format**: JSON structure for protocol messages
2. **Operation Types**: Types of operations supported
3. **Error Codes**: Standard error codes
4. **Compatibility Rules**: Version compatibility requirements

### 2.3 Protocol Flow

```
┌─────────────┐         ┌──────────────┐         ┌─────────────┐
│   CanvasL   │ ──────── │   Protocol   │ ─────── │ Frontmatter │
│   Parser    │          │   Handler    │         │  Processor  │
└─────────────┘         └──────────────┘         └─────────────┘
        │                       │                       │
        └───────────────────────┼───────────────────────┘
                                │
                        ┌───────▼────────┐
                        │ Knowledge Model │
                        │     Builder    │
                        └────────────────┘
```

---

## 3. Message Formats

### 3.1 Synchronization Message

**Purpose**: Synchronize CanvasL node with frontmatter

**Format**:
```json
{
  "operation": "sync",
  "protocol": "bipartite-bqf",
  "version": "1.0.0",
  "source": "canvasl" | "frontmatter",
  "target": "frontmatter" | "canvasl",
  "nodeId": "node-id",
  "bipartite": {
    "partition": "topology" | "system",
    "bqf": { /* BQF object */ },
    "polynomial": { /* Polynomial object */ }
  },
  "metadata": {
    "file": "path/to/file.canvasl" | "path/to/file.md",
    "timestamp": "2025-01-07T00:00:00Z"
  }
}
```

### 3.2 Validation Message

**Purpose**: Validate BQF form or bipartite structure

**Format**:
```json
{
  "operation": "validate",
  "protocol": "bipartite-bqf",
  "version": "1.0.0",
  "type": "bqf" | "bipartite" | "polynomial" | "frontmatter",
  "data": { /* Data to validate */ },
  "options": {
    "strict": true | false,
    "reportErrors": true | false
  }
}
```

**Response**:
```json
{
  "valid": true | false,
  "errors": [
    {
      "code": "error-code",
      "message": "Error message",
      "path": "path.to.field"
    }
  ],
  "warnings": [ /* Warning objects */ ]
}
```

### 3.3 BQF Operation Message

**Purpose**: Perform BQF operation (evaluate, transform, etc.)

**Format**:
```json
{
  "operation": "bqf-operation",
  "protocol": "bipartite-bqf",
  "version": "1.0.0",
  "operationType": "eval" | "transform" | "to-procedure" | "to-polynomial",
  "bqf": { /* BQF object */ },
  "parameters": {
    "values": [1, 2, 3] | null,
    "transformation": "transformation-name" | null
  }
}
```

**Response**:
```json
{
  "result": { /* Operation result */ },
  "metadata": {
    "operation": "operation-type",
    "duration": 0.123,
    "timestamp": "2025-01-07T00:00:00Z"
  }
}
```

### 3.4 Polynomial Operation Message

**Purpose**: Perform polynomial operation

**Format**:
```json
{
  "operation": "polynomial-operation",
  "protocol": "bipartite-bqf",
  "version": "1.0.0",
  "operationType": "add" | "mult" | "compose" | "eval",
  "operands": [
    { /* Polynomial object */ },
    { /* Polynomial object */ }
  ],
  "parameters": {
    "point": [1, 2, 3] | null
  }
}
```

---

## 4. Operation Sequences

### 4.1 CanvasL → Frontmatter Synchronization

**Sequence**:
1. Parse CanvasL file
2. Extract nodes with `bipartite` metadata
3. For each node:
   - Find corresponding frontmatter file (if exists)
   - Extract `bipartite` section from frontmatter
   - Compare CanvasL and frontmatter `bipartite` objects
   - Update frontmatter if CanvasL is newer
   - Report conflicts if both modified

**Error Handling**:
- File not found: Create new frontmatter file
- Parse error: Report error, skip node
- Conflict: Report conflict, require manual resolution

### 4.2 Frontmatter → CanvasL Synchronization

**Sequence**:
1. Parse frontmatter file
2. Extract `bipartite` metadata
3. Find corresponding CanvasL node (if exists)
4. Update CanvasL node `bipartite` object
5. Save CanvasL file

**Error Handling**:
- Node not found: Create new node
- Parse error: Report error, skip file
- Validation error: Report error, skip update

### 4.3 BQF Validation Sequence

**Sequence**:
1. Extract BQF from node or edge
2. Validate BQF form syntax
3. Validate coefficients (must be numbers)
4. Validate variables (must match dimension)
5. Validate signature (must be valid)
6. Validate against dimensional progression
7. Report validation results

**Error Codes**:
- `BQF_INVALID_FORM`: BQF form syntax invalid
- `BQF_INVALID_COEFFICIENTS`: Coefficients invalid
- `BQF_INVALID_VARIABLES`: Variables don't match dimension
- `BQF_INVALID_SIGNATURE`: Signature invalid
- `BQF_INVALID_PROGRESSION`: Doesn't match dimensional progression

### 4.4 Polynomial Operation Sequence

**Sequence**:
1. Validate polynomial operands
2. Perform operation (add, mult, compose, eval)
3. Validate result
4. Return result

**Error Codes**:
- `POLY_INVALID_OPERAND`: Operand invalid
- `POLY_INCOMPATIBLE_DIMENSIONS`: Dimensions incompatible
- `POLY_OPERATION_FAILED`: Operation failed

---

## 5. Error Handling

### 5.1 Error Code Format

Error codes MUST follow the format: `{CATEGORY}_{ERROR_TYPE}`

**Categories**:
- `BQF`: Binary Quadratic Form errors
- `BIPARTITE`: Bipartite structure errors
- `POLY`: Polynomial errors
- `FRONTMATTER`: Frontmatter errors
- `PROTOCOL`: Protocol errors

### 5.2 Standard Error Codes

#### 5.2.1 BQF Errors

- `BQF_INVALID_FORM`: BQF form syntax invalid
- `BQF_INVALID_COEFFICIENTS`: Coefficients invalid
- `BQF_INVALID_VARIABLES`: Variables don't match dimension
- `BQF_INVALID_SIGNATURE`: Signature invalid
- `BQF_INVALID_PROGRESSION`: Doesn't match dimensional progression

#### 5.2.2 Bipartite Errors

- `BIPARTITE_INVALID_PARTITION`: Partition value invalid
- `BIPARTITE_INVALID_EDGE`: Edge violates bipartite structure
- `BIPARTITE_INCONSISTENT`: Bipartite structure inconsistent

#### 5.2.3 Polynomial Errors

- `POLY_INVALID_OPERAND`: Operand invalid
- `POLY_INCOMPATIBLE_DIMENSIONS`: Dimensions incompatible
- `POLY_OPERATION_FAILED`: Operation failed

#### 5.2.4 Frontmatter Errors

- `FRONTMATTER_PARSE_ERROR`: Frontmatter parse error
- `FRONTMATTER_MISSING_FIELD`: Required field missing
- `FRONTMATTER_INVALID_FORMAT`: Format invalid

#### 5.2.5 Protocol Errors

- `PROTOCOL_VERSION_MISMATCH`: Protocol version mismatch
- `PROTOCOL_INVALID_MESSAGE`: Message format invalid
- `PROTOCOL_OPERATION_UNSUPPORTED`: Operation not supported

### 5.3 Error Response Format

```json
{
  "error": {
    "code": "ERROR_CODE",
    "message": "Human-readable error message",
    "path": "path.to.field",
    "details": { /* Additional error details */ }
  }
}
```

---

## 6. Compatibility Requirements

### 6.1 Protocol Version Compatibility

- **Same MAJOR version**: MUST be compatible
- **Different MAJOR version**: MAY be incompatible
- **PATCH updates**: SHOULD remain compatible

### 6.2 Message Format Compatibility

- New fields MAY be added (backward compatible)
- Required fields MUST NOT be removed (breaking change)
- Field types MUST NOT change (breaking change)

### 6.3 Operation Compatibility

- New operations MAY be added
- Existing operations MUST remain supported
- Operation signatures MUST NOT change (breaking change)

### 6.4 Error Code Compatibility

- New error codes MAY be added
- Existing error codes MUST remain valid
- Error code meanings MUST NOT change

---

## 7. Implementation Requirements

### 7.1 Protocol Handler

Implementations MUST provide:

- Protocol version validation
- Message format validation
- Operation routing
- Error handling
- Response formatting

### 7.2 Message Validation

- Messages MUST be validated before processing
- Invalid messages MUST return protocol error
- Validation errors MUST be reported clearly

### 7.3 Error Reporting

- Errors MUST use standard error codes
- Error messages MUST be human-readable
- Error paths MUST identify field location
- Error details MAY provide additional context

---

## 8. References

### 8.1 Related Specifications

- **`01-BIPARTITE-BQF-EXTENSION-RFC2119.md`**: Main extension specification
- **`03-FRONTMATTER-INTEGRATION-RFC2119.md`**: Frontmatter integration specification
- **`docs/04-CanvasL/CANVASL-RFC2119-SPEC.md`**: Base CanvasL specification

### 8.2 Standards

- **RFC 2119**: Key words for use in RFCs to Indicate Requirement Levels
- **JSON**: ECMA-404 The JSON Data Interchange Standard

---

**End of Protocol Specification**

