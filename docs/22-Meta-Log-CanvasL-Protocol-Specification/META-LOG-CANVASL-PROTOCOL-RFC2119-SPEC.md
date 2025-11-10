---
id: meta-log-canvasl-protocol-rfc2119-spec
title: "Meta-Log CanvasL Protocol Specification (RFC 2119)"
level: foundational
type: specification
tags: [meta-log, canvasl, protocol, rfc2119, specification, prolog, datalog, r5rs, jsonl, multiverse-canvas]
keywords: [meta-log-canvasl-protocol, rfc2119-specification, prolog-integration, datalog-integration, r5rs-integration, canvasl-format, jsonl-extension, multiverse-canvas, federated-provenance, automaton-evolution, knowledge-extraction, agent-api]
prerequisites: [r5rs-expressions-rfc2119-spec, jsonl-database-adapter-rfc2119-spec, canvasl-rfc2119-spec, multiverse-canvas-rfc2119-spec]
enables: [meta-log-canvasl-implementation-guide, meta-log-canvasl-quick-reference]
related: [r5rs-canvas-engine, blackboard-architecture-guide, federated-provenance-rfc2119-spec, automaton-evolution-logging-rfc2119-spec, agent-api-documentation]
readingTime: 180
difficulty: 5
blackboard:
  status: active
  assignedAgent: null
  lastUpdate: 2025-11-10
  dependencies: [r5rs-canvas-engine, meta-log-db, canvasl-parser]
  watchers: ["Query-Interface-Agent", "Self-Modification-Agent", "6D-Intelligence-Agent"]
  r5rsEngine: "r5rs-canvas-engine.scm"
  selfBuilding:
    enabled: true
    source: "r5rs-canvas-engine.scm"
    pattern: "meta-log-canvasl-protocol"
    regeneration:
      function: "r5rs:parse-jsonl-canvas"
      args: ["generate.metaverse.jsonl"]
      context:
        module: "MODULE 2: JSONL Parser & Canvas Loader"
        functions: ["r5rs:parse-jsonl-canvas", "r5rs:extract-facts", "r5rs:jsonl-to-rdf", "r5rs:prolog-query", "r5rs:datalog-query", "r5rs:sparql-query"]
        pipeline:
          - step: "parse-canvasl"
            function: "r5rs:parse-jsonl-canvas"
            args: ["generate.metaverse.canvasl"]
          - step: "extract-facts"
            function: "r5rs:extract-facts"
            args: ["parsed-objects"]
          - step: "convert-to-rdf"
            function: "r5rs:jsonl-to-rdf"
            args: ["facts"]
          - step: "query-provenance"
            function: "r5rs:sparql-query"
            args: ["SELECT ?id ?file ?line WHERE { ?id prov:wasDerivedFrom ?source }", "triples"]
          - step: "validate-shacl"
            function: "r5rs:shacl-validate"
            args: ["shapes", "triples"]
  prologIntegration:
    enabled: true
    module: "MODULE 6: Logic Programming"
    functions: ["r5rs:build-prolog-db", "r5rs:prolog-query", "r5rs:unify", "r5rs:resolve"]
    source: "grok_files/08-Grok.md"
  datalogIntegration:
    enabled: true
    module: "MODULE 6: Logic Programming"
    functions: ["r5rs:extract-facts", "r5rs:datalog-query", "r5rs:build-datalog-program", "r5rs:fixed-point"]
    source: "grok_files/03-Grok.md, grok_files/10-Grok.md"
  rdfIntegration:
    enabled: true
    module: "MODULE 3: RDF Layer"
    functions: ["r5rs:jsonl-to-rdf", "r5rs:rdf-query", "r5rs:sparql-query", "r5rs:rdfs-entailment"]
    source: "grok_files/04-Grok.md, grok_files/09-Grok.md"
  shaclValidation:
    enabled: true
    module: "MODULE 5: SHACL Validation"
    functions: ["r5rs:load-shacl-shapes", "r5rs:shacl-validate", "r5rs:shacl-report"]
    source: "grok_files/07-Grok.md"
  canvaslFormat:
    enabled: true
    directives: ["@version", "@schema", "@r5rs-engine"]
    extensions: ["r5rs-call", "dimension-references", "node-references", "scheme-expressions"]
    grammar: "ui/src/grammars/canvasl.grammar"
  federatedProvenance:
    enabled: true
    mechanisms: ["self-reference-metadata", "reference-nodes", "unified-topology"]
    queryInterfaces: ["prolog", "datalog", "sparql"]
  automatonEvolution:
    enabled: true
    features: ["snapshot-system", "memory-monitoring", "variant-generation", "evolution-analysis"]
  knowledgeExtraction:
    enabled: true
    capabilities: ["fact-extraction", "rule-extraction", "agent-extraction", "function-extraction"]
  agentAPI:
    enabled: true
    features: ["agent-discovery", "agent-execution", "workflow-engine", "coordination-engine"]
---

# Meta-Log CanvasL Protocol Specification (RFC 2119)

**Status**: Draft  
**Version**: 1.0  
**Date**: 2025-11-10  
**Authors**: Automaton System

## Abstract

This unified specification defines the **Meta-Log CanvasL Protocol**, a comprehensive system that integrates:

- **CanvasL Format**: Extended JSONL format with directives, R5RS functions, dimension references, and node references
- **Meta-Log Framework**: ProLog, DataLog, and R5RS integration for querying, reasoning, and computation
- **Federated Provenance**: Embedded provenance tracking across multiple files
- **Automaton Evolution**: Self-modification tracking, memory monitoring, and variant generation
- **Knowledge Extraction**: Structured knowledge extraction from documentation
- **Agent API**: Multi-agent system coordination and execution

The protocol provides a unified interface for working with computational topology canvases spanning dimensions 0D-7D, enabling self-referential multiverse canvas systems with full traceability, validation, and evolution capabilities.

## Table of Contents

1. [Introduction](#1-introduction)
2. [Terminology](#2-terminology)
3. [Architecture Overview](#3-architecture-overview)
4. [CanvasL Format Specification](#4-canvasl-format-specification)
5. [Meta-Log Framework Integration](#5-meta-log-framework-integration)
6. [R5RS Expression Foundation](#6-r5rs-expression-foundation)
7. [JSONL Database Adapter](#7-jsonl-database-adapter)
8. [ProLog Integration](#8-prolog-integration)
9. [DataLog Integration](#9-datalog-integration)
10. [RDF and SPARQL Support](#10-rdf-and-sparql-support)
11. [SHACL Validation](#11-shacl-validation)
12. [Federated Provenance Tracking](#12-federated-provenance-tracking)
13. [Automaton Evolution System](#13-automaton-evolution-system)
14. [Knowledge Extraction and Propagation](#14-knowledge-extraction-and-propagation)
15. [Agent Procedures and Constraints](#15-agent-procedures-and-constraints)
16. [Meta-Log Database Implementation](#16-meta-log-database-implementation)
17. [Automatons CanvasL Integration](#17-automatons-canvasl-integration)
18. [File Format Requirements](#18-file-format-requirements)
19. [Implementation Constraints](#19-implementation-constraints)
20. [Validation Requirements](#20-validation-requirements)
21. [Protocol Message Types](#21-protocol-message-types)
22. [References](#22-references)

---

## 1. Introduction

### 1.1 Purpose

This specification defines the **Meta-Log CanvasL Protocol**, a unified system that combines:

1. **CanvasL Format**: Extended JSONL format with R5RS function integration, dimension references, and node references
2. **Meta-Log Framework**: ProLog, DataLog, and R5RS integration for querying and reasoning
3. **Federated Provenance**: Embedded provenance tracking without separate databases
4. **Automaton Evolution**: Self-modification tracking and optimization
5. **Knowledge Extraction**: Structured knowledge extraction from documentation
6. **Agent API**: Multi-agent coordination and execution

The protocol enables a self-referential multiverse canvas system where:
- Multiple `automaton.*.jsonl` files are unified through `generate.metaverse.jsonl`
- R5RS Scheme functions provide computational primitives
- ProLog provides logical inference and unification
- DataLog provides fact extraction and query capabilities
- The system creates a self-referential multiverse canvas spanning dimensions 0D-7D

### 1.2 Scope

This specification covers:

- **CanvasL Format**: File format, grammar, directives, R5RS integration, dimension references, node references
- **Meta-Log Framework**: ProLog, DataLog, R5RS integration, RDF/SPARQL support, SHACL validation
- **R5RS Expressions**: Church encoding, lambda calculus foundations, computational manifold architecture
- **JSONL Database Adapter**: Database abstraction layer with R5RS function support
- **Federated Provenance**: Self-reference metadata, reference nodes, unified topology
- **Automaton Evolution**: Snapshot system, memory monitoring, variant generation
- **Knowledge Extraction**: Fact extraction, rule extraction, agent extraction, function extraction
- **Agent API**: Agent discovery, execution, workflows, coordination
- **Meta-Log Database**: Native database implementation with ProLog, DataLog, R5RS engines
- **Automatons CanvasL**: Format detection, backward/forward compatibility, R5RS integration

### 1.3 RFC 2119 Keywords

The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT", "SHOULD", "SHOULD NOT", "RECOMMENDED", "MAY", and "OPTIONAL" in this document are to be interpreted as described in RFC 2119.

### 1.4 Related Specifications

This specification unifies and extends:

- **`docs/01-R5RS-Expressions/R5RS-EXPRESSIONS-RFC2119-SPEC.md`**: R5RS expression foundations and Church encoding
- **`docs/02-JSONL-Database-Adapter/JSONL-DATABASE-ADAPTER-RFC2119-SPEC.md`**: Database adapter architecture
- **`docs/04-CanvasL/CANVASL-RFC2119-SPEC.md`**: CanvasL language specification
- **`docs/05-Meta-Log/MULTIVERSE-CANVAS-RFC2119-SPEC.md`**: Multiverse canvas specification
- **`docs/07-Meta-Log-Db/META-LOG-DB-RFC2119-SPEC.md`**: Meta-Log database implementation
- **`docs/12-Automatons-CanvasL/AUTOMATONS-CANVASL-RFC2119-SPEC.md`**: Automatons CanvasL integration
- **`docs/13-Federated-Provenance-Meta-Log/FEDERATED-PROVENANCE-RFC2119-SPEC.md`**: Federated provenance tracking
- **`docs/14-Automaton-Evolution-Logging/AUTOMATON-EVOLUTION-LOGGING-RFC2119-SPEC.md`**: Evolution logging system
- **`docs/15-Automaton-Evolution-Testing-Optimizing/AUTOMATON-EVOLUTION-TESTING-OPTIMIZING-RFC2119-SPEC.md`**: Testing and optimization
- **`docs/16-Knowledge-Extraction-Propagation/README.md`**: Knowledge extraction and propagation
- **`docs/19-Agent-Procedures-Constraints-API/README.md`**: Agent API documentation

---

## 2. Terminology

### 2.1 Core Terms

- **Meta-Log CanvasL Protocol**: Unified protocol combining Meta-Log framework and CanvasL format
- **CanvasL**: Extended JSONL format with directives, R5RS functions, dimension references, and node references (`.canvasl` extension)
- **JSONL**: JSON Lines format - one JSON object per line (`.jsonl` extension)
- **Meta-Log**: Framework integrating ProLog, DataLog, and R5RS for querying and reasoning
- **Multiverse Canvas**: Unified canvas spanning multiple automaton files and dimensions
- **R5RS**: Revised^5 Report on the Algorithmic Language Scheme
- **ProLog**: Logic programming language for unification and inference
- **DataLog**: Declarative logic programming language for fact extraction
- **Federated Provenance**: Embedded provenance tracking across multiple files without separate databases
- **Automaton Evolution**: Self-modification tracking, memory monitoring, and variant generation
- **Knowledge Extraction**: Structured knowledge extraction from documentation (facts, rules, agents, functions)
- **Agent API**: Multi-agent system coordination and execution interface

### 2.2 File Types

- **`generate.metaverse.jsonl`** / **`generate.metaverse.canvasl`**: Metaverse generator file that references all automaton files
- **`automaton-kernel.seed.jsonl`**: Minimal seed for kernel regeneration
- **`automaton-kernel.jsonl`** / **`automaton-kernel.canvasl`**: Full kernel with R5RS function trie and dimensional topology
- **`automaton.canvas.space.jsonl`**: Meta-layer for constraint enforcement and bipartite interfaces
- **`automaton.jsonl`** / **`automaton.canvasl`**: Operational automaton with OpenCode operations
- **`r5rs-functions-trie.jsonl`**: R5RS function definitions and registry

### 2.3 Dimensional Progression

- **0D**: Quantum vacuum topology (empty pattern `()`, Church zero)
- **1D**: Temporal topology (line topology ℝ¹, Church successor)
- **2D**: Bipartite topology (product 1D × 1D, Church pairs)
- **3D**: Algebraic/analytical structure (Church algebra, fixed-point analysis)
- **4D**: Network topology (IPv4/IPv6, spacetime, CI/CD operations)
- **5D**: Consensus topology (blockchain, immutable ledger, deployment decisions)
- **6D**: Intelligence topology (neural networks, attention mechanisms, test analysis)
- **7D**: Quantum topology (qubit superposition, entanglement)

### 2.4 Protocol Components

- **CanvasL Parser**: Parses CanvasL files with directives and extensions
- **Meta-Log Database**: Native database with ProLog, DataLog, R5RS engines
- **Provenance Tracker**: Tracks provenance through self-reference metadata
- **Evolution Logger**: Logs automaton evolution and generates variants
- **Knowledge Extractor**: Extracts structured knowledge from documentation
- **Agent Coordinator**: Coordinates multi-agent system operations

---

## 3. Architecture Overview

### 3.1 Three-Layer Architecture

The Meta-Log CanvasL Protocol SHALL implement a three-layer architecture:

```
┌─────────────────────────────────────────────────────────┐
│  TOP LAYER: Fixed Church Encoding Spine (IMMUTABLE)     │
│  - Church numerals: zero, one, succ, add, mult, exp    │
│  - Church booleans: true, false, if, not, and, or       │
│  - Y-combinator: Fixed-point for self-reference         │
│  - Vertical inheritance: 0D→1D→2D→...→7D               │
└─────────────────────────────────────────────────────────┘
                        ↓
┌─────────────────────────────────────────────────────────┐
│  MIDDLE LAYER: Implementation Templates (MUTABLE)     │
│  - Horizontal edges (h:*) define implementation maps    │
│  - Templates map topology → system implementations     │
│  - CanvasL directives configure behavior                │
└─────────────────────────────────────────────────────────┘
                        ↓
┌─────────────────────────────────────────────────────────┐
│  BOTTOM LAYER: JSONL/CanvasL Canvas Blackboard          │
│  - Queryable fact database                              │
│  - Self-referential via self-ref nodes                 │
│  - Federated provenance via self-reference metadata     │
│  - ProLog/DataLog/R5RS queryable                        │
└─────────────────────────────────────────────────────────┘
```

### 3.2 Data Flow

The protocol SHALL implement the following data flow:

```
CanvasL File (automaton.*.canvasl)
    ↓ [parse with CanvasL parser]
Parsed Objects (with directives, R5RS calls, references)
    ↓ [extract facts with DataLog]
DataLog Facts (node, edge, vertical, horizontal, provenance)
    ↓ [convert to RDF]
RDF Triples (semantic relationships)
    ↓ [query with ProLog/DataLog/SPARQL]
Query Results (unification, inference, semantic queries)
    ↓ [execute R5RS functions]
Computed Results (Church encoding, computations)
    ↓ [validate with SHACL]
Validation Results (constraint checking)
    ↓ [modify CanvasL]
CanvasL File (self-modification, evolution)
```

### 3.3 Integration Points

The protocol MUST integrate:

1. **CanvasL Parser**: Parses CanvasL format with directives and extensions
2. **R5RS Engine**: Executes R5RS Scheme functions (Church encoding, computations)
3. **ProLog Engine**: Unification and resolution for logical inference
4. **DataLog Engine**: Fact extraction and querying from JSONL/CanvasL entries
5. **RDF Triple Store**: Semantic relationships and SPARQL queries
6. **SHACL Validator**: Shape constraints for JSONL/CanvasL entries
7. **Provenance Tracker**: Embedded provenance tracking
8. **Evolution Logger**: Self-modification tracking and variant generation
9. **Knowledge Extractor**: Structured knowledge extraction
10. **Agent Coordinator**: Multi-agent system coordination

### 3.4 Protocol Layers

The protocol SHALL implement the following layers:

```
┌─────────────────────────────────────────────────────────┐
│  APPLICATION LAYER                                      │
│  - Agent API, Knowledge Extraction, Evolution Logging   │
└─────────────────────────────────────────────────────────┘
                        ↓
┌─────────────────────────────────────────────────────────┐
│  LOGIC LAYER                                            │
│  - ProLog Engine, DataLog Engine, R5RS Engine           │
└─────────────────────────────────────────────────────────┘
                        ↓
┌─────────────────────────────────────────────────────────┐
│  DATA LAYER                                             │
│  - RDF Triple Store, SHACL Validator, Provenance        │
└─────────────────────────────────────────────────────────┘
                        ↓
┌─────────────────────────────────────────────────────────┐
│  FORMAT LAYER                                           │
│  - CanvasL Parser, JSONL Parser, Format Detection       │
└─────────────────────────────────────────────────────────┘
                        ↓
┌─────────────────────────────────────────────────────────┐
│  STORAGE LAYER                                          │
│  - JSONL Files, CanvasL Files, Meta-Log Database       │
└─────────────────────────────────────────────────────────┘
```

---

## 4. CanvasL Format Specification

**Reference**: See `docs/04-CanvasL/CANVASL-RFC2119-SPEC.md` for complete CanvasL specification.

### 4.1 File Extension

- CanvasL files MUST use the `.canvasl` file extension
- CanvasL files MUST be valid JSONL files (backward compatible)
- CanvasL files MAY include directives starting with `@`
- CanvasL files MUST support both `.jsonl` and `.canvasl` extensions (format detection)

### 4.2 File Structure

A CanvasL file SHALL have the following structure:

```
[Directives]*
[JSONL Entries]*
```

Where:
- **Directives** (OPTIONAL): Zero or more directive lines (`@directive: value`)
- **JSONL Entries** (REQUIRED): One or more JSON objects, one per line

### 4.3 Directives

#### 4.3.1 Directive Syntax

Directives MUST follow this syntax:

```canvasl
@directive-name: value
```

Where:
- `directive-name` MUST start with `@` followed by an identifier
- `value` MUST be a valid JSON value (string, number, boolean, null, object, array)
- Directives MUST appear before JSONL entries
- Directives MAY appear on separate lines

#### 4.3.2 Standard Directives

The following directives are RECOMMENDED:

**Version Directive**:
```canvasl
@version: "1.0"
```
- Specifies CanvasL format version
- Value MUST be a string
- SHOULD be present in CanvasL files

**Schema Directive**:
```canvasl
@schema: "canvasl-v1"
```
- Specifies CanvasL schema version
- Value MUST be a string
- SHOULD be present in CanvasL files

**R5RS Engine Directive**:
```canvasl
@r5rs-engine: "r5rs-canvas-engine.scm"
```
- Specifies R5RS engine file
- Value MUST be a string (file path)
- MAY be used to specify custom R5RS engine

### 4.4 R5RS Function Calls

#### 4.4.1 Function Call Format

R5RS functions MUST be referenced using the `r5rs:` prefix:

```json
{
  "id": "r5rs-compute",
  "type": "r5rs-call",
  "function": "r5rs:church-add",
  "args": [2, 3]
}
```

#### 4.4.2 Function Name Requirements

- R5RS function names MUST be prefixed with `r5rs:`
- Function names MUST match entries in `r5rs-functions-trie.jsonl`
- Function names MUST be valid identifiers: `[a-zA-Z_][a-zA-Z0-9_-]*`

#### 4.4.3 Expression Format

R5RS functions MAY be invoked via Scheme expressions:

```json
{
  "id": "r5rs-compute",
  "type": "r5rs-call",
  "expression": "(church-add 2 3)"
}
```

- `expression` MUST be valid R5RS Scheme syntax
- `expression` MUST be evaluable by the R5RS engine
- `expression` MAY reference R5RS functions

### 4.5 Dimension References

#### 4.5.1 Dimension Format

Dimensions MUST be specified in format `[0-7]D`:

- `0D`: Quantum vacuum topology
- `1D`: Temporal topology
- `2D`: Bipartite topology
- `3D`: Algebraic/analytical structure
- `4D`: Network topology
- `5D`: Consensus topology
- `6D`: Intelligence topology
- `7D`: Quantum topology

#### 4.5.2 Dimension Field

Dimensions MAY be specified in node entries:

```json
{
  "id": "0D-topology",
  "type": "text",
  "dimension": "0D",
  "text": "Quantum Vacuum"
}
```

- `dimension` field is OPTIONAL
- `dimension` value MUST be `0D`, `1D`, `2D`, `3D`, `4D`, `5D`, `6D`, or `7D`
- `dimension` MAY be used in node IDs

### 4.6 Node References

#### 4.6.1 Reference Syntax

Node references MUST use `#id` syntax:

```json
{
  "id": "edge-1",
  "type": "vertical",
  "fromNode": "#0D-topology",
  "toNode": "#1D-topology"
}
```

- References MUST start with `#`
- References MUST be followed by a valid node ID
- References MAY be used in `fromNode`, `toNode`, or other fields

#### 4.6.2 Reference Resolution

- Referenced nodes MUST exist in the same file or referenced files
- Node references MUST resolve to valid node IDs
- Reference resolution MUST be performed during AST generation
- Unresolved references MUST be reported as errors

### 4.7 Grammar Specification

#### 4.7.1 Grammar Definition

CanvasL SHALL use a Lezer grammar (`ui/src/grammars/canvasl.grammar`) that extends JSONL.

#### 4.7.2 Top-Level Rule

```grammar
CanvasL {
  CanvasLEntry*
}

CanvasLEntry {
  CanvasLDirective? JSONLObject
}
```

#### 4.7.3 Token Definitions

The grammar MUST define the following tokens:

**Standard JSONL Tokens**:
- `jsonObjectStart`: `{`
- `jsonObjectEnd`: `}`
- `jsonString`: `"..."` (quoted string)
- `jsonNumber`: Numeric literals
- `jsonBoolean`: `true` | `false`
- `jsonNull`: `null`

**CanvasL-Specific Tokens**:
- `canvaslDirective`: `@[a-zA-Z_][a-zA-Z0-9_-]*` - Directives for metadata
- `canvaslReference`: `#[a-zA-Z0-9_-]+` - References to other nodes
- `canvaslDimension`: `[0-7]D` - Dimension identifiers (0D-7D)
- `canvaslR5RSFunction`: `r5rs:[a-zA-Z_][a-zA-Z0-9_-]*` - R5RS function references
- `canvaslSchemeExpression`: `([^)]*)` - Scheme expressions in parentheses

### 4.8 Backward Compatibility

- CanvasL files MUST be valid JSONL files
- Standard JSONL entries (without CanvasL extensions) MUST be supported
- CanvasL extensions are OPTIONAL and MUST NOT break standard JSONL parsing
- Format detection MUST support both `.jsonl` and `.canvasl` files

**Reference**: See `docs/12-Automatons-CanvasL/AUTOMATONS-CANVASL-RFC2119-SPEC.md` for complete compatibility requirements.

---

## 5. Meta-Log Framework Integration

**Reference**: See `docs/05-Meta-Log/MULTIVERSE-CANVAS-RFC2119-SPEC.md` for complete Meta-Log specification.

### 5.1 Framework Components

The Meta-Log framework MUST integrate:

1. **ProLog Engine**: Unification and resolution for logical inference
2. **DataLog Engine**: Fact extraction and querying from JSONL/CanvasL entries
3. **R5RS Engine**: Scheme function execution and Church encoding
4. **RDF Triple Store**: Semantic relationships and SPARQL queries
5. **SHACL Validator**: Shape constraints for JSONL/CanvasL entries

### 5.2 Integration Architecture

```
CanvasL/JSONL Files
    ↓ [parse]
Parsed Objects
    ↓ [extract facts]
DataLog Facts
    ↓ [convert to RDF]
RDF Triples
    ↓ [build ProLog DB]
ProLog Database
    ↓ [query]
Query Results
```

### 5.3 Query Interfaces

The framework MUST provide multiple query interfaces:

1. **ProLog Queries**: `r5rs:prolog-query(db, goal)` - Unification and resolution
2. **DataLog Queries**: `r5rs:datalog-query(program, goal)` - Fixed-point evaluation
3. **SPARQL Queries**: `r5rs:sparql-query(query-str, triples)` - RDF triple queries
4. **R5RS Invocation**: `r5rs:invoke-from-jsonl(function-name, args, context)` - Function calls

### 5.4 Constraint Types

The framework MUST enforce multiple constraint types:

1. **RFC2119**: Implementation requirement constraints (MUST, SHOULD, MAY)
2. **SHACL**: Shape constraints for JSONL/CanvasL entries
3. **ASP**: Answer Set Programming rules (stable model semantics)
4. **Prolog**: Logic programming rules (unification, resolution)
5. **Datalog**: DataLog fact extraction rules (fixed-point evaluation)

---

## 6. R5RS Expression Foundation

**Reference**: See `docs/01-R5RS-Expressions/R5RS-EXPRESSIONS-RFC2119-SPEC.md` for complete R5RS specification.

### 6.1 Church Encoding Primitives

The protocol MUST provide Church encoding primitives as pure R5RS functions:

#### 6.1.1 Church Numerals

```scheme
;; Church numerals (vertical spine: top layer)
(define zero  (lambda (f) (lambda (x) x)))
(define one   (lambda (f) (lambda (x) (f x))))
(define succ  (lambda (n) (lambda (f) (lambda (x) (f ((n f) x))))))
(define add   (lambda (m n) (lambda (f) (lambda (x) ((m f) ((n f) x))))))
(define mult  (lambda (m n) (lambda (f) (m (n f)))))
(define exp   (lambda (m n) (n m)))
```

#### 6.1.2 Church Booleans

```scheme
;; Church booleans
(define true  (lambda (t f) t))
(define false (lambda (t f) f))
(define if    (lambda (c t e) (c t e)))
(define not   (lambda (b) (b false true)))
(define and   (lambda (a b) (a b a)))
(define or    (lambda (a b) (a a b)))
```

#### 6.1.3 Y-Combinator

```scheme
;; Y-combinator (fixed-point for self-reference)
(define Y (lambda (f) ((lambda (x) (f (lambda (y) ((x x) y))))
                      (lambda (x) (f (lambda (y) ((x x) y)))))))
```

### 6.2 Blackboard System

The protocol MUST implement a blackboard system where JSONL/CanvasL canvas files serve as the blackboard:

```scheme
;; Blackboard: JSONL/CanvasL canvas as list of facts
(define *blackboard* '())

(define (load-canvas! filename)
  (set! *blackboard* '())
  (call-with-input-file filename
    (lambda (port)
      (let loop ((line (read-line port)))
        (if (eof-object? line)
            'done
            (begin
              (when (not (string=? line ""))
                (let ((obj (json->scheme line)))
                  (when obj
                    (set! *blackboard* (cons obj *blackboard*)))))
              (loop (read-line port))))))))

;; Query blackboard
(define (bb-query predicate)
  (filter predicate *blackboard*))

(define (bb-get id)
  (find (lambda (node) (equal? (assoc 'id node) id)) *blackboard*))
```

### 6.3 Required R5RS Functions

The protocol MUST provide the following R5RS functions:

#### 6.3.1 JSONL/CanvasL Parsing

- `r5rs:parse-jsonl-canvas(filename)` → List of parsed objects
- `r5rs:parse-canvasl-file(filename)` → Parsed CanvasL file with directives
- `r5rs:extract-facts(parsed-objects)` → DataLog facts
- `r5rs:query-facts(facts, query-pattern)` → Query results

#### 6.3.2 RDF Operations

- `r5rs:jsonl-to-rdf(facts)` → RDF triples
- `r5rs:rdf-query(triples, pattern)` → RDF query results
- `r5rs:sparql-query(query-str, triples)` → SPARQL query results
- `r5rs:rdfs-entailment(triples)` → RDFS-entailed triples

#### 6.3.3 ProLog Operations

- `r5rs:build-prolog-db(facts)` → ProLog database
- `r5rs:prolog-query(db, goal)` → ProLog query results
- `r5rs:unify(term1, term2)` → Unification result
- `r5rs:resolve(goal, db)` → Resolution result

#### 6.3.4 DataLog Operations

- `r5rs:datalog-query(program, goal)` → DataLog query results
- `r5rs:build-datalog-program(facts, rules)` → DataLog program
- `r5rs:fixed-point(program)` → Fixed-point computation

#### 6.3.5 SHACL Validation

- `r5rs:load-shacl-shapes(shapes-file)` → SHACL shapes
- `r5rs:shacl-validate(shapes, triples)` → Validation results
- `r5rs:shacl-report(validation-results)` → SHACL validation report

#### 6.3.6 Function Invocation

- `r5rs:invoke-from-jsonl(function-name, args, context)` → Function result
- `r5rs:register-function(name, implementation)` → Registration result
- `r5rs:query-function-registry(pattern)` → Matching functions

---

## 7. JSONL Database Adapter

**Reference**: See `docs/02-JSONL-Database-Adapter/JSONL-DATABASE-ADAPTER-RFC2119-SPEC.md` for complete adapter specification.

### 7.1 Database Adapter Interface

The protocol MUST provide a unified database adapter interface:

```typescript
interface DatabaseAdapter {
  connect(): Promise<void>;
  disconnect(): Promise<void>;
  read(file: string): Promise<string>;
  write(file: string, data: string): Promise<void>;
  query(pattern: string): Promise<any[]>;
  invokeR5RS(functionName: string, args: any[]): Promise<any>;
  parseCanvasL(file: string): Promise<CanvasLFile>;
  extractFacts(parsed: CanvasLFile): Promise<DataLogFacts>;
  queryProLog(db: ProLogDB, goal: string): Promise<any[]>;
  queryDataLog(program: DataLogProgram, goal: string): Promise<any[]>;
  querySPARQL(query: string, triples: RDFTriples): Promise<any[]>;
  validateSHACL(shapes: SHACLShapes, triples: RDFTriples): Promise<ValidationResult>;
}
```

### 7.2 Required Methods

All database adapters MUST implement:

- **`connect()`**: Establish database connection
- **`disconnect()`**: Close database connection
- **`read(file)`**: Read file/data from database
- **`write(file, data)`**: Write file/data to database
- **`query(pattern)`**: Query database with pattern
- **`invokeR5RS(functionName, args)`**: Invoke R5RS function
- **`parseCanvasL(file)`**: Parse CanvasL file with directives
- **`extractFacts(parsed)`**: Extract DataLog facts from parsed CanvasL
- **`queryProLog(db, goal)`**: Execute ProLog query
- **`queryDataLog(program, goal)`**: Execute DataLog query
- **`querySPARQL(query, triples)`**: Execute SPARQL query
- **`validateSHACL(shapes, triples)`**: Validate with SHACL

### 7.3 Supported Database Types

The protocol MUST support:

- **JSONL Adapter**: File-based JSONL/CanvasL storage (REQUIRED)
- **Redis Adapter**: In-memory caching (RECOMMENDED)
- **PostgreSQL Adapter**: Relational storage (OPTIONAL)
- **MongoDB Adapter**: Document storage (OPTIONAL)
- **SQLite Adapter**: Embedded storage (OPTIONAL)

### 7.4 R5RS Function Storage

R5RS functions MUST be stored as JSONL/CanvasL entries:

```json
{
  "type": "r5rs-function",
  "name": "r5rs:church-zero",
  "code": "(lambda (f) (lambda (x) x))",
  "metadata": {
    "module": "MODULE 1: Church Encoding",
    "description": "Church encoding of zero"
  }
}
```

---

## 8. ProLog Integration

**Reference**: See `docs/05-Meta-Log/MULTIVERSE-CANVAS-RFC2119-SPEC.md` Section 6 for complete ProLog specification.

### 8.1 ProLog Engine Requirements

The protocol MUST provide:

- **Unification**: Variable unification algorithm with occur check
- **Resolution**: SLD resolution algorithm (Linear resolution with selection function)
- **Query Execution**: ProLog query execution with backtracking
- **Database Building**: ProLog database construction from facts and rules

### 8.2 ProLog Syntax

The protocol MUST support:

#### 8.2.1 Facts

```prolog
node(node1, text).
node(node2, text).
edge(edge1, vertical, node1, node2).
```

#### 8.2.2 Rules

```prolog
inherits(X, Z) :- vertical(Y, X), inherits(Y, Z).
implements(X, Y) :- horizontal(H, X, Y).
```

#### 8.2.3 Queries

```prolog
?- node(?Id, ?Type).
?- inherits(?X, ?Z).
?- implements(?X, ?Y).
```

### 8.3 ProLog Functions

The protocol MUST provide:

- `r5rs:build-prolog-db(facts)` → ProLog database
- `r5rs:prolog-query(db, goal)` → ProLog query results
- `r5rs:unify(term1, term2)` → Unification result
- `r5rs:resolve(goal, db)` → Resolution result

### 8.4 Built-in Predicates

The protocol MUST support built-in predicates:

- `same(X, Y)`: X and Y are the same
- `inherits(X, Y)`: X inherits from Y
- `implements(X, Y)`: X implements Y
- `shacl-violation(N)`: Node N violates SHACL constraints
- `provenance(Id, File, Line, Pattern)`: Provenance information

---

## 9. DataLog Integration

**Reference**: See `docs/05-Meta-Log/MULTIVERSE-CANVAS-RFC2119-SPEC.md` Section 7 for complete DataLog specification.

### 9.1 DataLog Engine Requirements

The protocol MUST provide:

- **Fact Extraction**: Extract facts from JSONL/CanvasL entries
- **Fixed-Point Computation**: DataLog fixed-point evaluation
- **Query Execution**: DataLog query execution
- **Program Building**: DataLog program construction from facts and rules

### 9.2 DataLog Syntax

The protocol MUST support:

#### 9.2.1 Facts

```datalog
node(node1, text).
node(node2, text).
edge(edge1, vertical, node1, node2).
```

#### 9.2.2 Rules

```datalog
inherits(X, Z) :- vertical(Y, X), inherits(Y, Z).
implements(X, Y) :- horizontal(H, X, Y).
```

#### 9.2.3 Queries

```datalog
?- node(?Id, ?Type).
?- inherits(?X, ?Z).
?- implements(?X, ?Y).
```

### 9.3 DataLog Functions

The protocol MUST provide:

- `r5rs:extract-facts(parsed-objects)` → DataLog facts
- `r5rs:datalog-query(program, goal)` → DataLog query results
- `r5rs:build-datalog-program(facts, rules)` → DataLog program
- `r5rs:fixed-point(program)` → Fixed-point computation

### 9.4 Stratification Requirements

- Rules MUST be stratified (no negation cycles)
- Fixed-point computation MUST respect stratification
- Negation MUST be handled via stratification

### 9.5 Aggregation Support

The protocol MUST support aggregation:

- `count(X)`: Count of X
- `bagof(X, Goal, Bag)`: Bag of X satisfying Goal
- `length(List, Length)`: Length of List

---

## 10. RDF and SPARQL Support

**Reference**: See `docs/05-Meta-Log/MULTIVERSE-CANVAS-RFC2119-SPEC.md` Section 8 for complete RDF specification.

### 10.1 RDF Conversion Requirements

The protocol MUST convert JSONL/CanvasL entries to RDF triples:

```scheme
(define (jsonl-to-rdf facts)
  (let ((triples '()))
    (for-each
      (lambda (fact)
        (let ((id (cdr (assoc 'id fact)))
              (type (cdr (assoc 'type fact))))
          (set! triples (cons `(,id rdf:type ,type) triples))
          ;; Add more triples based on fact properties
          ))
      facts)
    triples))
```

### 10.2 RDF Triple Structure

RDF triples MUST follow the structure:

```
(subject, predicate, object)
```

Where:
- **Subject**: Node ID or URI
- **Predicate**: RDF property (rdf:type, rdfs:subClassOf, etc.)
- **Object**: Node ID, URI, or literal value

### 10.3 SPARQL Query Support

The protocol MUST support SPARQL queries:

```sparql
SELECT ?id ?type WHERE {
  ?id rdf:type ?type .
  ?id dimension "0D" .
}
```

### 10.4 RDF Functions

The protocol MUST provide:

- `r5rs:jsonl-to-rdf(facts)` → RDF triples
- `r5rs:rdf-query(triples, pattern)` → RDF query results
- `r5rs:sparql-query(query-str, triples)` → SPARQL query results
- `r5rs:rdfs-entailment(triples)` → RDFS-entailed triples

### 10.5 RDF Namespaces

The protocol MUST support RDF namespaces:

- `rdf:`: RDF vocabulary
- `rdfs:`: RDFS vocabulary
- `owl:`: OWL vocabulary
- `sh:`: SHACL vocabulary
- `prov:`: PROV-O provenance vocabulary
- `metaverse:`: Metaverse-specific vocabulary

---

## 11. SHACL Validation

**Reference**: See `docs/05-Meta-Log/MULTIVERSE-CANVAS-RFC2119-SPEC.md` Section 11 for complete SHACL specification.

### 11.1 SHACL Validation Requirements

The protocol MUST validate JSONL/CanvasL entries against SHACL shapes:

```scheme
(define (shacl-validate shapes triples)
  (let ((violations '()))
    (for-each
      (lambda (shape)
        (let ((constraint (cdr (assoc 'constraint shape)))
              (target-class (cdr (assoc 'targetClass shape))))
          ;; Validate constraint against triples
          (let ((violating-nodes (check-constraint constraint target-class triples)))
            (set! violations (append violations violating-nodes)))))
      shapes)
    violations))
```

### 11.2 SHACL Shape Structure

SHACL shapes MUST follow the structure:

```json
{
  "type": "shacl-shape",
  "targetClass": "automaton",
  "constraints": [
    {
      "path": "rdfs:label",
      "minCount": 1,
      "datatype": "xsd:string"
    },
    {
      "path": "owl:sameAs",
      "minCount": 1
    }
  ]
}
```

### 11.3 SHACL Functions

The protocol MUST provide:

- `r5rs:load-shacl-shapes(shapes-file)` → SHACL shapes
- `r5rs:shacl-validate(shapes, triples)` → Validation results
- `r5rs:shacl-report(validation-results)` → SHACL validation report

### 11.4 Validation Constraints

The protocol MUST validate:

- **Label validation**: `rdfs:label` MUST be string
- **Identity validation**: `owl:sameAs` minimum count 1
- **Technology validation**: `prov:used` MUST match specifications
- **Provenance validation**: `selfReference` MUST have file and line

---

## 12. Federated Provenance Tracking

**Reference**: See `docs/13-Federated-Provenance-Meta-Log/FEDERATED-PROVENANCE-RFC2119-SPEC.md` for complete provenance specification.

### 12.1 Provenance Architecture

The protocol MUST implement federated provenance tracking through three mechanisms:

1. **Self-Reference Metadata**: Embedded `file` and `line` provenance in JSONL entries
2. **Reference Nodes**: Explicit file-to-file relationships in `generate.metaverse.jsonl`
3. **Unified Topology**: Epistemic and semantic relationships encoded as RDF triples

### 12.2 Self-Reference Metadata

#### 12.2.1 Structure Requirements

Each JSONL/CanvasL entry that requires provenance tracking MUST include a `selfReference` field:

```json
{
  "id": "0D-automaton",
  "type": "automaton",
  "currentState": "identity",
  "dimensionalLevel": 0,
  "selfReference": {
    "file": "automaton-kernel.jsonl",
    "line": 14,
    "pattern": "identity"
  },
  "provenanceHistory": [
    {
      "file": "automaton-kernel.jsonl",
      "line": 14,
      "pattern": "identity"
    },
    {
      "file": "automaton.jsonl",
      "line": 2103,
      "pattern": "Identity Evolution (0D)"
    }
  ]
}
```

#### 12.2.2 Field Requirements

- **`file`**: MUST be present, MUST be a string containing source filename
- **`line`**: MUST be present, MUST be a positive integer (1-based line number)
- **`pattern`**: SHOULD be present, MAY be a string describing semantic pattern
- **`provenanceHistory`**: MAY be present, array of provenance entries for cross-file tracking

### 12.3 Reference Nodes

Reference nodes in `generate.metaverse.jsonl` MUST have the following structure:

```json
{
  "id": "metaverse-ref-kernel",
  "type": "reference",
  "target": "automaton-kernel.jsonl",
  "metadata": {
    "regenerate": {
      "function": "r5rs:parse-jsonl-canvas",
      "args": ["automaton-kernel.jsonl"]
    },
    "reference": {
      "file": "automaton-kernel.jsonl",
      "type": "kernel",
      "role": "full-implementation"
    }
  }
}
```

### 12.4 Unified Topology

The protocol MUST create unified topology encoding provenance relationships:

#### 12.4.1 Epistemic Topology

```prolog
knows(canvas-space, kernel).
knows(kernel, automaton).
generates(seed, kernel).
validates(canvas-space, kernel).
```

#### 12.4.2 Semantic Topology

```prolog
means(canvas-space, constraint-enforcement).
means(kernel-seed, bootstrap).
means(kernel, full-implementation).
means(automaton, operational).
```

### 12.5 Provenance Queries

The protocol MUST support provenance queries via:

- **ProLog**: `provenance(Id, File, Line, Pattern)`
- **DataLog**: Extract provenance facts from `selfReference` metadata
- **SPARQL**: Query `prov:wasDerivedFrom` relationships

### 12.6 Provenance-Aware Deduplication

When encountering objects with duplicate IDs, the protocol MUST implement provenance-aware deduplication:

1. **Same-File Duplicates**: Merge provenance history, keep latest version
2. **Cross-File Duplicates**: Preserve both objects (federated provenance requirement)

---

## 13. Automaton Evolution System

**Reference**: See `docs/14-Automaton-Evolution-Logging/AUTOMATON-EVOLUTION-LOGGING-RFC2119-SPEC.md` and `docs/15-Automaton-Evolution-Testing-Optimizing/AUTOMATON-EVOLUTION-TESTING-OPTIMIZING-RFC2119-SPEC.md` for complete evolution specification.

### 13.1 Evolution Architecture

The protocol MUST support automaton evolution through:

1. **Snapshot System**: Capture automaton state at points in time
2. **Memory Monitoring**: Track memory usage patterns
3. **Variant Generation**: Generate optimized automaton variants
4. **Evolution Analysis**: Analyze self-modification patterns

### 13.2 Snapshot System

#### 13.2.1 Snapshot Requirements

The protocol MUST:

- **Capture Snapshots**: Capture automaton state snapshots
- **Store Snapshots**: Store snapshots in Meta-Log-Db
- **Query Snapshots**: Query snapshots for analysis

#### 13.2.2 Snapshot Content

Snapshots MUST include:

- **Memory Metrics**: Heap usage, RSS, object counts
- **State Information**: Current automaton state
- **Execution History**: Execution history length
- **Timestamp**: Snapshot timestamp
- **Dimensional Level**: Current dimensional level (0D-7D)

### 13.3 Memory Monitoring

#### 13.3.1 Monitoring Requirements

The protocol MUST:

- **Track Memory Usage**: Track heap and RSS usage
- **Monitor Object Counts**: Monitor object counts
- **Detect Leaks**: Detect memory leaks
- **Generate Reports**: Generate memory reports

#### 13.3.2 Monitoring Metrics

The protocol MUST track:

- **Heap Used**: Heap memory used
- **Heap Total**: Total heap memory
- **RSS**: Resident Set Size
- **Object Count**: Number of objects

### 13.4 Variant Generation

#### 13.4.1 Variant Types

The protocol MUST support:

- **Llama Variant**: Optimized for Llama 3.2 inference
- **GPT Variant**: Optimized for GPT-OSS 20B models
- **Native Variant**: Native execution without LLM dependencies
- **Fast Variant**: Fast execution mode with reduced complexity

#### 13.4.2 Generation Requirements

The protocol MUST:

- **Analyze Patterns**: Analyze evolution patterns
- **Generate Variants**: Generate optimized variants
- **Validate Variants**: Validate generated variants

### 13.5 Evolution Analysis

#### 13.5.1 Analysis Requirements

The protocol MUST:

- **Analyze Patterns**: Analyze self-modification patterns
- **Track Changes**: Track changes over time
- **Identify Trends**: Identify evolution trends
- **Generate Insights**: Generate evolution insights

### 13.6 Testing and Optimization

#### 13.6.1 Testing Framework

The protocol MUST provide:

- **Variant Testing**: Test all generated variants
- **Automated Testing**: Automated test execution
- **Test Reporting**: Comprehensive test reporting
- **Test Coverage**: Test coverage metrics

#### 13.6.2 Optimization Strategies

The protocol MUST:

- **Identify Bottlenecks**: Identify performance bottlenecks
- **Apply Optimizations**: Apply optimization strategies
- **Measure Impact**: Measure optimization impact
- **Validate Results**: Validate optimization results

---

## 14. Knowledge Extraction and Propagation

**Reference**: See `docs/16-Knowledge-Extraction-Propagation/README.md` for complete knowledge extraction specification.

### 14.1 Knowledge Extraction System

The protocol MUST support structured knowledge extraction from documentation:

#### 14.1.1 Extraction Capabilities

The protocol MUST extract:

- **Facts**: Definitions, requirements, examples, capabilities (1263+ facts)
- **Rules**: RFC2119 keywords (MUST, SHOULD, MAY statements) (164+ rules)
- **Agents**: Agent definitions from AGENTS.md frontmatter (15 agents)
- **Functions**: R5RS function calls and signatures (92+ functions)
- **Relationships**: Prerequisites, enables, related links (577+ relationships)

### 14.2 Knowledge Base Structure

The protocol MUST maintain a knowledge base with:

```typescript
interface KnowledgeBase {
  facts: Fact[];
  rules: Rule[];
  agents: Agent[];
  functions: Function[];
  relationships: Relationship[];
  
  queryFacts(pattern: string): Fact[];
  queryRules(keyword: string): Rule[];
  queryAgents(dimension?: string): Agent[];
  queryFunctions(pattern: string): Function[];
  queryRelationships(type: string): Relationship[];
}
```

### 14.3 Knowledge Propagation

The protocol MUST support knowledge propagation:

- **Vertical Propagation**: Knowledge propagates upward through dimensional hierarchy (0D→7D)
- **Horizontal Propagation**: Knowledge propagates across topology↔systems relationships
- **Temporal Propagation**: Knowledge propagates through evolution phases

### 14.4 Natural Language Interface

The protocol MUST support natural language queries:

- **Query Engine**: Natural language query engine for knowledge base
- **Intent Parsing**: Parse user intent and extract entities
- **Response Generation**: Generate intelligent responses based on knowledge

---

## 15. Agent Procedures and Constraints

**Reference**: See `docs/19-Agent-Procedures-Constraints-API/README.md` for complete agent API specification.

### 15.1 Agent API Architecture

The protocol MUST provide an Agent API for multi-agent system coordination:

#### 15.1.1 Agent Discovery

The protocol MUST support:

- **List Agents**: List all available agents
- **Get Agent Details**: Get individual agent details
- **Filter by Dimension**: Filter agents by dimensional level (0D-7D)

#### 15.1.2 Agent Execution

The protocol MUST support:

- **Execute Operations**: Execute operations on agents
- **Support Operations**: Query, analyze, execute operations
- **JSON Parameters**: Support JSON parameter passing

### 15.2 Workflow Engine

The protocol MUST provide a workflow engine:

#### 15.2.1 Workflow Types

- **Sequential Workflow**: Execute operations sequentially
- **Parallel Workflow**: Execute operations in parallel
- **Conditional Workflow**: Execute based on conditions
- **Loop Workflow**: Execute operations in loops

### 15.3 Coordination Engine

The protocol MUST provide a coordination engine:

#### 15.3.1 Coordination Patterns

- **Parallel Coordination**: Coordinate parallel agent execution
- **Sequential Coordination**: Coordinate sequential agent execution
- **Hierarchical Coordination**: Coordinate hierarchical agent structures

### 15.4 Status Monitoring

The protocol MUST provide status monitoring:

- **Status Tracking**: Track agent execution status
- **Status History**: Maintain status history
- **Status Dashboard**: Provide status dashboard UI

---

## 16. Meta-Log Database Implementation

**Reference**: See `docs/07-Meta-Log-Db/META-LOG-DB-RFC2119-SPEC.md` for complete database specification.

### 16.1 Database Engine Requirements

The protocol MUST provide `MetaLogDb` class with:

- **Initialization**: Database initialization
- **Query Methods**: ProLog, DataLog, SPARQL queries
- **R5RS Invocation**: R5RS function calls
- **File Operations**: JSONL/CanvasL file loading

### 16.2 Database Interface

The database MUST support:

- **Multiple Queries**: Concurrent query execution
- **Transaction Support**: Transaction-based operations
- **Error Handling**: Comprehensive error handling

### 16.3 Engine Integration

The database MUST integrate:

- **ProLog Engine**: Unification and resolution
- **DataLog Engine**: Fact extraction and fixed-point computation
- **R5RS Registry**: Function loading and execution
- **RDF Triple Store**: SPARQL support
- **SHACL Validator**: Constraint checking

### 16.4 Implementation Status

The Meta-Log Database implementation MUST be complete with:

- ✅ ProLog engine with unification and resolution
- ✅ DataLog engine with fact extraction and fixed-point computation
- ✅ R5RS registry with function loading and execution
- ✅ JSONL/CanvasL parser with RDF conversion
- ✅ RDF triple store with SPARQL support
- ✅ SHACL validator with constraint checking

---

## 17. Automatons CanvasL Integration

**Reference**: See `docs/12-Automatons-CanvasL/AUTOMATONS-CANVASL-RFC2119-SPEC.md` for complete integration specification.

### 17.1 Format Detection

The protocol MUST support automatic format detection:

#### 17.1.1 Detection Methods

- **File Extension**: `.jsonl` vs `.canvasl` extension
- **Content Analysis**: Analyze file content for directives
- **Header Detection**: Detect CanvasL directives

#### 17.1.2 Detection Requirements

- **Detect Format**: Automatically detect JSONL vs CanvasL
- **Handle Both Formats**: Support both JSONL and CanvasL
- **Preserve Format**: Preserve original format when saving

### 17.2 Backward Compatibility

#### 17.2.1 JSONL Support

The protocol MUST:

- **Read JSONL**: Read standard JSONL files
- **Write JSONL**: Write standard JSONL files
- **Maintain Compatibility**: Maintain JSONL compatibility

#### 17.2.2 Compatibility Requirements

- **Handle Legacy Files**: Support legacy JSONL files
- **Preserve Structure**: Preserve JSONL structure
- **Support Migration**: Support migration to CanvasL

### 17.3 Forward Compatibility

#### 17.3.1 CanvasL Support

The protocol MUST:

- **Read CanvasL**: Read CanvasL files with directives
- **Write CanvasL**: Write CanvasL files with directives
- **Support Extensions**: Support CanvasL extensions

#### 17.3.2 Extension Support

The protocol MUST support:

- **Directives**: `@version`, `@schema`, `@r5rs-engine`
- **R5RS Calls**: R5RS function calls
- **Dimension References**: Dimension references
- **Node References**: Node references

### 17.4 R5RS Integration in Automatons

The protocol MUST support R5RS function calls in automaton files:

- **Function Invocation**: Invoke R5RS functions from automaton entries
- **Argument Passing**: Pass arguments to functions
- **Result Handling**: Handle function results

---

## 18. File Format Requirements

### 18.1 CanvasL File Structure

CanvasL files MUST follow this structure:

```
[Directives]*
[JSONL Entries]*
```

Where:
- **Directives** (OPTIONAL): Zero or more directive lines
- **JSONL Entries** (REQUIRED): One or more JSON objects, one per line

### 18.2 Standard File Types

The protocol MUST support the following standard file types:

#### 18.2.1 Metaverse Generator

- **File**: `generate.metaverse.jsonl` or `generate.metaverse.canvasl`
- **Purpose**: References all automaton files
- **Structure**: Contains reference nodes pointing to other files

#### 18.2.2 Kernel Files

- **File**: `automaton-kernel.seed.jsonl` (minimal seed)
- **File**: `automaton-kernel.jsonl` or `automaton-kernel.canvasl` (full kernel)
- **Purpose**: Core automaton with R5RS function trie
- **Structure**: Contains foundational topology and R5RS functions

#### 18.2.3 Operational Files

- **File**: `automaton.jsonl` or `automaton.canvasl`
- **Purpose**: Operational automaton with OpenCode operations
- **Structure**: Contains operational automaton data

#### 18.2.4 Constraint Files

- **File**: `automaton.canvas.space.jsonl`
- **Purpose**: Constraint enforcement and bipartite interfaces
- **Structure**: Contains SHACL shapes and constraint rules

#### 18.2.5 Function Registry

- **File**: `r5rs-functions-trie.jsonl`
- **Purpose**: R5RS function definitions and registry
- **Structure**: Contains R5RS function entries

### 18.3 File Format Detection

The protocol MUST implement format detection:

```typescript
function detectFormat(filePath: string): 'jsonl' | 'canvasl' {
  const lowerPath = filePath.toLowerCase();
  
  if (lowerPath.endsWith('.canvasl')) {
    return 'canvasl';
  }
  
  if (lowerPath.endsWith('.jsonl')) {
    // Check for CanvasL directives
    const content = readFileSync(filePath, 'utf-8');
    if (content.match(/^@\w+:/m)) {
      return 'canvasl'; // Has directives
    }
    return 'jsonl';
  }
  
  // Default to jsonl for backward compatibility
  return 'jsonl';
}
```

### 18.4 File Format Conversion

The protocol MUST support format conversion:

- **JSONL → CanvasL**: Add directives and convert to CanvasL format
- **CanvasL → JSONL**: Remove directives and convert to JSONL format
- **Preserve Data**: Preserve all data during conversion
- **Preserve Provenance**: Preserve provenance metadata during conversion

---

## 19. Implementation Constraints

### 19.1 RFC 2119 Compliance

The protocol MUST comply with RFC 2119 requirements:

- **MUST**: Implement exactly one system per topology dimension
- **SHOULD**: Use specified technologies (Three.js, WebLLM, etc.)
- **MUST**: Maintain SHACL shape compliance
- **MUST**: Preserve federated provenance through all transformations
- **MUST**: Implement provenance-aware deduplication for objects with duplicate IDs

### 19.2 ASP Rules

The protocol MUST enforce ASP (Answer Set Programming) rules:

```prolog
1 { layer(N,D) : depth(D) } 1 :- node(N).
:- implements(X,Y1), implements(X,Y2), Y1 != Y2.
```

### 19.3 Prolog Inheritance

The protocol MUST support Prolog inheritance:

```prolog
inherits(X,Z) :- vertical(Y,X), inherits(Y,Z).
```

### 19.4 DataLog Fact Extraction

The protocol MUST support DataLog fact extraction:

```datalog
node(Id, Type, X, Y, Text).
edge(Id, Type, FromNode, ToNode).
vertical(Id, FromNode, ToNode).
horizontal(Id, FromNode, ToNode).
```

### 19.5 SHACL Constraints

The protocol MUST enforce SHACL constraints:

- Label validation: `rdfs:label` MUST be string
- Identity validation: `owl:sameAs` minimum count 1
- Technology validation: `prov:used` MUST match specifications
- Provenance validation: `selfReference` MUST have file and line

### 19.6 R5RS Function Requirements

The protocol MUST:

- Provide all required R5RS functions from `r5rs-canvas-engine.scm`
- Support function registry in `r5rs-functions-trie.jsonl`
- Enable function invocation from JSONL/CanvasL entries
- Support pure functions (no side effects unless marked)

---

## 20. Validation Requirements

### 20.1 Validation Pipeline

All files MUST be validated in this order:

1. **JSONL Syntax**: Files MUST be valid JSONL
2. **CanvasL Syntax**: CanvasL extensions MUST be valid
3. **Fact Extraction**: Facts MUST be extractable
4. **RDF Conversion**: RDF triples MUST be valid
5. **SHACL Validation**: SHACL shapes MUST be valid
6. **RFC2119 Validation**: RFC2119 constraints MUST be satisfied
7. **ASP Validation**: ASP constraints MUST be satisfied
8. **Prolog Validation**: Prolog rules MUST be valid
9. **Datalog Validation**: Datalog programs MUST be stratified

### 20.2 Validation Functions

The protocol MUST provide validation functions:

- `r5rs:validate-jsonl-syntax(file)` → Validation result
- `r5rs:validate-canvasl-syntax(file)` → Validation result
- `r5rs:validate-facts(facts)` → Validation result
- `r5rs:validate-rdf(triples)` → Validation result
- `r5rs:validate-shacl(shapes, triples)` → Validation result
- `r5rs:validate-rfc2119(constraints)` → Validation result
- `r5rs:validate-asp(rules)` → Validation result
- `r5rs:validate-prolog(db)` → Validation result
- `r5rs:validate-datalog(program)` → Validation result

### 20.3 Validation Reporting

The protocol MUST provide validation reporting:

- **Error Reporting**: Detailed error messages
- **Warning Reporting**: Warning messages for non-critical issues
- **Success Reporting**: Success confirmation for valid files
- **Report Format**: Structured validation reports

---

## 21. Protocol Message Types

### 21.1 Query Messages

The protocol MUST support query messages:

#### 21.1.1 ProLog Query

```json
{
  "type": "prolog-query",
  "database": "automaton-db",
  "goal": "node(?Id, ?Type)",
  "context": {
    "facts": [...],
    "rules": [...]
  }
}
```

#### 21.1.2 DataLog Query

```json
{
  "type": "datalog-query",
  "program": {
    "facts": [...],
    "rules": [...]
  },
  "goal": "inherits(?X, ?Z)"
}
```

#### 21.1.3 SPARQL Query

```json
{
  "type": "sparql-query",
  "query": "SELECT ?id ?type WHERE { ?id rdf:type ?type }",
  "triples": [...]
}
```

### 21.2 Invocation Messages

The protocol MUST support invocation messages:

#### 21.2.1 R5RS Function Invocation

```json
{
  "type": "r5rs-invoke",
  "function": "r5rs:church-add",
  "args": [2, 3],
  "context": {
    "facts": [...],
    "triples": [...]
  }
}
```

### 21.3 Validation Messages

The protocol MUST support validation messages:

#### 21.3.1 SHACL Validation

```json
{
  "type": "shacl-validate",
  "shapes": [...],
  "triples": [...]
}
```

### 21.4 Evolution Messages

The protocol MUST support evolution messages:

#### 21.4.1 Snapshot Request

```json
{
  "type": "snapshot-request",
  "automaton-id": "automaton-1",
  "timestamp": "2025-11-10T16:20:44Z"
}
```

#### 21.4.2 Variant Generation

```json
{
  "type": "variant-generation",
  "base-variant": "automaton-base",
  "optimization-target": "memory",
  "constraints": [...]
}
```

### 21.5 Knowledge Extraction Messages

The protocol MUST support knowledge extraction messages:

#### 21.5.1 Knowledge Extraction Request

```json
{
  "type": "knowledge-extraction",
  "source": "docs/AGENTS.md",
  "extract-types": ["facts", "rules", "agents", "functions"]
}
```

### 21.6 Agent API Messages

The protocol MUST support agent API messages:

#### 21.6.1 Agent Discovery

```json
{
  "type": "agent-discovery",
  "filter": {
    "dimension": "4D"
  }
}
```

#### 21.6.2 Agent Execution

```json
{
  "type": "agent-execution",
  "agent-id": "4D-Network-Agent",
  "operation": "query",
  "parameters": {
    "query": "SELECT ?id WHERE { ?id dimension '4D' }"
  }
}
```

---

## 22. References

### 22.1 Related Specifications

- **`docs/01-R5RS-Expressions/R5RS-EXPRESSIONS-RFC2119-SPEC.md`**: R5RS expression foundations
- **`docs/02-JSONL-Database-Adapter/JSONL-DATABASE-ADAPTER-RFC2119-SPEC.md`**: Database adapter architecture
- **`docs/04-CanvasL/CANVASL-RFC2119-SPEC.md`**: CanvasL language specification
- **`docs/05-Meta-Log/MULTIVERSE-CANVAS-RFC2119-SPEC.md`**: Multiverse canvas specification
- **`docs/07-Meta-Log-Db/META-LOG-DB-RFC2119-SPEC.md`**: Meta-Log database implementation
- **`docs/12-Automatons-CanvasL/AUTOMATONS-CANVASL-RFC2119-SPEC.md`**: Automatons CanvasL integration
- **`docs/13-Federated-Provenance-Meta-Log/FEDERATED-PROVENANCE-RFC2119-SPEC.md`**: Federated provenance tracking
- **`docs/14-Automaton-Evolution-Logging/AUTOMATON-EVOLUTION-LOGGING-RFC2119-SPEC.md`**: Evolution logging system
- **`docs/15-Automaton-Evolution-Testing-Optimizing/AUTOMATON-EVOLUTION-TESTING-OPTIMIZING-RFC2119-SPEC.md`**: Testing and optimization
- **`docs/16-Knowledge-Extraction-Propagation/README.md`**: Knowledge extraction and propagation
- **`docs/19-Agent-Procedures-Constraints-API/README.md`**: Agent API documentation

### 22.2 Implementation References

- **`r5rs-canvas-engine.scm`**: Unified R5RS function implementations
- **`grok_files/02-Grok.md` through `grok_files/25-Grok.md`**: R5RS concept definitions
- **`ui/src/grammars/canvasl.grammar`**: CanvasL Lezer grammar
- **`meta-log-db/`**: Meta-Log database implementation
- **`evolutions/document-knowledge-extractor/`**: Knowledge extraction system
- **`evolutions/natural-language-query/`**: Natural language query engine

### 22.3 Standards References

- **RFC 2119**: Key words for use in RFCs to Indicate Requirement Levels
- **R5RS**: Revised^5 Report on the Algorithmic Language Scheme
- **RDF 1.1**: Resource Description Framework
- **SPARQL 1.1**: SPARQL Query Language
- **SHACL**: Shapes Constraint Language
- **PROV-O**: PROV Ontology for Provenance

---

**End of Specification**
