---
id: canvasl-m-s-expression-grammar
title: "CanvasL M/S-Expression Grammar and Human-Readable Translation"
level: intermediate
type: specification
tags: [canvasl, grammar, m-expressions, s-expressions, dual-pairs, human-readable]
keywords: [canvasl, m-expressions, s-expressions, grammar, translation, dual-pairs, affine, projective]
prerequisites: [canvasl-rfc2119-spec, dual-pairs-unified]
enables: [canvasl-translation, m-s-expression-mapping]
related: [canvasl-rfc2119-spec, dual-pairs-unified, computational-mapping]
readingTime: 45
difficulty: 4
---

# CanvasL M/S-Expression Grammar and Human-Readable Translation

## Overview

This document provides a grammar specification and human-readable translation guide for CanvasL that demonstrates the dual pair relationship between M-expressions (mathematical notation, affine space) and S-expressions (LISP notation, projective space) within the CanvasL format.

**Key Concepts**:
- **M-Expressions**: Mathematical notation representing "what things ARE" (affine space, values, facts)
- **S-Expressions**: LISP notation representing "what things DO" (projective space, functions, rules)
- **CanvasL**: Extended JSONL format that bridges both expression types

## Dual Pair Foundation

### M-Expression (Affine Space)
**Expression Space**: What things ARE

```
f(x, y) = x + y
result = f(3, 4)  ; = 7
```

**Properties**:
- Points in affine space
- Values/data
- Static definitions
- **BQF**: [a, 0, 0] - Pure affine (x² term)

### S-Expression (Projective Space)
**Function Space**: What things DO

```
(lambda (x y) (+ x y))
(define f (lambda (x y) (+ x y)))
(f 3 4)  ; Application: projective → affine
```

**Properties**:
- Lines in projective space
- Functions/transformations
- Dynamic execution
- **BQF**: [0, 0, c] - Pure projective (y² term)

## CanvasL Grammar for M/S-Expressions

### Grammar Structure

```ebnf
CanvasLFile ::= Directives? JSONLEntries+

Directives ::= DirectiveLine+
DirectiveLine ::= "@" Identifier ":" JSONValue "\n"

JSONLEntries ::= JSONLObject+
JSONLObject ::= "{" JSONLProperty ("," JSONLProperty)* "}" "\n"

JSONLProperty ::= JSONKey ":" JSONLValue

JSONLValue ::= 
    JSONString
  | JSONNumber
  | JSONBoolean
  | JSONNull
  | JSONLObject
  | JSONLArray
  | MExpression      ; M-expression notation
  | SExpression       ; S-expression notation
  | R5RSCall          ; R5RS function call
  | DimensionRef      ; Dimension reference
  | NodeRef           ; Node reference

MExpression ::= 
    MFunctionDef      ; f(x, y) = expression
  | MAssignment       ; variable = expression
  | MValue            ; Static value

MFunctionDef ::= Identifier "(" MParameterList? ")" "=" MExpression
MParameterList ::= Identifier ("," Identifier)*
MAssignment ::= Identifier "=" MExpression
MValue ::= JSONNumber | JSONString | JSONBoolean

SExpression ::= 
    SLambda           ; (lambda (params) body)
  | SApplication      ; (function args)
  | SDefine           ; (define name value)
  | SList             ; (item1 item2 ...)

SLambda ::= "(" "lambda" "(" SParameterList? ")" SExpression ")"
SParameterList ::= Identifier+
SApplication ::= "(" SExpression SExpression* ")"
SDefine ::= "(" "define" Identifier SExpression ")"
SList ::= "(" SExpression* ")"

R5RSCall ::= 
    R5RSFunction      ; r5rs:function-name
  | R5RSExpression    ; (scheme-expression)

R5RSFunction ::= "r5rs:" Identifier
R5RSExpression ::= "(" SchemeExpression ")"

DimensionRef ::= "[0-7]D"
NodeRef ::= "#" Identifier

JSONKey ::= String
JSONString ::= '"' StringChar* '"'
JSONNumber ::= Number
JSONBoolean ::= "true" | "false"
JSONNull ::= "null"
JSONLArray ::= "[" JSONLValue ("," JSONLValue)* "]"
```

## Translation Rules

### Rule 1: M-Expression → S-Expression → CanvasL

#### M-Expression (Mathematical Notation)
```
f(x, y) = x + y
result = f(3, 4)
```

#### S-Expression (LISP Notation)
```scheme
(define f (lambda (x y) (+ x y)))
(define result (f 3 4))
```

#### CanvasL (JSONL Format)
```json
{
  "id": "function-f",
  "type": "r5rs-call",
  "function": "r5rs:lambda",
  "args": [
    ["x", "y"],
    {"type": "r5rs-call", "function": "r5rs:add", "args": ["x", "y"]}
  ],
  "metadata": {
    "mExpression": "f(x, y) = x + y",
    "sExpression": "(lambda (x y) (+ x y))",
    "bqf": [1, 0, 0]
  }
}
{
  "id": "result",
  "type": "node",
  "value": {
    "type": "r5rs-call",
    "function": "r5rs:apply",
    "args": ["#function-f", [3, 4]]
  },
  "metadata": {
    "mExpression": "result = f(3, 4)",
    "sExpression": "(f 3 4)",
    "bqf": [0, 0, 1]
  }
}
```

### Rule 2: BQF Transformation in CanvasL

#### M-Expression (Affine Value)
```
cube_bqf = [8, 12, 6]
```

#### S-Expression (Function Application)
```scheme
(define cube-bqf '(8 12 6))
(dual-swap cube-bqf)  ; → (6 12 8)
```

#### CanvasL (With BQF Operations)
```json
{
  "id": "cube-bqf",
  "type": "node",
  "bqf": [8, 12, 6],
  "metadata": {
    "mExpression": "cube_bqf = [8, 12, 6]",
    "sExpression": "(define cube-bqf '(8 12 6))",
    "polyhedron": "cube"
  }
}
{
  "id": "octahedron-bqf",
  "type": "r5rs-call",
  "function": "r5rs:dual-swap",
  "args": [{"fromNode": "#cube-bqf", "property": "bqf"}],
  "metadata": {
    "mExpression": "octa_bqf = dual_swap(cube_bqf)",
    "sExpression": "(dual-swap cube-bqf)",
    "polyhedron": "octahedron"
  }
}
```

### Rule 3: Consensus Patterns

#### M-Expression (Mathematical Definition)
```
tetra_consensus(facts) = gcd(facts[0], facts[1], facts[2], facts[3])
```

#### S-Expression (LISP Function)
```scheme
(define (tetra-consensus facts)
  (if (= (length facts) 4)
      (reduce gcd facts)
      (error "Need 4 facts")))
```

#### CanvasL (With Consensus Service)
```json
{
  "id": "tetra-consensus",
  "type": "r5rs-call",
  "function": "r5rs:tetra-consensus",
  "args": [[2, 4, 6, 8]],
  "metadata": {
    "mExpression": "tetra_consensus([2, 4, 6, 8]) = gcd(2, 4, 6, 8)",
    "sExpression": "(tetra-consensus '(2 4 6 8))",
    "polyhedron": "tetrahedron",
    "bqf": [4, 6, 4],
    "consensusType": "gcd"
  }
}
```

### Rule 4: Type System Mapping

#### M-Expression (Type Definition)
```
R5RS_8_Tuple = [Boolean, Pair, Symbol, Number, Char, String, Vector, Procedure]
Cube_Vertex(type) = type_to_cube_vertex(type)
```

#### S-Expression (LISP Mapping)
```scheme
(define r5rs-8-tuple '(boolean pair symbol number char string vector procedure))
(define (cube-vertex type)
  (type-to-cube-vertex type))
```

#### CanvasL (With Type Mapping)
```json
{
  "id": "r5rs-8-tuple",
  "type": "node",
  "types": ["boolean", "pair", "symbol", "number", "char", "string", "vector", "procedure"],
  "metadata": {
    "mExpression": "R5RS_8_Tuple = [Boolean, Pair, Symbol, Number, Char, String, Vector, Procedure]",
    "sExpression": "(define r5rs-8-tuple '(boolean pair symbol number char string vector procedure))",
    "polyhedron": "cube",
    "bqf": [8, 12, 6]
  }
}
{
  "id": "type-mapping",
  "type": "r5rs-call",
  "function": "r5rs:type-to-cube-vertex",
  "args": ["procedure"],
  "metadata": {
    "mExpression": "Cube_Vertex(Procedure) = 7",
    "sExpression": "(type-to-cube-vertex 'procedure)",
    "result": 7
  }
}
```

## Human-Readable Translation Examples

### Example 1: Simple Addition

**M-Expression** (What it IS):
```
sum = 2 + 3
```

**S-Expression** (What it DOES):
```scheme
(define sum (+ 2 3))
```

**CanvasL** (Bridged Format):
```json
{
  "id": "sum",
  "type": "r5rs-call",
  "function": "r5rs:add",
  "args": [2, 3],
  "metadata": {
    "mExpression": "sum = 2 + 3",
    "sExpression": "(+ 2 3)",
    "bqf": [0, 0, 1],
    "translation": "M-expression (affine value) → S-expression (function application) → CanvasL (R5RS call)"
  }
}
```

### Example 2: Polyhedron BQF

**M-Expression** (Mathematical Definition):
```
cube = Polyhedron(vertices=8, edges=12, faces=6)
cube_bqf = [8, 12, 6]
octa_bqf = dual_swap(cube_bqf)  ; = [6, 12, 8]
```

**S-Expression** (LISP Function):
```scheme
(define cube-bqf '(8 12 6))
(define octa-bqf (dual-swap cube-bqf))  ; → (6 12 8)
```

**CanvasL** (With Polyhedra):
```json
{
  "id": "cube",
  "type": "node",
  "polyhedron": "cube",
  "bqf": [8, 12, 6],
  "metadata": {
    "mExpression": "cube_bqf = [8, 12, 6]",
    "sExpression": "(define cube-bqf '(8 12 6))",
    "vertices": 8,
    "edges": 12,
    "faces": 6
  }
}
{
  "id": "octahedron",
  "type": "r5rs-call",
  "function": "r5rs:dual-swap",
  "args": [{"fromNode": "#cube", "property": "bqf"}],
  "metadata": {
    "mExpression": "octa_bqf = dual_swap(cube_bqf)",
    "sExpression": "(dual-swap cube-bqf)",
    "polyhedron": "octahedron",
    "bqf": [6, 12, 8]
  }
}
```

### Example 3: Consensus Pattern

**M-Expression** (Mathematical Definition):
```
consensus = tetra_consensus([2, 4, 6, 8])
         = gcd(2, 4, 6, 8)
         = 2
```

**S-Expression** (LISP Function):
```scheme
(define consensus (tetra-consensus '(2 4 6 8)))
;; → 2 (GCD of all)
```

**CanvasL** (With Consensus):
```json
{
  "id": "tetra-consensus-result",
  "type": "r5rs-call",
  "function": "r5rs:tetra-consensus",
  "args": [[2, 4, 6, 8]],
  "metadata": {
    "mExpression": "consensus = gcd(2, 4, 6, 8) = 2",
    "sExpression": "(tetra-consensus '(2 4 6 8))",
    "polyhedron": "tetrahedron",
    "bqf": [4, 6, 4],
    "consensusType": "gcd",
    "result": 2
  }
}
```

### Example 4: Constraint Pointer

**M-Expression** (Mathematical Definition):
```
constraint = create_constraint(cube_bqf, octa_bqf)
direction = constraint.direction
```

**S-Expression** (LISP Function):
```scheme
(define constraint (create-constraint cube-bqf octa-bqf))
(define direction (constraint-direction constraint))
```

**CanvasL** (With Constraint):
```json
{
  "id": "cube-octa-constraint",
  "type": "r5rs-call",
  "function": "r5rs:create-constraint-pointer",
  "args": [
    {"fromNode": "#cube", "property": "bqf"},
    {"fromNode": "#octahedron", "property": "bqf"}
  ],
  "metadata": {
    "mExpression": "constraint = create_constraint(cube_bqf, octa_bqf)",
    "sExpression": "(create-constraint cube-bqf octa-bqf)",
    "dualPair": "cube-octahedron",
    "direction": [0, 0, -2],
    "strength": 0.2
  }
}
```

## Grammar Translation Table

| M-Expression | S-Expression | CanvasL Type | BQF |
|--------------|--------------|-------------|-----|
| `f(x) = x + 1` | `(lambda (x) (+ x 1))` | `r5rs-call` | `[1, 0, 0]` |
| `result = f(5)` | `(f 5)` | `r5rs-call` | `[0, 0, 1]` |
| `bqf = [8, 12, 6]` | `(define bqf '(8 12 6))` | `node` with `bqf` | `[8, 12, 6]` |
| `dual = dual_swap(bqf)` | `(dual-swap bqf)` | `r5rs-call` | `[6, 12, 8]` |
| `consensus = gcd(a, b)` | `(gcd a b)` | `r5rs-call` | `[a, 0, 0]` |
| `consensus = lcm(a, b)` | `(lcm a b)` | `r5rs-call` | `[0, 0, c]` |

## BQF Encoding in CanvasL

### BQF Structure
```json
{
  "bqf": [a, b, c],
  "metadata": {
    "affine": "a (vertices, values, facts)",
    "interaction": "b (edges, ports, hashes)",
    "projective": "c (faces, functions, rules)"
  }
}
```

### M-Expression BQF
```json
{
  "id": "m-expression-bqf",
  "type": "node",
  "bqf": [1, 0, 0],
  "metadata": {
    "mExpression": "value = 42",
    "expressionType": "affine",
    "bqfInterpretation": "Pure affine (x² term only)"
  }
}
```

### S-Expression BQF
```json
{
  "id": "s-expression-bqf",
  "type": "r5rs-call",
  "function": "r5rs:lambda",
  "bqf": [0, 0, 1],
  "metadata": {
    "sExpression": "(lambda (x) (+ x 1))",
    "expressionType": "projective",
    "bqfInterpretation": "Pure projective (y² term only)"
  }
}
```

### Complete BQF (Both)
```json
{
  "id": "complete-bqf",
  "type": "r5rs-call",
  "function": "r5rs:compose-bqf",
  "args": [
    {"bqf": [1, 0, 0]},
    {"bqf": [0, 0, 1]}
  ],
  "bqf": [1, 1, 1],
  "metadata": {
    "mExpression": "f(x) = x + 1",
    "sExpression": "(lambda (x) (+ x 1))",
    "expressionType": "both",
    "bqfInterpretation": "Complete BQF (x² + xy + y²)"
  }
}
```

## Translation Workflow

### Step 1: M-Expression → S-Expression
```
M: f(x, y) = x + y
     ↓
S: (lambda (x y) (+ x y))
```

### Step 2: S-Expression → CanvasL
```
S: (lambda (x y) (+ x y))
     ↓
CanvasL: {
  "type": "r5rs-call",
  "function": "r5rs:lambda",
  "args": [["x", "y"], {"function": "r5rs:add", "args": ["x", "y"]}]
}
```

### Step 3: CanvasL → Execution
```
CanvasL → R5RS Engine → Result
```

## Human-Readable Format

### M-Expression Format (Affine)
```
# M-Expression: What things ARE
function_name(parameter1, parameter2) = expression
variable = value
constant = 42
```

### S-Expression Format (Projective)
```
# S-Expression: What things DO
(lambda (parameter1 parameter2) expression)
(define variable value)
(define constant 42)
```

### CanvasL Format (Bridged)
```json
{
  "id": "identifier",
  "type": "r5rs-call",
  "function": "r5rs:function-name",
  "args": [arg1, arg2],
  "metadata": {
    "mExpression": "human-readable M-expression",
    "sExpression": "human-readable S-expression",
    "bqf": [a, b, c]
  }
}
```

## Complete Example: Polyhedron Consensus

### M-Expression
```
cube_consensus(types) = lcm(types[0], types[1], ..., types[7])
result = cube_consensus([1, 2, 3, 4, 5, 6, 7, 8])
       = 840
```

### S-Expression
```scheme
(define (cube-consensus types)
  (if (= (length types) 8)
      (apply lcm types)
      (error "Need 8 types")))

(define result (cube-consensus '(1 2 3 4 5 6 7 8)))
;; → 840
```

### CanvasL
```json
@version: "1.0"
@schema: "canvasl-v1"

{
  "id": "cube-consensus-function",
  "type": "r5rs-call",
  "function": "r5rs:cube-consensus",
  "metadata": {
    "mExpression": "cube_consensus(types) = lcm(types[0], ..., types[7])",
    "sExpression": "(define (cube-consensus types) (apply lcm types))",
    "polyhedron": "cube",
    "bqf": [8, 12, 6],
    "consensusType": "lcm"
  }
}
{
  "id": "cube-consensus-result",
  "type": "r5rs-call",
  "function": "r5rs:cube-consensus",
  "args": [[1, 2, 3, 4, 5, 6, 7, 8]],
  "metadata": {
    "mExpression": "result = cube_consensus([1, 2, 3, 4, 5, 6, 7, 8]) = 840",
    "sExpression": "(cube-consensus '(1 2 3 4 5 6 7 8))",
    "result": 840,
    "fromNode": "#cube-consensus-function"
  }
}
```

## Summary

This grammar document provides:

1. **Complete Grammar Specification**: EBNF grammar for M/S-expressions in CanvasL
2. **Translation Rules**: Step-by-step translation from M → S → CanvasL
3. **Human-Readable Examples**: Real-world examples showing all three formats
4. **BQF Integration**: How BQF encoding maps to expression types
5. **Polyhedra Integration**: Examples using polyhedra consensus and constraints

**Key Insight**: CanvasL serves as a bridge between M-expressions (affine, "what things ARE") and S-expressions (projective, "what things DO"), enabling both mathematical notation and functional programming within a unified JSONL format.

---

**Last Updated**: 2025-01-07  
**Status**: Complete Grammar Specification  
**Related**: `docs/04-CanvasL/CANVASL-RFC2119-SPEC.md`, `docs/31-Understanding-Computational-Geometries/01-Research/dual-pairs-unified.md`

