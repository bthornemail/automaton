# GECS Tree Structure: Complete Explanation

## Overview

This document explains the **Geometric-Epistemic Coordinate System (GECS)** tree structure and how to build it using CanvasL files. The tree maps computational concepts to geometric structures using the dual pair framework.

## The Tree Structure

```
0D/
├── 01-ontology/
│   ├── 01-subject-type.canvasl
│   ├── 02-predicate-type.canvasl
│   ├── 03-object-type.canvasl
│   └── 04-event-type.canvasl
├── 02-syntax/
│   ├── 01-polynomial-function.canvasl
│   └── 02-polynomial-expression.canvasl
├── 03-topology/
│   ├── 01-affine-point.canvasl
│   └── 02-projective-line.canvasl
├── 04-semantics/
│   ├── 01-observation.canvasl
│   ├── 02-measurement.canvasl
│   ├── 03-reference-frame.canvasl
│   └── 04-context.canvasl
├── 05-modality/
│   ├── 01-KK.canvasl  (Known Knowns)
│   ├── 02-KU.canvasl  (Known Unknowns)
│   ├── 03-UK.canvasl  (Unknown Knowns)
│   └── 04-UU.canvasl  (Unknown Unknowns)
├── 06-theory/
│   ├── 01-axiom.canvasl
│   └── 02-proof.canvasl
└── 07-system/
    ├── 01-rules.canvasl
    └── 02-facts.canvasl
```

## Why This Structure?

### The GECS Coordinate System

**GECS addresses**: `{dimension}-{branch}-{leaf}`

Example: `0D-01-01` = Dimension 0D, Branch 01 (ontology), Leaf 01 (subject-type)

### The 7 Branches Explained

#### 1. Ontology (01): What Things ARE

**Purpose**: Define the fundamental types/categories

**Files**:
- `01-subject-type.canvasl`: Who/what performs actions (AFFINE - the actor IS)
- `02-predicate-type.canvasl`: What actions/relations exist (PROJECTIVE - what DOES)
- `03-object-type.canvasl`: What is acted upon (AFFINE - the target IS)
- `04-event-type.canvasl`: When things happen (temporal structure)

**Dual Pair**: Subject/Object (affine) ↔ Predicate (projective)

#### 2. Syntax (02): How Things Are WRITTEN

**Purpose**: Define the notation/representation

**Files**:
- `01-polynomial-function.canvasl`: Functions (PROJECTIVE - what code DOES)
- `02-polynomial-expression.canvasl`: Expressions (AFFINE - what code IS)

**Dual Pair**: Expression (affine) ↔ Function (projective)

#### 3. Topology (03): The GEOMETRIC Foundation

**Purpose**: Define the geometric structures

**Files**:
- `01-affine-point.canvasl`: Points (AFFINE - values/data)
- `02-projective-line.canvasl`: Lines (PROJECTIVE - functions/transformations)

**Dual Pair**: Point (affine) ↔ Line (projective)

#### 4. Semantics (04): What Things MEAN

**Purpose**: Define interpretation and context

**Files**:
- `01-observation.canvasl`: The act of observing (measurement)
- `02-measurement.canvasl`: The data from observation
- `03-reference-frame.canvasl`: The coordinate system for interpretation
- `04-context.canvasl`: The situation/environment of interpretation

**Dual Pair**: Observation/Measurement (affine data) ↔ Reference Frame (projective interpretation)

#### 5. Modality (05): What We KNOW

**Purpose**: Rumsfeldian analysis - epistemic states

**Files**:
- `01-KK.canvasl`: Known Knowns (we know we know it - AFFINE)
- `02-KU.canvasl`: Known Unknowns (we know we don't know it)
- `03-UK.canvasl`: Unknown Knowns (we don't know we know it - implicit)
- `04-UU.canvasl`: Unknown Unknowns (we don't know we don't know it)

**Dual Pair**: KK/KU (affine - explicit knowledge) ↔ UK/UU (projective - implicit/potential)

#### 6. Theory (06): How We PROVE Things

**Purpose**: Define the logical foundation

**Files**:
- `01-axiom.canvasl`: Ground truths (AFFINE - what we assume IS true)
- `02-proof.canvasl`: Derivations (PROJECTIVE - how we PROVE things)

**Dual Pair**: Axiom (affine - ground truth) ↔ Proof (projective - derivation)

#### 7. System (07): The COMPUTATIONAL Implementation

**Purpose**: Define the executable system

**Files**:
- `01-rules.canvasl`: Prolog rules (PROJECTIVE - what system DOES)
- `02-facts.canvasl`: Prolog facts (AFFINE - what system KNOWS)

**Dual Pair**: Facts (affine - data/GCD) ↔ Rules (projective - functions/LCM)

## Building the Tree

### Step 1: Create Directory Structure

```bash
mkdir -p 0D/01-ontology
mkdir -p 0D/02-syntax
mkdir -p 0D/03-topology
mkdir -p 0D/04-semantics
mkdir -p 0D/05-modality
mkdir -p 0D/06-theory
mkdir -p 0D/07-system
```

### Step 2: Create Base Template

Each `.canvasl` file follows this structure:

```canvasl
@version: "1.0"
@schema: "canvasl-v1-bipartite-bqf"
@r5rs-engine: "r5rs-canvas-engine.scm"

{"id": "0D-{branch}-{leaf}", "type": "text", "dimension": "0D", "x": 0, "y": 0, "bipartite": {"partition": "topology", "bqf": {"form": "Q() = 0", "variables": [], "symbol": "()", "polynomial": "0", "procedure": "(lambda () 'vacuum)"}}}
```

### Step 3: Ontology Files

#### `0D/01-ontology/01-subject-type.canvasl`

```canvasl
@version: "1.0"
@schema: "canvasl-v1-bipartite-bqf"
@r5rs-engine: "r5rs-canvas-engine.scm"

{"id": "0D-01-01", "type": "text", "dimension": "0D", "text": "# Subject Type\n\nDefines WHO or WHAT performs an action.\n\n**Affine (Expression)**: The subject IS a thing.\n\n**Examples**:\n- Person: 'John'\n- System: 'Database'\n- Agent: 'Automaton'", "bipartite": {"partition": "topology", "dimension": "0D", "bqf": {"form": "Q() = 0", "variables": [], "symbol": "()", "polynomial": "0", "procedure": "(lambda () 'subject)"}}}
```

#### `0D/01-ontology/02-predicate-type.canvasl`

```canvasl
@version: "1.0"
@schema: "canvasl-v1-bipartite-bqf"

{"id": "0D-01-02", "type": "text", "dimension": "0D", "text": "# Predicate Type\n\nDefines WHAT actions or relations exist.\n\n**Projective (Function)**: The predicate DOES something.\n\n**Examples**:\n- Action: 'runs', 'computes'\n- Relation: 'is-parent-of', 'connects-to'\n- Process: 'evaluates', 'transforms'", "bipartite": {"partition": "topology", "dimension": "0D", "bqf": {"form": "Q() = 0", "variables": [], "symbol": "()", "polynomial": "0", "procedure": "(lambda () 'predicate)"}}}
```

#### `0D/01-ontology/03-object-type.canvasl`

```canvasl
@version: "1.0"
@schema: "canvasl-v1-bipartite-bqf"

{"id": "0D-01-03", "type": "text", "dimension": "0D", "text": "# Object Type\n\nDefines WHAT is acted upon.\n\n**Affine (Expression)**: The object IS a thing.\n\n**Examples**:\n- Data: 'file', 'record'\n- Value: '42', 'true'\n- Entity: 'document', 'node'", "bipartite": {"partition": "topology", "dimension": "0D", "bqf": {"form": "Q() = 0", "variables": [], "symbol": "()", "polynomial": "0", "procedure": "(lambda () 'object)"}}}
```

#### `0D/01-ontology/04-event-type.canvasl`

```canvasl
@version: "1.0"
@schema: "canvasl-v1-bipartite-bqf"

{"id": "0D-01-04", "type": "text", "dimension": "0D", "text": "# Event Type\n\nDefines WHEN things happen (temporal structure).\n\n**Examples**:\n- Timestamp: '2025-01-07T12:00:00Z'\n- Duration: '5 seconds'\n- Sequence: 'before', 'after', 'during'", "bipartite": {"partition": "topology", "dimension": "0D", "bqf": {"form": "Q() = 0", "variables": [], "symbol": "()", "polynomial": "0", "procedure": "(lambda () 'event)"}}}
```

### Step 4: Syntax Files

#### `0D/02-syntax/01-polynomial-function.canvasl`

```canvasl
@version: "1.0"
@schema: "canvasl-v1-bipartite-bqf"

{"id": "0D-02-01", "type": "text", "dimension": "0D", "text": "# Polynomial Function\n\n**Projective (Function)**: What the polynomial DOES.\n\n**Example**:\n```scheme\n(lambda (x) (* x x))  ; Function that squares\n```\n\nThis is the EXECUTABLE form - you can CALL it.", "bipartite": {"partition": "topology", "dimension": "0D", "bqf": {"form": "Q() = 0", "variables": [], "symbol": "(lambda (x) (* x x))", "polynomial": "x^2", "procedure": "(lambda (x) (* x x))"}}}
```

#### `0D/02-syntax/02-polynomial-expression.canvasl`

```canvasl
@version: "1.0"
@schema: "canvasl-v1-bipartite-bqf"

{"id": "0D-02-02", "type": "text", "dimension": "0D", "text": "# Polynomial Expression\n\n**Affine (Expression)**: What the polynomial IS.\n\n**Example**:\n```\nP(x) = x^2 + 2x + 1\n```\n\nThis is the SYMBOLIC form - you can MANIPULATE it algebraically.", "bipartite": {"partition": "topology", "dimension": "0D", "bqf": {"form": "Q() = 0", "variables": [], "symbol": "x^2 + 2x + 1", "polynomial": "x^2 + 2x + 1", "procedure": "(lambda (x) (+ (* x x) (* 2 x) 1))"}}}
```

### Step 5: Topology Files

#### `0D/03-topology/01-affine-point.canvasl`

```canvasl
@version: "1.0"
@schema: "canvasl-v1-bipartite-bqf"

{"id": "0D-03-01", "type": "text", "dimension": "0D", "text": "# Affine Point\n\n**Affine Space**: A single point - a VALUE.\n\n**Examples**:\n- Number: 42\n- Boolean: true\n- Character: 'a'\n- Fact: color(apple, red)\n\n**Geometric**: A point in space.\n**Computational**: A value in memory.\n**Prolog**: A ground fact.", "bipartite": {"partition": "topology", "dimension": "0D", "bqf": {"form": "Q() = 0", "variables": [], "symbol": "()", "polynomial": "0", "procedure": "(lambda () 'point)"}}}
```

#### `0D/03-topology/02-projective-line.canvasl`

```canvasl
@version: "1.0"
@schema: "canvasl-v1-bipartite-bqf"

{"id": "0D-03-02", "type": "text", "dimension": "0D", "text": "# Projective Line\n\n**Projective Space**: A line through the origin - a FUNCTION.\n\n**Key Insight**: A point in projective space is actually a LINE in affine space!\n\n**Examples**:\n- Function: x -> x + 1\n- Rule: red(X) :- color(X, red)\n- Transformation: f(x) = 2x\n\n**Geometric**: A line through origin.\n**Computational**: A function/transformation.\n**Prolog**: A rule.", "bipartite": {"partition": "topology", "dimension": "0D", "bqf": {"form": "Q() = 0", "variables": [], "symbol": "(lambda (x) (+ x 1))", "polynomial": "x + 1", "procedure": "(lambda (x) (+ x 1))"}}}
```

### Step 6: Semantics Files

#### `0D/04-semantics/01-observation.canvasl`

```canvasl
@version: "1.0"
@schema: "canvasl-v1-bipartite-bqf"

{"id": "0D-04-01", "type": "text", "dimension": "0D", "text": "# Observation\n\nThe ACT of observing - measuring a system.\n\n**Projective**: The observation DOES something (collapses wavefunction, reads sensor).\n\n**Example**: Looking at a thermometer.", "bipartite": {"partition": "topology", "dimension": "0D"}}
```

#### `0D/04-semantics/02-measurement.canvasl`

```canvasl
@version: "1.0"
@schema: "canvasl-v1-bipartite-bqf"

{"id": "0D-04-02", "type": "text", "dimension": "0D", "text": "# Measurement\n\nThe DATA from an observation.\n\n**Affine**: The measurement IS a value (72°F, true, 'red').\n\n**Example**: The thermometer reads 72°F.", "bipartite": {"partition": "topology", "dimension": "0D"}}
```

#### `0D/04-semantics/03-reference-frame.canvasl`

```canvasl
@version: "1.0"
@schema: "canvasl-v1-bipartite-bqf"

{"id": "0D-04-03", "type": "text", "dimension": "0D", "text": "# Reference Frame\n\nThe coordinate system for interpretation.\n\n**Projective**: The frame TRANSFORMS measurements (Fahrenheit vs Celsius).\n\n**Example**: Temperature scale (F vs C vs K).", "bipartite": {"partition": "topology", "dimension": "0D"}}
```

#### `0D/04-semantics/04-context.canvasl`

```canvasl
@version: "1.0"
@schema: "canvasl-v1-bipartite-bqf"

{"id": "0D-04-04", "type": "text", "dimension": "0D", "text": "# Context\n\nThe situation/environment of interpretation.\n\n**Example**: 'Hot' means different things for:\n- Coffee: 140°F\n- Summer day: 90°F\n- Oven: 350°F", "bipartite": {"partition": "topology", "dimension": "0D"}}
```

### Step 7: Modality Files (Rumsfeldian Analysis)

#### `0D/05-modality/01-KK.canvasl`

```canvasl
@version: "1.0"
@schema: "canvasl-v1-bipartite-bqf"

{"id": "0D-05-01", "type": "text", "dimension": "0D", "text": "# Known Knowns (KK)\n\n**We know we know it.**\n\n**Affine**: Explicit, documented knowledge.\n\n**Examples**:\n- Documented facts\n- Published research\n- Established knowledge\n- color(apple, red) - we know apples are red", "bipartite": {"partition": "topology", "dimension": "0D", "bqf": {"form": "Q() = 0", "procedure": "(lambda () 'known-known)"}}}
```

#### `0D/05-modality/02-KU.canvasl`

```canvasl
@version: "1.0"
@schema: "canvasl-v1-bipartite-bqf"

{"id": "0D-05-02", "type": "text", "dimension": "0D", "text": "# Known Unknowns (KU)\n\n**We know we don't know it.**\n\n**Examples**:\n- Identified research gaps\n- Open questions\n- TODOs in code\n- 'How does X relate to Y?' - we know we need to find out", "bipartite": {"partition": "topology", "dimension": "0D", "bqf": {"form": "Q() = 0", "procedure": "(lambda () 'known-unknown)"}}}
```

#### `0D/05-modality/03-UK.canvasl`

```canvasl
@version: "1.0"
@schema: "canvasl-v1-bipartite-bqf"

{"id": "0D-05-03", "type": "text", "dimension": "0D", "text": "# Unknown Knowns (UK)\n\n**We don't know we know it (implicit knowledge).**\n\n**Projective**: Hidden/implicit patterns.\n\n**Examples**:\n- Tacit knowledge\n- Muscle memory\n- Unwritten rules\n- Implicit assumptions\n- 'How do I walk?' - you know how, but can't explain", "bipartite": {"partition": "topology", "dimension": "0D", "bqf": {"form": "Q() = 0", "procedure": "(lambda () 'unknown-known)"}}}
```

#### `0D/05-modality/04-UU.canvasl`

```canvasl
@version: "1.0"
@schema: "canvasl-v1-bipartite-bqf"

{"id": "0D-05-04", "type": "text", "dimension": "0D", "text": "# Unknown Unknowns (UU)\n\n**We don't know we don't know it.**\n\n**Projective**: Completely hidden possibilities.\n\n**Examples**:\n- Undiscovered patterns\n- Black swans\n- Paradigm shifts\n- 'What questions haven't we asked yet?'", "bipartite": {"partition": "topology", "dimension": "0D", "bqf": {"form": "Q() = 0", "procedure": "(lambda () 'unknown-unknown)"}}}
```

### Step 8: Theory Files

#### `0D/06-theory/01-axiom.canvasl`

```canvasl
@version: "1.0"
@schema: "canvasl-v1-bipartite-bqf"

{"id": "0D-06-01", "type": "text", "dimension": "0D", "text": "# Axiom\n\n**Affine (Ground Truth)**: What we assume IS true without proof.\n\n**Examples**:\n- 'All men are mortal' (Socrates syllogism)\n- 'A = A' (identity)\n- '1 + 1 = 2' (arithmetic axiom)\n\n**In Prolog**: Base facts that ground the system.", "bipartite": {"partition": "topology", "dimension": "0D", "bqf": {"form": "Q() = 0", "procedure": "(lambda () 'axiom)"}}}
```

#### `0D/06-theory/02-proof.canvasl`

```canvasl
@version: "1.0"
@schema: "canvasl-v1-bipartite-bqf"

{"id": "0D-06-02", "type": "text", "dimension": "0D", "text": "# Proof\n\n**Projective (Derivation)**: How we PROVE things from axioms.\n\n**Examples**:\n- Logical inference\n- Mathematical proof\n- Prolog resolution\n\n**In Prolog**: Rules that derive new facts from existing facts.", "bipartite": {"partition": "topology", "dimension": "0D", "bqf": {"form": "Q() = 0", "procedure": "(lambda () 'proof)"}}}
```

### Step 9: System Files

#### `0D/07-system/01-rules.canvasl`

```canvasl
@version: "1.0"
@schema: "canvasl-v1-bipartite-bqf"

{"id": "0D-07-01", "type": "text", "dimension": "0D", "text": "# Rules (Prolog)\n\n**Projective (Functions/LCM)**: What the system DOES.\n\n**Rules span execution paths** - they're the LCM (union) of all possible behaviors.\n\n**Examples**:\n```prolog\nancestor(X, Y) :- parent(X, Y).\nancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).\n```\n\n**Function Space**: Each rule is a line through projective space.", "bipartite": {"partition": "system", "dimension": "0D", "bqf": {"form": "Q() = 0", "procedure": "(lambda () 'rules)"}}}
```

#### `0D/07-system/02-facts.canvasl`

```canvasl
@version: "1.0"
@schema: "canvasl-v1-bipartite-bqf"

{"id": "0D-07-02", "type": "text", "dimension": "0D", "text": "# Facts (Prolog)\n\n**Affine (Data/GCD)**: What the system KNOWS.\n\n**Facts are the GCD** - they're the common ground (intersection) that everything shares.\n\n**Examples**:\n```prolog\nparent(tom, bob).\nparent(bob, ann).\ncolor(apple, red).\n```\n\n**Point-Set Topology**: Each fact is a point in computational space.\n\n**Program = GCD of all facts** - the ground truth.", "bipartite": {"partition": "system", "dimension": "0D", "bqf": {"form": "Q() = 0", "procedure": "(lambda () 'facts)"}}}
```

## Connecting the Files

### Create Index File: `0D/index.canvasl`

```canvasl
@version: "1.0"
@schema: "canvasl-v1-bipartite-bqf"
@r5rs-engine: "r5rs-canvas-engine.scm"

{"id": "0D", "type": "text", "dimension": "0D", "text": "# 0D: Dimensional Root", "x": 0, "y": 0}
{"id": "0D-01", "type": "text", "dimension": "0D", "text": "# 01: Ontology", "x": 0, "y": 100}
{"id": "0D-02", "type": "text", "dimension": "0D", "text": "# 02: Syntax", "x": 0, "y": 200}
{"id": "0D-03", "type": "text", "dimension": "0D", "text": "# 03: Topology", "x": 0, "y": 300}
{"id": "0D-04", "type": "text", "dimension": "0D", "text": "# 04: Semantics", "x": 0, "y": 400}
{"id": "0D-05", "type": "text", "dimension": "0D", "text": "# 05: Modality", "x": 0, "y": 500}
{"id": "0D-06", "type": "text", "dimension": "0D", "text": "# 06: Theory", "x": 0, "y": 600}
{"id": "0D-07", "type": "text", "dimension": "0D", "text": "# 07: System", "x": 0, "y": 700}
{"id": "e-0D-01", "type": "vertical", "fromNode": "#0D", "toNode": "#0D-01", "label": "has branch"}
{"id": "e-0D-02", "type": "vertical", "fromNode": "#0D", "toNode": "#0D-02", "label": "has branch"}
{"id": "e-0D-03", "type": "vertical", "fromNode": "#0D", "toNode": "#0D-03", "label": "has branch"}
{"id": "e-0D-04", "type": "vertical", "fromNode": "#0D", "toNode": "#0D-04", "label": "has branch"}
{"id": "e-0D-05", "type": "vertical", "fromNode": "#0D", "toNode": "#0D-05", "label": "has branch"}
{"id": "e-0D-06", "type": "vertical", "fromNode": "#0D", "toNode": "#0D-06", "label": "has branch"}
{"id": "e-0D-07", "type": "vertical", "fromNode": "#0D", "toNode": "#0D-07", "label": "has branch"}
```

## How to Use This Tree

### 1. Navigate by GECS Address

```
0D-01-01  →  Dimension 0D, Branch 01 (Ontology), Leaf 01 (Subject Type)
0D-03-02  →  Dimension 0D, Branch 03 (Topology), Leaf 02 (Projective Line)
0D-07-02  →  Dimension 0D, Branch 07 (System), Leaf 02 (Facts)
```

### 2. Understand Dual Pairs

Each branch contains dual pair concepts:
- **Ontology**: Subject/Object (affine) ↔ Predicate (projective)
- **Syntax**: Expression (affine) ↔ Function (projective)
- **Topology**: Point (affine) ↔ Line (projective)
- **Theory**: Axiom (affine) ↔ Proof (projective)
- **System**: Facts (affine/GCD) ↔ Rules (projective/LCM)

### 3. Query the System

```prolog
% Find all affine concepts
?- affine_concept(X).

% Find dual pair
?- dual_pair(affine_point, Y).
Y = projective_line.

% Find by GECS address
?- gecs_address('0D-03-01', Concept).
Concept = affine_point.
```

### 4. Extend to Other Dimensions

Repeat this structure for 1D, 2D, 3D, etc.:

```
1D/
├── 01-ontology/...
├── 02-syntax/...
└── ...

2D/
├── 01-ontology/...
├── 02-syntax/...
└── ...
```

## Summary

**This tree structure provides**:

1. **Coordinate addressing**: GECS format for precise location
2. **Dual pair organization**: Affine vs Projective at every level
3. **Epistemic analysis**: KK/KU/UK/UU modality
4. **Geometric foundation**: Point-set topology (facts) vs Function space (rules)
5. **Computational mapping**: Prolog facts (GCD) vs rules (LCM)

**The tree IS the explanation** - by navigating it, you understand the complete system.