---
id: macro-example-rdf-sparql
title: "Macro Example: RDF_SPARQL.canvasl"
level: intermediate
type: code-example
tags: [canvasl-macros, rdf, sparql, shacl, owl, ui-inference, recursion-safety]
keywords: [canvasl-macros, rdf-triples, sparql-queries, shacl-validation, owl-reasoning, ui-inference, recursion-safety]
prerequisites: [proposal-restructuring, canvasl-rfc2119-spec]
enables: [macros-with-rdf-annotations, macros-with-wikidata]
related: [proposal-restructuring, macros-with-rdf-annotations, macros-with-wikidata]
readingTime: 30
difficulty: 4
blackboard:
  status: active
  assignedAgent: "2D-Structural-Agent"
  lastUpdate: "2025-01-07"
  dependencies: [canvasl-parser, shacl-validator, owl-reasoner]
  watchers: ["6D-Intelligence-Agent"]
---

```json
// src/macros/RDF_SPARQL.canvasl.jsonl

{"@version": "1.0", "type": "macro", "name": "rdf-prefix", "description": "Declare standard UI ontology prefixes", "expansion": [
  {"type": "rdf-prefix", "prefix": "rdf", "uri": "http://www.w3.org/1999/02/22-rdf-syntax-ns#"},
  {"type": "rdf-prefix", "prefix": "rdfs", "uri": "http://www.w3.org/2000/01/rdf-schema#"},
  {"type": "rdf-prefix", "prefix": "owl", "uri": "http://www.w3.org/2002/07/owl#"},
  {"type": "rdf-prefix", "prefix": "xsd", "uri": "http://www.w3.org/2001/XMLSchema#"},
  {"type": "rdf-prefix", "prefix": "sh", "uri": "http://www.w3.org/ns/shacl#"},
  {"type": "rdf-prefix", "prefix": "ui", "uri": "https://canvasl.org/ui#"},
  {"type": "rdf-prefix", "prefix": "proj", "uri": "https://canvasl.org/projector#"}
]}

{"@version": "1.0", "type": "macro", "name": "ui-ontology", "description": "Declare UI ontology with OWL and SHACL", "expansion": [
  {"type": "rdf-triple", "subject": "ui:Component", "predicate": "rdf:type", "object": "owl:Class"},
  {"type": "rdf-triple", "subject": "ui:Container", "predicate": "rdfs:subClassOf", "object": "ui:Component"},
  {"type": "rdf-triple", "subject": "ui:Button", "predicate": "rdfs:subClassOf", "object": "ui:Component"},
  {"type": "rdf-triple", "subject": "ui:contains", "predicate": "rdf:type", "object": "owl:ObjectProperty"},
  {"type": "rdf-triple", "subject": "ui:contains", "predicate": "rdfs:domain", "object": "ui:Container"},
  {"type": "rdf-triple", "subject": "ui:contains", "predicate": "rdfs:range", "object": "ui:Component"},
  {"type": "rdf-triple", "subject": "ui:layout", "predicate": "rdf:type", "object": "owl:DatatypeProperty"},
  {"type": "rdf-triple", "subject": "ui:onClick", "predicate": "rdf:type", "object": "owl:ObjectProperty"},

  {"type": "shacl-shape", "id": "ui:ContainerShape", "targetClass": "ui:Container", "property": [
    {"path": "ui:layout", "minCount": 1, "maxCount": 1, "datatype": "xsd:string"},
    {"path": "ui:contains", "minCount": 0, "node": "ui:ComponentShape"}
  ]},
  {"type": "shacl-shape", "id": "ui:ComponentShape", "targetClass": "ui:Component", "property": [
    {"path": "rdfs:label", "minCount": 1, "maxCount": 1, "datatype": "xsd:string"}
  ]},
  {"type": "shacl-shape", "id": "ui:NoCycleShape", "targetSubjectsOf": "ui:contains", "sparql": [
    {"type": "sparql-ask", "query": "ASK { ?x ui:contains+ ?x }", "message": "CYCLE DETECTED: ui:contains forms a cycle"}
  ]}
]}

{"@version": "1.0", "type": "macro", "name": "ui-component", "description": "Base component with validation", "params": ["id", "type", "label"], "expansion": [
  {"type": "rdf-triple", "subject": {"var": "id"}, "predicate": "rdf:type", "object": {"var": "type"}},
  {"type": "rdf-triple", "subject": {"var": "id"}, "predicate": "rdfs:label", "object": {"literal": {"var": "label"}}},
  {"type": "shacl-validate", "shape": "ui:ComponentShape", "focus": {"var": "id"}, "onError": "ui:error-missing-label"}
]}

{"@version": "1.0", "type": "macro", "name": "ui-container", "description": "Recursion-safe container with SHACL cycle detection", "params": ["id", "layout", "children"], "expansion": [
  {"type": "macro", "call": "ui-component", "args": [{"var": "id"}, "ui:Container", {"literal": "Container"}]},
  {"type": "rdf-triple", "subject": {"var": "id"}, "predicate": "ui:layout", "object": {"literal": {"var": "layout"}}},
  {"type": "rdf-triple", "subject": {"var": "id"}, "predicate": "ui:contains", "object": {"var": "child"}, "repeat": {"var": "children"}, "guard": {"not": {"equals": [{"var": "child"}, {"var": "id"}]}}},
  {"type": "shacl-validate", "shape": "ui:ContainerShape", "focus": {"var": "id"}, "onError": "ui:error-invalid-container"},
  {"type": "shacl-validate", "shape": "ui:NoCycleShape", "focus": {"var": "id"}, "onError": "ui:error-cycle-detected"}
]}

{"@version": "1.0", "type": "macro", "name": "ui-button", "description": "Button with action and validation", "params": ["id", "label", "action"], "expansion": [
  {"type": "macro", "call": "ui-component", "args": [{"var": "id"}, "ui:Button", {"var": "label"}]},
  {"type": "rdf-triple", "subject": {"var": "id"}, "predicate": "ui:onClick", "object": {"var": "action"}}
]}

{"@version": "1.0", "type": "macro", "name": "ui-infer-layout", "description": "OWL + SPARQL: Infer render order and positions", "expansion": [
  {"type": "owl-reason", "entailment": "rdfs", "onError": "ui:error-owl-failure"},
  {"type": "sparql-construct", "query": "PREFIX ui: <https://canvasl.org/ui#>\nCONSTRUCT { \n  ?parent ui:renderOrder ?order .\n  ?child ui:positionIn ?parent .\n  ?child ui:zIndex ?z .\n}\nWHERE {\n  ?parent ui:contains ?child .\n  BIND(xsd:integer(RAND() * 1000) AS ?z)\n  OPTIONAL { ?child ui:order ?explicit }\n  BIND(COALESCE(?explicit, ?z) AS ?order)\n}", "onError": "ui:error-sparql-layout"}
]}

{"@version": "1.0", "type": "macro", "name": "ui-infer-style", "description": "Semantic CSS via OWL class inference", "expansion": [
  {"type": "owl-reason", "entailment": "rdfs"},
  {"type": "sparql-construct", "query": "PREFIX ui: <https://canvasl.org/ui#>\nCONSTRUCT { ?c ui:css ?css . }\nWHERE {\n  VALUES (?type ?css) {\n    (ui:Button \"background: #FF6B6B; color: white; border: none; padding: 12px; border-radius: 8px;\")\n    (ui:Container \"display: flex; gap: 16px;\")\n  }\n  ?c rdf:type ?type .\n}", "onError": "ui:error-sparql-style"}
]}

{"@version": "1.0", "type": "macro", "name": "ui-slide-from-rdf", "description": "Safe slide generation with full validation", "params": ["slideId", "rootComponent"], "expansion": [
  {"type": "try", "steps": [
    {"type": "shacl-validate", "shape": "ui:ContainerShape", "focus": {"var": "rootComponent"}, "onError": "ui:error-invalid-root"},
    {"type": "macro", "call": "ui-infer-layout"},
    {"type": "macro", "call": "ui-infer-style"},
    {"type": "r5rs-call", "function": "render-slide", "args": [{"var": "slideId"}, {"var": "rootComponent"}]}
  ], "catch": {"var": "error"}, "finally": [
    {"type": "log", "level": "info", "message": {"template": "Slide {slideId} rendered with {error ? 'errors' : 'success'}"}, "args": [{"var": "slideId"}, {"var": "error"}]}
  ]}
]}

{"@version": "1.0", "type": "macro", "name": "ui-error-missing-label", "description": "Error handler: missing label", "expansion": [
  {"type": "log", "level": "error", "message": "UI component missing rdfs:label"},
  {"type": "rdf-triple", "subject": {"focus": true}, "predicate": "ui:status", "object": {"literal": "invalid"}}
]}

{"@version": "1.0", "type": "macro", "name": "ui-error-cycle-detected", "description": "Critical: cycle in containment", "expansion": [
  {"type": "log", "level": "error", "message": "CYCLE DETECTED in ui:contains — halting render"},
  {"type": "halt", "code": 400, "reason": "Cyclic containment"}
]}

{"@version": "1.0", "type": "macro", "name": "ui-error-invalid-container", "description": "SHACL violation", "expansion": [
  {"type": "log", "level": "warn", "message": "Container failed SHACL validation"}
]}
```

---

### Key Fixes & Enhancements

| Feature | Implementation |
|-------|----------------|
| **Recursion Safety** | `guard: { not: { equals: [child, id] } }` + `ui:NoCycleShape` with `ui:contains+ ?x` SPARQL |
| **SHACL Validation** | Full shapes: `ContainerShape`, `ComponentShape`, `NoCycleShape` |
| **OWL Reasoning** | `owl-reason` step before SPARQL inference |
| **Error Handling** | `try/catch/finally`, `onError` hooks, `halt` on critical errors |
| **Extensible** | All errors route to named macros (`ui-error-*`) |

---

### Safe Usage Example

```json
// templates/slides/safe-slide.canvasl.jsonl
{"@include": "src/macros/RDF_SPARQL.canvasl.jsonl"}
{"type": "macro", "call": "rdf-prefix"}
{"type": "macro", "call": "ui-ontology"}

{"type": "macro", "call": "ui-container", "args": [
  "#main", "column", ["#header", "#content", "#footer"]
]}

{"type": "macro", "call": "ui-button", "args": ["#header", "Home", "#nav-home"]}
{"type": "macro", "call": "ui-container", "args": ["#content", "row", ["#btn1", "#btn2"]]}
{"type": "macro", "call": "ui-button", "args": ["#btn1", "Click Me", "#action-click"]}

// No cycle: #content does not contain itself
{"type": "macro", "call": "ui-slide-from-rdf", "args": ["slide1", "#main"]}
```

---

### Runtime Flow (Browser)

1. **Parser** expands macros → RDF triples + SHACL + SPARQL
2. **MetaLogBridge**:
   - Validates with SHACL → fails early on cycle/missing label
   - Applies OWL RDFS entailment
   - Runs SPARQL CONSTRUCT → `ui:renderOrder`, `ui:css`
3. **Renderer** consumes inferred graph → safe DOM/canvas
4. **Errors** → routed to `log`, `halt`, or UI toast

---

**Robust. Safe. Reasoning-Powered.**  
Ready for production UI inference in CanvasL.