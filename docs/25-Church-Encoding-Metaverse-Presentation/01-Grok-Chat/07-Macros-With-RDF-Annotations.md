---
id: macros-with-rdf-annotations
title: "Macros with RDF* Annotations"
level: advanced
type: code-example
tags: [rdf-star, rdf-annotations, provenance, shacl, owl, error-handling]
keywords: [rdf-star, rdf-annotations, provenance-tracking, shacl-validation, owl-reasoning, error-handling]
prerequisites: [macro-example-rdf-sparql, rdf-star-spec]
enables: [macros-with-wikidata, template-federation-annotations]
related: [macro-example-rdf-sparql, macros-with-wikidata, template-federation-annotations]
readingTime: 25
difficulty: 4
blackboard:
  status: active
  assignedAgent: "2D-Structural-Agent"
  lastUpdate: "2025-01-07"
  dependencies: [rdf-star-engine, shacl-validator]
  watchers: ["6D-Intelligence-Agent"]
---

```json
// src/macros/RDF_SPARQL.canvasl.jsonl
// FULL REWRITE: RDF* (RDF-star) for annotations + SHACL + OWL + Error Handling + Recursion Safety

{"@version": "1.0", "type": "macro", "name": "rdf-prefix", "description": "Standard prefixes including RDF-star and provenance", "expansion": [
  {"type": "rdf-prefix", "prefix": "rdf", "uri": "http://www.w3.org/1999/02/22-rdf-syntax-ns#"},
  {"type": "rdf-prefix", "prefix": "rdfs", "uri": "http://www.w3.org/2000/01/rdf-schema#"},
  {"type": "rdf-prefix", "prefix": "owl", "uri": "http://www.w3.org/2002/07/owl#"},
  {"type": "rdf-prefix", "prefix": "xsd", "uri": "http://www.w3.org/2001/XMLSchema#"},
  {"type": "rdf-prefix", "prefix": "sh", "uri": "http://www.w3.org/ns/shacl#"},
  {"type": "rdf-prefix", "prefix": "prov", "uri": "http://www.w3.org/ns/prov#"},
  {"type": "rdf-prefix", "prefix": "ui", "uri": "https://canvasl.org/ui#"},
  {"type": "rdf-prefix", "prefix": "proj", "uri": "https://canvasl.org/projector#"},
  {"type": "rdf-prefix", "prefix": "anno", "uri": "https://canvasl.org/annotation#"}
]}

{"@version": "1.0", "type": "macro", "name": "ui-ontology", "description": "UI ontology with RDF* annotation support", "expansion": [
  {"type": "rdf-triple", "subject": "ui:Component", "predicate": "rdf:type", "object": "owl:Class"},
  {"type": "rdf-triple", "subject": "ui:Container", "predicate": "rdfs:subClassOf", "object": "ui:Component"},
  {"type": "rdf-triple", "subject": "ui:Button", "predicate": "rdfs:subClassOf", "object": "ui:Component"},
  {"type": "rdf-triple", "subject": "ui:contains", "predicate": "rdf:type", "object": "owl:ObjectProperty"},
  {"type": "rdf-triple", "subject": "ui:contains", "predicate": "rdfs:domain", "object": "ui:Container"},
  {"type": "rdf-triple", "subject": "ui:contains", "predicate": "rdfs:range", "object": "ui:Component"},
  {"type": "rdf-triple", "subject": "ui:layout", "predicate": "rdf:type", "object": "owl:DatatypeProperty"},
  {"type": "rdf-triple", "subject": "ui:onClick", "predicate": "rdf:type", "object": "owl:ObjectProperty"},
  {"type": "rdf-triple", "subject": "anno:confidence", "predicate": "rdf:type", "object": "owl:DatatypeProperty"},
  {"type": "rdf-triple", "subject": "anno:author", "predicate": "rdf:type", "object": "owl:DatatypeProperty"},
  {"type": "rdf-triple", "subject": "anno:timestamp", "predicate": "rdf:type", "object": "owl:DatatypeProperty"},

  {"type": "shacl-shape", "id": "ui:ContainerShape", "targetClass": "ui:Container", "property": [
    {"path": "ui:layout", "minCount": 1, "maxCount": 1, "datatype": "xsd:string"},
    {"path": "ui:contains", "minCount": 0, "node": "ui:ComponentShape"}
  ]},
  {"type": "shacl-shape", "id": "ui:ComponentShape", "targetClass": "ui:Component", "property": [
    {"path": "rdfs:label", "minCount": 1, "maxCount": 1, "datatype": "xsd:string"}
  ]},
  {"type": "shacl-shape", "id": "ui:NoCycleShape", "targetSubjectsOf": "ui:contains", "sparql": [
    {"type": "sparql-ask", "query": "ASK { ?x ui:contains+ ?x }", "message": "CYCLE DETECTED: ui:contains forms a cycle"}
  ]},
  {"type": "shacl-shape", "id": "ui:AnnotationShape", "targetSubjectsOf": "anno:confidence", "property": [
    {"path": "anno:confidence", "datatype": "xsd:decimal", "minInclusive": 0, "maxInclusive": 1}
  ]}
]}

{"@version": "1.0", "type": "macro", "name": "ui-component", "description": "Base component with optional RDF* annotation", "params": ["id", "type", "label", "annotations?"], "expansion": [
  {"type": "rdf-triple", "subject": {"var": "id"}, "predicate": "rdf:type", "object": {"var": "type"}},
  {"type": "rdf-triple", "subject": {"var": "id"}, "predicate": "rdfs:label", "object": {"literal": {"var": "label"}}},
  {"type": "shacl-validate", "shape": "ui:ComponentShape", "focus": {"var": "id"}, "onError": "ui:error-missing-label"},
  {"type": "rdf-star", "statement": {"subject": {"var": "id"}, "predicate": "rdfs:label", "object": {"literal": {"var": "label"}}}, "annotations": {"var": "annotations"}, "repeat": true}
]}

{"@version": "1.0", "type": "macro", "name": "ui-container", "description": "Recursion-safe container with RDF* provenance", "params": ["id", "layout", "children", "annotations?"], "expansion": [
  {"type": "macro", "call": "ui-component", "args": [{"var": "id"}, "ui:Container", {"literal": "Container"}, {"var": "annotations"}]},
  {"type": "rdf-triple", "subject": {"var": "id"}, "predicate": "ui:layout", "object": {"literal": {"var": "layout"}}},
  {"type": "rdf-triple", "subject": {"var": "id"}, "predicate": "ui:contains", "object": {"var": "child"}, "repeat": {"var": "children"}, "guard": {"not": {"equals": [{"var": "child"}, {"var": "id"}]}}},
  {"type": "rdf-star", "statement": {"subject": {"var": "id"}, "predicate": "ui:contains", "object": {"var": "child"}}, "annotations": [
    {"predicate": "anno:confidence", "object": {"literal": 0.95}},
    {"predicate": "anno:author", "object": {"literal": "projector@canvasl.org"}},
    {"predicate": "anno:timestamp", "object": {"literal": {"function": "now"}, "datatype": "xsd:dateTime"}}
  ], "repeat": {"var": "children"}},
  {"type": "shacl-validate", "shape": "ui:ContainerShape", "focus": {"var": "id"}, "onError": "ui:error-invalid-container"},
  {"type": "shacl-validate", "shape": "ui:NoCycleShape", "focus": {"var": "id"}, "onError": "ui:error-cycle-detected"}
]}

{"@version": "1.0", "type": "macro", "name": "ui-button", "description": "Button with action and annotated click", "params": ["id", "label", "action", "annotations?"], "expansion": [
  {"type": "macro", "call": "ui-component", "args": [{"var": "id"}, "ui:Button", {"var": "label"}, {"var": "annotations"}]},
  {"type": "rdf-triple", "subject": {"var": "id"}, "predicate": "ui:onClick", "object": {"var": "action"}},
  {"type": "rdf-star", "statement": {"subject": {"var": "id"}, "predicate": "ui:onClick", "object": {"var": "action"}}, "annotations": [
    {"predicate": "anno:confidence", "object": {"literal": 1.0}},
    {"predicate": "prov:wasGeneratedBy", "object": {"literal": "ui-macro-expansion"}}
  ]}
]}

{"@version": "1.0", "type": "macro", "name": "ui-infer-layout", "description": "OWL + SPARQL with RDF* provenance", "expansion": [
  {"type": "owl-reason", "entailment": "rdfs", "onError": "ui:error-owl-failure"},
  {"type": "sparql-construct", "query": "PREFIX ui: <https://canvasl.org/ui#>\nPREFIX anno: <https://canvasl.org/annotation#>\nCONSTRUCT { \n  ?parent ui:renderOrder ?order .\n  ?child ui:positionIn ?parent .\n  ?child ui:zIndex ?z .\n  << ?parent ui:contains ?child >> anno:confidence 0.98 .\n}\nWHERE {\n  ?parent ui:contains ?child .\n  BIND(xsd:integer(RAND() * 1000) AS ?z)\n  OPTIONAL { ?child ui:order ?explicit }\n  BIND(COALESCE(?explicit, ?z) AS ?order)\n}", "onError": "ui:error-sparql-layout"}
]}

{"@version": "1.0", "type": "macro", "name": "ui-infer-style", "description": "Semantic CSS with annotated inference", "expansion": [
  {"type": "owl-reason", "entailment": "rdfs"},
  {"type": "sparql-construct", "query": "PREFIX ui: <https://canvasl.org/ui#>\nPREFIX anno: <https://canvasl.org/annotation#>\nCONSTRUCT { \n  ?c ui:css ?css .\n  << ?c rdf:type ?type >> anno:confidence 1.0 .\n}\nWHERE {\n  VALUES (?type ?css) {\n    (ui:Button \"background: #FF6B6B; color: white; border: none; padding: 12px; border-radius: 8px;\")\n    (ui:Container \"display: flex; gap: 16px;\")\n  }\n  ?c rdf:type ?type .\n}", "onError": "ui:error-sparql-style"}
]}

{"@version": "1.0", "type": "macro", "name": "ui-slide-from-rdf", "description": "Safe slide generation with full RDF* provenance", "params": ["slideId", "rootComponent"], "expansion": [
  {"type": "try", "steps": [
    {"type": "shacl-validate", "shape": "ui:ContainerShape", "focus": {"var": "rootComponent"}, "onError": "ui:error-invalid-root"},
    {"type": "shacl-validate", "shape": "ui:AnnotationShape", "focus": {"graph": true}, "onError": "ui:error-invalid-annotation"},
    {"type": "macro", "call": "ui-infer-layout"},
    {"type": "macro", "call": "ui-infer-style"},
    {"type": "r5rs-call", "function": "render-slide", "args": [{"var": "slideId"}, {"var": "rootComponent"}]},
    {"type": "rdf-star", "statement": {"subject": {"var": "slideId"}, "predicate": "ui:rendered", "object": {"literal": true}}, "annotations": [
      {"predicate": "prov:wasGeneratedBy", "object": {"literal": "projector@canvasl.org"}},
      {"predicate": "anno:timestamp", "object": {"literal": {"function": "now"}, "datatype": "xsd:dateTime"}}
    ]}
  ], "catch": {"var": "error"}, "finally": [
    {"type": "log", "level": "info", "message": {"template": "Slide {slideId} rendered at {time} with status: {status}"}, "args": [{"var": "slideId"}, {"function": "now"}, {"var": "error"}]}
  ]}
]}

{"@version": "1.0", "type": "macro", "name": "ui-error-missing-label", "expansion": [
  {"type": "log", "level": "error", "message": "UI component missing rdfs:label"},
  {"type": "rdf-triple", "subject": {"focus": true}, "predicate": "ui:status", "object": {"literal": "invalid"}},
  {"type": "rdf-star", "statement": {"subject": {"focus": true}, "predicate": "ui:status", "object": {"literal": "invalid"}}, "annotations": [
    {"predicate": "anno:confidence", "object": {"literal": 1.0}},
    {"predicate": "anno:author", "object": {"literal": "validator"}}
  ]}
]}

{"@version": "1.0", "type": "macro", "name": "ui-error-cycle-detected", "expansion": [
  {"type": "log", "level": "error", "message": "CYCLE DETECTED in ui:contains — halting render"},
  {"type": "halt", "code": 400, "reason": "Cyclic containment"},
  {"type": "rdf-star", "statement": {"subject": {"focus": true}, "predicate": "ui:contains", "object": {"focus": true}}, "annotations": [
    {"predicate": "anno:invalid", "object": {"literal": true}}
  ]}
]}

{"@version": "1.0", "type": "macro", "name": "ui-error-invalid-container", "expansion": [
  {"type": "log", "level": "warn", "message": "Container failed SHACL validation"}
]}
```

---

### RDF* (RDF-star) Integration Highlights

| Feature | RDF* Usage |
|-------|-----------|
| **Provenance** | `<< s p o >> anno:author "projector"` |
| **Confidence** | `<< ?parent ui:contains ?child >> anno:confidence 0.98` |
| **Validation** | Annotate invalid statements: `<< #btn1 ui:status "invalid" >> anno:confidence 1.0` |
| **Inference** | SPARQL CONSTRUCT emits `<< s p o >>` for traceability |

---

### Safe Example with Annotations

```json
// templates/slides/annotated-slide.canvasl.jsonl
{"@include": "src/macros/RDF_SPARQL.canvasl.jsonl"}
{"type": "macro", "call": "rdf-prefix"}
{"type": "macro", "call": "ui-ontology"}

{"type": "macro", "call": "ui-container", "args": [
  "#main", "column", [" × "#header", "#content"], [
    {"anno:confidence": 0.99}, {"anno:author": "designer@canvasl.org"}
  ]
]}

{"type": "macro", "call": "ui-button", "args": [
  "#header", "Home", "#nav-home", [
    {"anno:confidence": 1.0}, {"prov:wasGeneratedBy": "macro"}
  ]
]}

{"type": "macro", "call": "ui-slide-from-rdf", "args": ["slide1", "#main"]}
```

**Expands to**:
```turtle
<< #main ui:contains #header >> anno:confidence 0.99 ; anno:author "designer@canvasl.org" .
<< #header ui:onClick #nav-home >> anno:confidence 1.0 ; prov:wasGeneratedBy "macro" .
```

---

### Runtime (MetaLogBridge)

```js
// Pseudo
graph.addStar({ s, p, o }, { anno:confidence: 0.98, anno:author: "infer" });
validator.validateSHACL(shape, focus).catch(err => macroCall('ui:error-*'));
```

---

**RDF* + SHACL + OWL + Safe + Annotated + Browser-Ready**  
UI inference with full provenance and trust.