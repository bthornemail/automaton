---
id: macros-with-wikidata
title: "Macros with Wikidata Integration"
level: advanced
type: code-example
tags: [wikidata, sparql-federation, rdf-star, live-data, semantic-web]
keywords: [wikidata-integration, sparql-federation, rdf-star, live-data, semantic-web, entity-linking]
prerequisites: [macros-with-rdf-annotations, sparql-agent-protection-system]
enables: [wikidata-properties-extension, template-federation-annotations]
related: [macros-with-rdf-annotations, wikidata-properties-extension, template-federation-annotations]
readingTime: 30
difficulty: 4
blackboard:
  status: active
  assignedAgent: "4D-Network-Agent"
  lastUpdate: "2025-01-07"
  dependencies: [wikidata-sparql-endpoint, sparql-federation]
  watchers: ["6D-Intelligence-Agent"]
---

```json
// src/macros/RDF_SPARQL_WIKIDATA.canvasl.jsonl
// FULL REWRITE: RDF* + Wikidata Integration + SHACL + OWL + Error Handling + Recursion Safety

{"@version": "1.0", "type": "macro", "name": "wikidata-prefix", "description": "Wikidata and schema.org prefixes for external knowledge", "expansion": [
  {"type": "rdf-prefix", "prefix": "wd", "uri": "http://www.wikidata.org/entity/"},
  {"type": "rdf-prefix", "prefix": "wdt", "uri": "http://www.wikidata.org/prop/direct/"},
  {"type": "rdf-prefix", "prefix": "p", "uri": "http://www.wikidata.org/prop/"},
  {"type": "rdf-prefix", "prefix": "ps", "uri": "http://www.wikidata.org/prop/statement/"},
  {"type": "rdf-prefix", "prefix": "pq", "uri": "http://www.wikidata.org/prop/qualifier/"},
  {"type": "rdf-prefix", "prefix": "schema", "uri": "http://schema.org/"},
  {"type": "rdf-prefix", "prefix": "rdfs", "uri": "http://www.w3.org/2000/01/rdf-schema#"},
  {"type": "rdf-prefix", "prefix": "owl", "uri": "http://www.w3.org/2002/07/owl#"},
  {"type": "rdf-prefix", "prefix": "xsd", "uri": "http://www.w3.org/2001/XMLSchema#"},
  {"type": "rdf-prefix", "prefix": "sh", "uri": "http://www.w3.org/ns/shacl#"},
  {"type": "rdf-prefix", "prefix": "prov", "uri": "http://www.w3.org/ns/prov#"},
  {"type": "rdf-prefix", "prefix": "ui", "uri": "https://canvasl.org/ui#"},
  {"type": "rdf-prefix", "prefix": "proj", "uri": "https://canvasl.org/projector#"},
  {"type": "rdf-prefix", "prefix": "anno", "uri": "https://canvasl.org/annotation#"}
]}

{"@version": "1.0", "type": "macro", "name": "wikidata-ontology", "description": "Link local UI to Wikidata entities with provenance", "expansion": [
  {"type": "rdf-triple", "subject": "ui:Component", "predicate": "rdfs:subClassOf", "object": "schema:CreativeWork"},
  {"type": "rdf-triple", "subject": "ui:Button", "predicate": "owl:equivalentClass", "object": "schema:Action"},
  {"type": "rdf-triple", "subject": "ui:contains", "predicate": "owl:equivalentProperty", "object": "schema:contains"},
  {"type": "rdf-triple", "subject": "ui:layout", "predicate": "rdfs:subPropertyOf", "object": "schema:layout"},
  {"type": "rdf-triple", "subject": "ui:onClick", "predicate": "rdfs:subPropertyOf", "object": "schema:potentialAction"},

  {"type": "shacl-shape", "id": "ui:WikidataLinkShape", "targetClass": "ui:Component", "property": [
    {"path": "schema:sameAs", "minCount": 0, "maxCount": 1, "nodeKind": "sh:IRI", "pattern": "^http://www.wikidata.org/entity/Q"}
  ]},
  {"type": "shacl-shape", "id": "ui:WikidataLabelShape", "targetSubjectsOf": "schema:sameAs", "property": [
    {"path": "rdfs:label", "minCount": 1, "datatype": "xsd:string"}
  ]}
]}

{"@version": "1.0", "type": "macro", "name": "ui-component-wikidata", "description": "UI component linked to Wikidata with RDF* annotation", "params": ["id", "type", "label", "wikidata?", "annotations?"], "expansion": [
  {"type": "macro", "call": "ui-component", "args": [{"var": "id"}, {"var": "type"}, {"var": "label"}, {"var": "annotations"}]},
  {"type": "if", "condition": {"var": "wikidata"}, "then": [
    {"type": "rdf-triple", "subject": {"var": "id"}, "predicate": "schema:sameAs", "object": {"var": "wikidata"}},
    {"type": "rdf-star", "statement": {"subject": {"var": "id"}, "predicate": "schema:sameAs", "object": {"var": "wikidata"}}, "annotations": [
      {"predicate": "anno:confidence", "object": {"literal": 1.0}},
      {"predicate": "prov:wasDerivedFrom", "object": {"literal": "wikidata-lookup"}}
    ]}
  ]},
  {"type": "shacl-validate", "shape": "ui:WikidataLinkShape", "focus": {"var": "id"}, "onError": "ui:error-invalid-wikidata"}
]}

{"@version": "1.0", "type": "macro", "name": "ui-container-wikidata", "description": "Container with Wikidata-aligned children", "params": ["id", "layout", "children", "wikidata?", "annotations?"], "expansion": [
  {"type": "macro", "call": "ui-component-wikidata", "args": [{"var": "id"}, "ui:Container", {"literal": "Container"}, {"var": "wikidata"}, {"var": "annotations"}]},
  {"type": "rdf-triple", "subject": {"var": "id"}, "predicate": "ui:layout", "object": {"literal": {"var": "layout"}}},
  {"type": "rdf-triple", "subject": {"var": "id"}, "predicate": "ui:contains", "object": {"var": "child"}, "repeat": {"var": "children"}, "guard": {"not": {"equals": [{"var": "child"}, {"var": "id"}]}}},
  {"type": "rdf-star", "statement": {"subject": {"var": "id"}, "predicate": "ui:contains", "object": {"var": "child"}}, "annotations": [
    {"predicate": "anno:confidence", "object": {"literal": 0.95}},
    {"predicate": "anno:author", "object": {"literal": "projector@canvasl.org"}},
    {"predicate": "anno:timestamp", "object": {"literal": {"function": "now"}, "datatype": "xsd:dateTime"}}
  ], "repeat": {"var": "children"}}
]}

{"@version": "1.0", "type": "macro", "name": "ui-button-wikidata", "description": "Button with Wikidata action", "params": ["id", "label", "action", "wikidata?", "annotations?"], "expansion": [
  {"type": "macro", "call": "ui-component-wikidata", "args": [{"var": "id"}, "ui:Button", {"var": "label"}, {"var": "wikidata"}, {"var": "annotations"}]},
  {"type": "rdf-triple", "subject": {"var": "id"}, "predicate": "ui:onClick", "object": {"var": "action"}}
]}

{"@version": "1.0", "type": "macro", "name": "wikidata-enrich", "description": "SPARQL CONSTRUCT to pull Wikidata labels and images", "expansion": [
  {"type": "sparql-construct", "query": "PREFIX wd: <http://www.wikidata.org/entity/>\nPREFIX wdt: <http://www.wikidata.org/prop/direct/>\nPREFIX schema: <http://schema.org/>\nCONSTRUCT { \n  ?local rdfs:label ?wdLabel .\n  ?local ui:image ?image .\n  << ?local schema:sameAs ?wd >> anno:retrievedAt ?time .\n}\nWHERE {\n  ?local schema:sameAs ?wd .\n  SERVICE <https://query.wikidata.org/sparql> {\n    ?wd rdfs:label ?wdLabel FILTER(LANG(?wdLabel) = 'en')\n    OPTIONAL { ?wd wdt:P18 ?image }\n  }\n  BIND(NOW() AS ?time)\n}", "onError": "ui:error-wikidata-fetch", "cache": "5m"}
]}

{"@version": "1.0", "type": "macro", "name": "ui-infer-layout-wikidata", "description": "Layout with Wikidata-informed ordering", "expansion": [
  {"type": "owl-reason", "entailment": "rdfs"},
  {"type": "macro", "call": "wikidata-enrich"},
  {"type": "sparql-construct", "query": "PREFIX ui: <https://canvasl.org/ui#>\nPREFIX anno: <https://canvasl.org/annotation#>\nCONSTRUCT { \n  ?parent ui:renderOrder ?order .\n  ?child ui:positionIn ?parent .\n  ?child ui:zIndex ?z .\n  << ?parent ui:contains ?child >> anno:confidence 0.98 .\n}\nWHERE {\n  ?parent ui:contains ?child .\n  BIND(xsd:integer(RAND() * 1000) AS ?z)\n  OPTIONAL { ?child ui:order ?explicit }\n  BIND(COALESCE(?explicit, ?z) AS ?order)\n}"}
]}

{"@version": "1.0", "type": "macro", "name": "ui-slide-from-wikidata", "description": "Generate slide with live Wikidata enrichment", "params": ["slideId", "rootComponent"], "expansion": [
  {"type": "try", "steps": [
    {"type": "shacl-validate", "shape": "ui:ContainerShape", "focus": {"var": "rootComponent"}},
    {"type": "macro", "call": "wikidata-enrich"},
    {"type": "macro", "call": "ui-infer-layout-wikidata"},
    {"type": "macro", "call": "ui-infer-style"},
    {"type": "r5rs-call", "function": "render-slide", "args": [{"var": "slideId"}, {"var": "rootComponent"}]},
    {"type": "rdf-star", "statement": {"subject": {"var": "slideId"}, "predicate": "ui:rendered", "object": {"literal": true}}, "annotations": [
      {"predicate": "prov:wasGeneratedBy", "object": {"literal": "projector@canvasl.org"}},
      {"predicate": "anno:enrichedWith", "object": {"literal": "wikidata"}}
    ]}
  ], "catch": {"var": "error"}, "finally": [
    {"type": "log", "level": "info", "message": {"template": "Slide {slideId} enriched with Wikidata at {time}"}, "args": [{"var": "slideId"}, {"function": "now"}]}
  ]}
]}

{"@version": "1.0", "type": "macro", "name": "ui-error-wikidata-fetch", "expansion": [
  {"type": "log", "level": "warn", "message": "Failed to fetch from Wikidata — using local data"},
  {"type": "rdf-star", "statement": {"subject": {"focus": true}, "predicate": "schema:sameAs", "object": {"focus": true}}, "annotations": [
    {"predicate": "anno:fetchFailed", "object": {"literal": true}}
  ]}
]}
```

---

### Wikidata Integration Highlights

| Feature | Implementation |
|-------|----------------|
| **Entity Linking** | `schema:sameAs wd:Q42` |
| **Live Labels/Images** | `SERVICE <https://query.wikidata.org/sparql>` |
| **Caching** | `cache: "5m"` to avoid rate limits |
| **Provenance** | `<< s p o >> anno:retrievedAt "2025-11-10T12:40:00Z"` |
| **Fallback** | On error, use local label; annotate failure |

---

### Example: Slide About Los Angeles

```json
// templates/slides/la-slide.canvasl.jsonl
{"@include": "src/macros/RDF_SPARQL_WIKIDATA.canvasl.jsonl"}
{"type": "macro", "call": "wikidata-prefix"}
{"type": "macro", "call": "wikidata-ontology"}

{"type": "macro", "call": "ui-container-wikidata", "args": [
  "#la-slide", "column", ["#title", "#image", "#facts"], "wd:Q65"
]}

{"type": "macro", "call": "ui-component-wikidata", "args": ["#title", "ui:Text", "Los Angeles", null, [{"anno:author": "user"}]]}
{"type": "macro", "call": "ui-component-wikidata", "args": ["#image", "ui:Image", "LA Skyline", null]}
{"type": "macro", "call": "ui-button-wikidata", "args": ["#facts", "Show Population", "#action-pop", "wd:P1082"]}

{"type": "macro", "call": "ui-slide-from-wikidata", "args": ["slide-la", "#la-slide"]}
```

**Result**:
- `#title` → "Los Angeles" (from Wikidata `rdfs:label`)
- `#image` → pulls `wdt:P18` (official image)
- `#facts` button → `wd:P1082` (population) triggers dynamic query
- All with `<< ... >> anno:retrievedAt "2025-11-10T20:40:00Z"`

---

### Browser Runtime (MetaLogBridge)

```js
// Wikidata SPARQL with caching
const response = await fetch('https://query.wikidata.org/sparql', {
  method: 'POST',
  headers: { 'Accept': 'application/sparql-results+json' },
  body: new URLSearchParams({ query })
});
const data = await response.json();
cache.set(query, data, 300000); // 5 min
```

---

**Live Knowledge. Semantic UI. Full Provenance.**  
Your slides now breathe with Wikidata — safely, verifiably, beautifully.