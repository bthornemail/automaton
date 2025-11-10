---
id: wikidata-properties-extension
title: "Wikidata Properties Extension"
level: advanced
type: code-example
tags: [wikidata-properties, property-queries, live-data, rdf-star, semantic-enrichment]
keywords: [wikidata-properties, property-queries, live-data-enrichment, rdf-star-annotations, semantic-enrichment]
prerequisites: [macros-with-wikidata, sparql-agent-protection-system]
enables: [template-federation-annotations, canvasl-semantic-slides-project]
related: [macros-with-wikidata, template-federation-annotations, canvasl-semantic-slides-project]
readingTime: 30
difficulty: 4
blackboard:
  status: active
  assignedAgent: "4D-Network-Agent"
  lastUpdate: "2025-01-07"
  dependencies: [wikidata-sparql-endpoint, property-mapping]
  watchers: ["6D-Intelligence-Agent"]
---

```json
// src/macros/RDF_SPARQL_WIKIDATA.canvasl.jsonl
// FULL REWRITE: Wikidata Property Queries + Live Data + RDF* + SHACL + OWL + Error Handling

{"@version": "1.0", "type": "macro", "name": "wikidata-prefix", "description": "Wikidata core and property prefixes", "expansion": [
  {"type": "rdf-prefix", "prefix": "wd", "uri": "http://www.wikidata.org/entity/"},
  {"type": "rdf-prefix", "prefix": "wdt", "uri": "http://www.wikidata.org/prop/direct/"},
  {"type": "rdf-prefix", "prefix": "p", "uri": "http://www.wikidata.org/prop/"},
  {"type": "rdf-prefix", "prefix": "ps", "uri": "http://www.wikidata.org/prop/statement/"},
  {"type": "rdf-prefix", "prefix": "pq", "uri": "http://www.wikidata.org/prop/qualifier/"},
  {"type": "rdf-prefix", "prefix": "pr", "uri": "http://www.wikidata.org/prop/reference/"},
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

{"@version": "1.0", "type": "macro", "name": "wikidata-property-query", "description": "Generic macro to query any Wikidata property", "params": ["localId", "wikidataId", "property", "targetPredicate", "unit?", "qualifier?"], "expansion": [
  {"type": "sparql-construct", "query": {
    "template": "PREFIX wd: <http://www.wikidata.org/entity/>\nPREFIX wdt: <http://www.wikidata.org/prop/direct/>\nPREFIX pq: <http://www.wikidata.org/prop/qualifier/>\nCONSTRUCT { \n  ?local {targetPredicate} ?value .\n  << ?local {targetPredicate} ?value >> anno:retrievedAt ?time .\n  << ?local {targetPredicate} ?value >> anno:source \"wikidata\" .\n}\nWHERE {\n  BIND({wikidataId} AS ?wd)\n  ?wd {property} ?value .\n  OPTIONAL { ?stmt pq:{qualifier} ?qval . }\n  BIND(NOW() AS ?time)\n}"
  }, "bindings": {
    "wikidataId": {"var": "wikidataId"},
    "property": {"var": "property"},
    "targetPredicate": {"var": "targetPredicate"},
    "qualifier": {"var": "qualifier"}
  }, "onError": "ui:error-wikidata-property", "cache": "10m"}
]}

{"@version": "1.0", "type": "macro", "name": "wikidata-population", "description": "P1082 - Population", "params": ["localId", "wikidataId"], "expansion": [
  {"type": "macro", "call": "wikidata-property-query", "args": [
    {"var": "localId"}, {"var": "wikidataId"}, "wdt:P1082", "ui:population", null, "P585"
  ]},
  {"type": "rdf-star", "statement": {"subject": {"var": "localId"}, "predicate": "ui:population", "object": {"var": "value"}}, "annotations": [
    {"predicate": "schema:unitText", "object": {"literal": "people"}}
  ], "repeat": true}
]}

{"@version": "1.0", "type": "macro", "name": "wikidata-area", "description": "P2046 - Area (km²)", "params": ["localId", "wikidataId"], "expansion": [
  {"type": "macro", "call": "wikidata-property-query", "args": [
    {"var": "localId"}, {"var": "wikidataId"}, "wdt:P2046", "ui:area", "km²", "P518"
  ]}
]}

{"@version": "1.0", "type": "macro", "name": "wikidata-elevation", "description": "P2044 - Elevation above sea level (m)", "params": ["localId", "wikidataId"], "expansion": [
  {"type": "macro", "call": "wikidata-property-query", "args": [
    {"var": "localId"}, {"var": "wikidataId"}, "wdt:P2044", "ui:elevation", "m"
  ]}
]}

{"@version": "1.0", "type": "macro", "name": "wikidata-inception", "description": "P571 - Inception date", "params": ["localId", "wikidataId"], "expansion": [
  {"type": "macro", "call": "wikidata-property-query", "args": [
    {"var": "localId"}, {"var": "wikidataId"}, "wdt:P571", "ui:founded", null
  ]},
  {"type": "r5rs-call", "function": "format-date", "args": [{"var": "value"}]}
]}

{"@version": "1.0", "type": "macro", "name": "wikidata-coordinates", "description": "P625 - Geographic coordinates", "params": ["localId", "wikidataId"], "expansion": [
  {"type": "sparql-construct", "query": "PREFIX wd: <http://www.wikidata.org/entity/>\nPREFIX wdt: <http://www.wikidata.org/prop/direct/>\nCONSTRUCT { \n  ?local ui:latitude ?lat .\n  ?local ui:longitude ?lon .\n  << ?local ui:latitude ?lat >> anno:source \"wikidata\" .\n}\nWHERE {\n  BIND({wikidataId} AS ?wd)\n  ?wd wdt:P625 ?coord .\n  BIND(str(?coord) AS ?str)\n  BIND(xsd:double(REPLACE(?str, \".*point\\\\(([^ ]+) .*\", \"$1\")) AS ?lon)\n  BIND(xsd:double(REPLACE(?str, \".*point[^ ]+ ([^\\\\)]+)\\\\).*\", \"$1\")) AS ?lat)\n}", "bindings": {"wikidataId": {"var": "wikidataId"}}}
]}

{"@version": "1.0", "type": "macro", "name": "wikidata-image", "description": "P18 - Image", "params": ["localId", "wikidataId"], "expansion": [
  {"type": "macro", "call": "wikidata-property-query", "args": [
    {"var": "localId"}, {"var": "wikidataId"}, "wdt:P18", "ui:image", null
  ]},
  {"type": "r5rs-call", "function": "wikimedia-commons-url", "args": [{"var": "value"}]}
]}

{"@version": "1.0", "type": "macro", "name": "wikidata-official-website", "description": "P856 - Official website", "params": ["localId", "wikidataId"], "expansion": [
  {"type": "macro", "call": "wikidata-property-query", "args": [
    {"var": "localId"}, {"var": "wikidataId"}, "wdt:P856", "ui:website"
  ]}
]}

{"@version": "1.0", "type": "macro", "name": "wikidata-instance-of", "description": "P31 - Instance of (for type inference)", "params": ["localId", "wikidataId"], "expansion": [
  {"type": "sparql-construct", "query": "PREFIX wd: <http://www.wikidata.org/entity/>\nPREFIX wdt: <http://www.wikidata.org/prop/direct/>\nCONSTRUCT { ?local rdf:type ?type . }\nWHERE {\n  BIND({wikidataId} AS ?wd)\n  ?wd wdt:P31 ?instance .\n  BIND(IRI(CONCAT(\"ui:\", REPLACE(STR(?instance), \".*/\", \"\"))) AS ?type)\n}", "bindings": {"wikidataId": {"var": "wikidataId"}}}
]}

{"@version": "1.0", "type": "macro", "name": "wikidata-enrich-all", "description": "Enrich with common properties", "params": ["localId", "wikidataId"], "expansion": [
  {"type": "macro", "call": "wikidata-population", "args": [{"var": "localId"}, {"var": "wikidataId"}]},
  {"type": "macro", "call": "wikidata-area", "args": [{"var": "localId"}, {"var": "wikidataId"}]},
  {"type": "macro", "call": "wikidata-elevation", "args": [{"var": "localId"}, {"var": "wikidataId"}]},
  {"type": "macro", "call": "wikidata-coordinates", "args": [{"var": "localId"}, {"var": "wikidataId"}]},
  {"type": "macro", "call": "wikidata-image", "args": [{"var": "localId"}, {"var": "wikidataId"}]},
  {"type": "macro", "call": "wikidata-official-website", "args": [{"var": "localId"}, {"var": "wikidataId"}]},
  {"type": "macro", "call": "wikidata-instance-of", "args": [{"var": "localId"}, {"var": "wikidataId"}]}
]}

{"@version": "1.0", "type": "macro", "name": "ui-slide-from-wikidata", "description": "Full slide with live property queries", "params": ["slideId", "rootComponent", "wikidataId"], "expansion": [
  {"type": "try", "steps": [
    {"type": "shacl-validate", "shape": "ui:ContainerShape", "focus": {"var": "rootComponent"}},
    {"type": "macro", "call": "wikidata-enrich-all", "args": [{"var": "rootComponent"}, {"var": "wikidataId"}]},
    {"type": "macro", "call": "ui-infer-layout-wikidata"},
    {"type": "macro", "call": "ui-infer-style"},
    {"type": "r5rs-call", "function": "render-slide", "args": [{"var": "slideId"}, {"var": "rootComponent"}]},
    {"type": "rdf-star", "statement": {"subject": {"var": "slideId"}, "predicate": "ui:rendered", "object": {"literal": true}}, "annotations": [
      {"predicate": "prov:wasGeneratedBy", "object": {"literal": "projector@canvasl.org"}},
      {"predicate": "anno:enrichedWith", "object": {"literal": "wikidata"}}
    ]}
  ], "catch": {"var": "error"}, "finally": [
    {"type": "log", "level": "info", "message": {"template": "Slide {slideId} enriched with Wikidata properties at {time}"}, "args": [{"var": "slideId"}, {"function": "now"}]}
  ]}
]}

{"@version": "1.0", "type": "macro", "name": "ui-error-wikidata-property", "expansion": [
  {"type": "log", "level": "warn", "message": {"template": "Failed to fetch property {property} for {wikidataId}"}, "args": [{"focus": "property"}, {"focus": "wikidataId"}]},
  {"type": "rdf-star", "statement": {"subject": {"focus": true}, "predicate": {"focus": "targetPredicate"}, "object": {"literal": "N/A"}}, "annotations": [
    {"predicate": "anno:fetchFailed", "object": {"literal": true}}
  ]}
]}
```

---

### Example: Los Angeles Slide with Property Queries

```json
// templates/slides/la-slide.canvasl.jsonl
{"@include": "src/macros/RDF_SPARQL_WIKIDATA.canvasl.jsonl"}
{"type": "macro", "call": "wikidata-prefix"}
{"type": "macro", "call": "wikidata-ontology"}

{"type": "macro", "call": "ui-container-wikidata", "args": [
  "#la", "column", ["#title", "#stats", "#map", "#btn"], "wd:Q65"
]}

{"type": "macro", "call": "ui-component-wikidata", "args": ["#title", "ui:Text", "Los Angeles", "wd:Q65"]}
{"type": "macro", "call": "ui-component-wikidata", "args": ["#stats", "ui:Card", "City Stats", null]}
{"type": "macro", "call": "ui-component-wikidata", "args": ["#map", "ui:Map", "Location", null]}
{"type": "macro", "call": "ui-button-wikidata", "args": ["#btn", "Show Website", "#action-website", "wd:P856"]}

{"type": "macro", "call": "ui-slide-from-wikidata", "args": ["slide-la", "#la", "wd:Q65"]}
```

**Live Output (inferred)**:
```turtle
#la ui:population "3,898,747"^^xsd:integer ;
    ui:area "1302"^^xsd:decimal ;
    ui:elevation "93"^^xsd:decimal ;
    ui:latitude "34.05"^^xsd:double ;
    ui:longitude "-118.24"^^xsd:double ;
    ui:image "https://upload.wikimedia.org/wikipedia/commons/..." ;
    ui:website "https://lacity.gov" .

<< #la ui:population "3898747" >> anno:retrievedAt "2025-11-10T12:41:00Z"^^xsd:dateTime .
```

---

### Supported Properties

| Macro | Wikidata | UI Predicate | Unit |
|------|----------|--------------|------|
| `wikidata-population` | P1082 | `ui:population` | people |
| `wikidata-area` | P2046 | `ui:area` | km² |
| `wikidata-elevation` | P2044 | `ui:elevation` | m |
| `wikidata-coordinates` | P625 | `ui:latitude/longitude` | ° |
| `wikidata-image` | P18 | `ui:image` | URL |
| `wikidata-official-website` | P856 | `ui:website` | URL |
| `wikidata-inception` | P571 | `ui:founded` | date |

---

**Live. Accurate. Verifiable.**  
Your UI now pulls real-time, citable data from Wikidata — with full provenance, fallbacks, and browser caching.