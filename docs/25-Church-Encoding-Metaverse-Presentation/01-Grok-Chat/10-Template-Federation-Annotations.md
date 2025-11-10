---
id: template-federation-annotations
title: "Template Federation Annotations"
level: advanced
type: code-example
tags: [sparql-federation, public-private-integration, rdf-star, agent-protection, cross-dataset]
keywords: [sparql-federation, public-private-integration, rdf-star-annotations, agent-protection, cross-dataset-queries]
prerequisites: [wikidata-properties-extension, sparql-agent-protection-system]
enables: [canvasl-semantic-slides-project, federated-knowledge-model]
related: [wikidata-properties-extension, sparql-agent-protection-system, canvasl-semantic-slides-project]
readingTime: 25
difficulty: 5
blackboard:
  status: active
  assignedAgent: "5D-Consensus-Agent"
  lastUpdate: "2025-01-07"
  dependencies: [sparql-federation, agent-protection-system]
  watchers: ["4D-Network-Agent", "6D-Intelligence-Agent"]
---

```json
// src/macros/RDF_Federated.canvasl.jsonl
// FEDERATED SPARQL EXAMPLES: Public + Private + Cross-Dataset
// All queries use SPARQL 1.1 SERVICE, RDF*, SHACL, and agent-protected private endpoints

{"@version": "1.0", "type": "macro", "name": "federated-prefix", "expansion": [
  {"type": "rdf-prefix", "prefix": "dbr", "uri": "http://dbpedia.org/resource/"},
  {"type": "rdf-prefix", "prefix": "dbo", "uri": "http://dbpedia.org/ontology/"},
  {"type": "rdf-prefix", "prefix": "wd", "uri": "http://www.wikidata.org/entity/"},
  {"type": "rdf-prefix", "prefix": "wdt", "uri": "http://www.wikidata.org/prop/direct/"},
  {"type": "rdf-prefix", "prefix": "geo", "uri": "http://www.geonames.org/ontology#"},
  {"type": "rdf-prefix", "prefix": "ui", "uri": "https://canvasl.org/ui#"},
  {"type": "rdf-prefix", "prefix": "anno", "uri": "https://canvasl.org/annotation#"},
  {"type": "rdf-prefix", "prefix": "prov", "uri": "http://www.w3.org/ns/prov#"}
]}

// 1. PUBLIC-ONLY: DBpedia + Wikidata
{"@version": "1.0", "type": "macro", "name": "dbpedia-wikidata-hybrid", "params": ["localId", "dbpediaId"], "expansion": [
  {"type": "sparql-construct", "query": "
    PREFIX dbr: <http://dbpedia.org/resource/>
    PREFIX dbo: <http://dbpedia.org/ontology/>
    PREFIX wd: <http://www.wikidata.org/entity/>
    PREFIX wdt: <http://www.wikidata.org/prop/direct/>
    CONSTRUCT { 
      ?local ui:abstract ?abstract .
      ?local ui:population ?pop .
      << ?local ui:population ?pop >> anno:source \"wikidata\" .
    }
    WHERE {
      SERVICE <https://dbpedia.org/sparql> {
        dbr:{dbpediaId} dbo:abstract ?abstract .
        FILTER(LANG(?abstract) = 'en')
      }
      SERVICE <https://query.wikidata.org/sparql> {
        BIND(dbr:{dbpediaId} AS ?dbr)
        ?wd wdt:P31 wd:Q515 ; wdt:P1082 ?pop .
        OPTIONAL { ?dbr owl:sameAs ?wd }
      }
    }
  ", "bindings": {"dbpediaId": {"var": "dbpediaId"}}, "cache": "10m"}
]}

// 2. PUBLIC + PRIVATE (Agent-Protected)
{"@version": "1.0", "type": "macro", "name": "dbpedia-private-notes", "params": ["localId", "dbpediaId", "privateEndpoint"], "expansion": [
  {"type": "sparql-construct", "query": "
    PREFIX dbr: <http://dbpedia.org/resource/>
    PREFIX dbo: <http://dbpedia.org/ontology/>
    PREFIX ui: <https://canvasl.org/ui#>
    CONSTRUCT { 
      ?local ui:summary ?summary .
      ?local ui:note ?note .
      << ?local ui:note ?note >> anno:private true ; prov:wasAttributedTo ?user .
    }
    WHERE {
      SERVICE <https://dbpedia.org/sparql> {
        dbr:{dbpediaId} dbo:abstract ?summary .
        FILTER(LANG(?summary) = 'en')
      }
      SERVICE <{privateEndpoint}> {
        ?local ui:personalNote ?note .
        ?local ui:consent true .
      }
    }
  ", "bindings": {"dbpediaId": {"var": "dbpediaId"}, "privateEndpoint": {"var": "privateEndpoint"}}}
]}

// 3. CROSS-DATASET: DBpedia + GeoNames + Private
{"@version": "1.0", "type": "macro", "name": "city-full-profile", "params": ["localId", "dbpediaId", "privateGraph"], "expansion": [
  {"type": "sparql-construct", "query": "
    PREFIX dbr: <http://dbpedia.org/resource/>
    PREFIX dbo: <http://dbpedia.org/ontology/>
    PREFIX geo: <http://www.geonames.org/ontology#>
    PREFIX ui: <https://canvasl.org/ui#>
    CONSTRUCT { 
      ?local ui:area ?area .
      ?local ui:elevation ?elev .
      ?local ui:visitNote ?note .
    }
    WHERE {
      SERVICE <https://dbpedia.org/sparql> {
        dbr:{dbpediaId} dbo:areaTotal ?area .
      }
      SERVICE <http://factforge.net/sparql> {
        dbr:{dbpediaId} geo:elevation ?elev .
      }
      SERVICE <{privateGraph}> {
        ?local ui:visitNote ?note .
        FILTER(?note != 'private')
      }
    }
  ", "bindings": {"dbpediaId": {"var": "dbpediaId"}, "privateGraph": {"var": "privateGraph"}}}
]}

// 4. CONDITIONAL FEDERATION (Agent Decision)
{"@version": "1.0", "type": "macro", "name": "conditional-enrich", "params": ["localId", "dbpediaId", "consent?"], "expansion": [
  {"type": "if", "condition": {"var": "consent"}, "then": [
    {"type": "macro", "call": "dbpedia-private-notes", "args": [{"var": "localId"}, {"var": "dbpediaId"}, "local://private"]}
  ], "else": [
    {"type": "macro", "call": "dbpedia-wikidata-hybrid", "args": [{"var": "localId"}, {"var": "dbpediaId"}]}
  ]}
]}

// 5. FULL SLIDE WITH FEDERATION
{"@version": "1.0", "type": "macro", "name": "slide-federated", "params": ["slideId", "root", "dbpediaId", "privateEndpoint?"], "expansion": [
  {"type": "try", "steps": [
    {"type": "macro", "call": "dbpedia-wikidata-hybrid", "args": [{"var": "root"}, {"var": "dbpediaId"}]},
    {"type": "if", "condition": {"var": "privateEndpoint"}, "then": [
      {"type": "macro", "call": "dbpedia-private-notes", "args": [{"var": "root"}, {"var": "dbpediaId"}, {"var": "privateEndpoint"}]}
    ]},
    {"type": "r5rs-call", "function": "render-slide", "args": [{"var": "slideId"}, {"var": "root"}]}
  ], "catch": "error", "finally": [
    {"type": "log", "level": "info", "message": "Federated slide rendered"}
  ]}
]}
```

---

### **Example: Los Angeles (Live in Browser)**

```json
// templates/slides/la-federated.canvasl.jsonl
{"@include": "src/macros/RDF_Federated.canvasl.jsonl"}
{"type": "macro", "call": "federated-prefix"}

{"type": "macro", "call": "ui-container", "args": ["#la", "column", ["#info", "#notes"]]}

{"type": "rdf-triple", "subject": "#la", "predicate": "schema:sameAs", "object": "dbr:Los_Angeles"}

{"type": "macro", "call": "slide-federated", "args": [
  "la-slide", "#la", "Los_Angeles", "local://user-blackboard"
]}
```

**Result (Browser-Executed)**:
```turtle
#la ui:abstract "Los Angeles is the largest city in California..." ;
    ui:population "3898747"^^xsd:integer ;
    ui:note "Visited in 2023 — great tacos" .

<< #la ui:note "..." >> anno:private true ; prov:wasAttributedTo "user@localhost" .
```

---

### **Runtime Execution (MetaLogBridge)**

```js
// SPARQL with SERVICE federation
const query = `
  SERVICE <https://dbpedia.org/sparql> { ... }
  SERVICE <local://user-blackboard> { ... }
`;
const result = await fetch('https://dbpedia.org/sparql', { method: 'POST', body: query });
```

---

**Federated. Protected. Live.**  
Public + private + cross-dataset — all in one query, all in your browser.