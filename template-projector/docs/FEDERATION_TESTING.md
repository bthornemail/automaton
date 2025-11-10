# Federated SPARQL Query Testing Guide

**Version**: 1.0  
**Last Updated**: 2025-01-07

## Overview

This guide describes the comprehensive federated SPARQL query testing suite for CanvasL Semantic Slides. The test suite covers SERVICE federation, VALUES binding optimization, agent protection, and error recovery scenarios.

## Test Suite Structure

### Test File: `test/federation-test.html`

**Total Tests**: 20 comprehensive federation tests

### Test Categories

#### 1. Unit Tests (Tests 1-4)
**Purpose**: Isolate components and test individual federation features

- **Test 1**: Basic SERVICE Binding Flow
  - Tests VALUES binding constrains remote queries
  - Verifies no "whole dataset" fetches

- **Test 2**: VALUES Interplay (SPARQL 1.1)
  - Tests VALUES limits remote query results
  - Verifies query optimization

- **Test 3**: Agent Protection (ProLog Integration)
  - Tests consent-based access control
  - Verifies private endpoints are protected

- **Test 4**: VALUES Constraint Optimization
  - Tests VALUES bindings passed to SERVICE blocks
  - Verifies query rewriting efficiency

#### 2. Integration Tests (Tests 5-10)
**Purpose**: Test real endpoints and federation scenarios

- **Test 5**: Full Federation (DBpedia + Wikidata)
  - Tests public endpoint federation
  - Verifies cross-dataset queries

- **Test 6**: Private Federation with Consent
  - Tests public + private federation
  - Verifies consent-based access

- **Test 7**: Error Recovery (Partial Failure)
  - Tests partial results when one SERVICE fails
  - Verifies error recovery strategies

- **Test 8**: Cross-Dataset Federation
  - Tests multiple properties from same endpoint
  - Verifies query composition

- **Test 9**: Multiple SERVICE Blocks
  - Tests queries with multiple SERVICE blocks
  - Verifies result joining

- **Test 10**: Binding Flow Optimization
  - Tests VALUES bindings optimize queries
  - Verifies network efficiency

#### 3. End-to-End Tests (Tests 11-15)
**Purpose**: Test full CanvasL workflow integration

- **Test 11**: E2E Slide Federation
  - Tests slide loading with federation
  - Verifies DBpedia integration

- **Test 12**: Agent Denial E2E
  - Tests consent denial in full workflow
  - Verifies private data protection

- **Test 13**: Federation with Macro Expansion
  - Tests macros with SERVICE queries
  - Verifies macro + federation integration

- **Test 14**: Offline Fallback
  - Tests offline detection
  - Verifies graceful degradation

- **Test 15**: RDF* Provenance Annotations
  - Tests provenance tracking
  - Verifies RDF* annotations

#### 4. Advanced Tests (Tests 16-20)
**Purpose**: Test advanced federation scenarios

- **Test 16**: Query Rewriting Efficiency
  - Tests SERVICE block parsing
  - Verifies VALUES extraction

- **Test 17**: Network Variability Handling
  - Tests retry strategies
  - Verifies partial failure recovery

- **Test 18**: Rate Limit Recovery
  - Tests rate limit handling
  - Verifies exponential backoff

- **Test 19**: Complex Join Patterns
  - Tests multi-SERVICE joins
  - Verifies result combination

- **Test 20**: Performance Measurement
  - Tests query performance
  - Verifies <5s target

## Components

### SparqlFederation Class

**File**: `src/utils/SparqlFederation.js`

**Features**:
- SERVICE block parsing
- VALUES binding extraction
- Agent consent checking
- Federated query execution
- Result joining
- Error recovery

**Usage**:
```javascript
import { SparqlFederation } from '../src/utils/SparqlFederation.js';

const federation = new SparqlFederation(metaLogBridge, errorHandler);

// Register endpoints
federation.registerEndpoint('https://dbpedia.org/sparql', {
  url: 'https://dbpedia.org/sparql',
  requiresConsent: false
});

// Execute federated query
const result = await federation.executeFederatedQuery(query, {
  recoverPartial: true
});
```

### AgentProtection Class

**File**: `src/utils/AgentProtection.js`

**Features**:
- ProLog-based consent management
- Access control checking
- Consent granting/revoking

**Usage**:
```javascript
import { AgentProtection } from '../src/utils/AgentProtection.js';

const agentProtection = new AgentProtection(metaLogBridge);
await agentProtection.init();

// Grant consent
await agentProtection.grantConsent('user', 'local://blackboard');

// Check access
const allowed = await agentProtection.checkAccess('user', 'local://blackboard', query);
```

## Running Tests

### Development Mode
```bash
npm run dev
# Navigate to: http://localhost:5173/test/federation-test.html
```

### Production Preview
```bash
npm run build
npm run preview
# Navigate to: http://localhost:4173/test/federation-test.html
```

### Direct Test Script
```bash
npm run test:federation
```

## Test Scenarios

### Scenario 1: Public Federation
```sparql
SELECT ?city ?pop WHERE {
  SERVICE <https://dbpedia.org/sparql> {
    dbr:Los_Angeles dbo:populationTotal ?pop .
  }
}
```

### Scenario 2: VALUES Optimization
```sparql
VALUES ?city { "Los_Angeles" "New_York" }
SERVICE <https://dbpedia.org/sparql> {
  ?city dbo:populationTotal ?pop .
}
```

### Scenario 3: Private Federation
```sparql
SERVICE <https://dbpedia.org/sparql> {
  dbr:Einstein dbo:abstract ?pub .
}
SERVICE <local://blackboard> {
  ?s ui:personalNote ?priv .
}
```

### Scenario 4: Error Recovery
```sparql
SERVICE <https://dbpedia.org/sparql> {
  ?s dbo:abstract ?abstract .
}
SERVICE <mock-fail> {
  ?s ?p ?o .
}
```

## Best Practices

### 1. VALUES Binding Optimization
Always use VALUES before SERVICE to constrain queries:

```sparql
VALUES ?id { "Einstein" "Newton" }
SERVICE <endpoint> {
  ?person rdfs:label ?id .
}
```

### 2. Error Recovery
Enable partial recovery for federated queries:

```javascript
const result = await federation.executeFederatedQuery(query, {
  recoverPartial: true
});
```

### 3. Agent Protection
Always check consent for private endpoints:

```javascript
const hasConsent = await agentProtection.checkAccess('user', endpoint, query);
if (!hasConsent) {
  // Handle denial
}
```

### 4. Mock Endpoints
Use mock endpoints for testing:

```javascript
federation.registerMockEndpoint('mock://endpoint', async (query, bindings) => {
  return {
    results: {
      bindings: [/* mock data */]
    }
  };
});
```

## Performance Targets

- **Query Execution**: < 5 seconds for federated queries
- **Network Calls**: Minimize via VALUES optimization
- **Error Recovery**: < 3 retries with exponential backoff
- **Result Joining**: Efficient for multiple SERVICE blocks

## Troubleshooting

### CORS Issues
If you see CORS errors:
1. Check endpoint allows CORS
2. Use proxy server for development
3. Verify headers are correct

### Consent Denied
If queries are denied:
1. Check `agentProtection.getConsentStatus()`
2. Verify endpoint requires consent
3. Grant consent with `agentProtection.grantConsent()`

### Partial Failures
If some SERVICE blocks fail:
1. Enable `recoverPartial: true`
2. Check error recovery strategies
3. Verify error classification

## Related Documentation

- **SPARQL 1.1 Federated Query**: https://www.w3.org/TR/sparql11-federated-query/
- **FedBench**: SPARQL federation benchmarks
- **Plugin Extension Guide**: `docs/PLUGIN_EXTENSION_GUIDE.md`
- **Browser Compatibility**: `docs/BROWSER_COMPATIBILITY.md`

## Status

**Federation Testing**: ✅ **COMPLETE**  
**Test Coverage**: 20 comprehensive tests  
**Components**: SparqlFederation, AgentProtection  
**Ready for**: Production use
