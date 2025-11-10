# Federation Query Optimization Guide

**Version**: 1.0  
**Last Updated**: 2025-01-07

## Overview

This guide describes optimization strategies for federated SPARQL queries in CanvasL Semantic Slides, focusing on VALUES binding optimization, query rewriting efficiency, and performance measurement.

## Optimization Strategies

### 1. VALUES Binding Optimization

**Principle**: Use VALUES clauses before SERVICE blocks to constrain remote queries.

**Before (Inefficient)**:
```sparql
SELECT ?city ?pop WHERE {
  SERVICE <https://dbpedia.org/sparql> {
    ?city dbo:populationTotal ?pop .
    FILTER(?city IN (dbr:Los_Angeles, dbr:New_York))
  }
}
```

**After (Optimized)**:
```sparql
VALUES ?city { "Los_Angeles" "New_York" }
SELECT ?city ?pop WHERE {
  SERVICE <https://dbpedia.org/sparql> {
    ?city dbo:populationTotal ?pop .
  }
}
```

**Benefits**:
- Reduces remote query scope
- Minimizes network traffic
- Faster query execution
- Lower endpoint load

### 2. Query Rewriting Efficiency

**Optimization**: Only include VALUES for variables actually used in SERVICE block.

**Implementation**:
```javascript
// Extract variables used in query
const usedVariables = extractVariables(serviceQuery);

// Only add VALUES for used variables
for (const [variable, values] of Object.entries(valuesBindings)) {
  if (usedVariables.has(variable)) {
    // Add VALUES clause
  }
}
```

**Performance Impact**: Reduces query size and parsing overhead.

### 3. Result Joining Optimization

**Strategy**: Efficient deduplication using normalized binding keys.

**Implementation**:
```javascript
// Create normalized key for comparison
function createBindingKey(binding) {
  const keys = Object.keys(binding).sort();
  return keys.map(key => `${key}:${binding[key].value}`).join('|');
}
```

**Benefits**:
- Faster deduplication
- Lower memory usage
- Preserves binding structure

### 4. Network Call Minimization

**Strategy**: Batch multiple queries when possible.

**Example**:
```sparql
-- Instead of multiple queries:
-- Query 1: Abstract
-- Query 2: Thumbnail
-- Query 3: BirthDate

-- Use single query:
SELECT ?abstract ?thumbnail ?birthDate WHERE {
  SERVICE <https://dbpedia.org/sparql> {
    dbr:Einstein dbo:abstract ?abstract ;
                 dbo:thumbnail ?thumbnail ;
                 dbo:birthDate ?birthDate .
  }
}
```

## Performance Targets

### Query Execution Times

| Query Type | Target | Acceptable |
|------------|--------|------------|
| Single SERVICE | < 2s | < 5s |
| Multiple SERVICE | < 5s | < 10s |
| VALUES Optimized | < 1s | < 3s |
| Query Rewriting | < 100ms | < 500ms |
| Result Joining | < 50ms | < 200ms |

### Network Efficiency

- **VALUES Optimization**: 50-80% reduction in query size
- **Batch Queries**: 60-70% reduction in network calls
- **Caching**: 90%+ cache hit rate for repeated queries

## Measurement Tools

### Performance Test Suite

**File**: `test/performance-test.html`

**Metrics Tracked**:
- Average query duration
- Min/Max duration
- Success rate
- Network call count
- VALUES optimization impact

**Usage**:
```bash
npm run test:performance
```

### Verification Tests

**File**: `test/federation-verification.html`

**Tests**:
- SERVICE block parsing accuracy
- VALUES extraction correctness
- Query rewriting verification
- Binding flow validation

**Usage**:
```bash
npm run test:federation-verify
```

## Best Practices

### 1. Always Use VALUES for Constraints

```sparql
-- ✅ Good
VALUES ?id { "Q42" "Q123" }
SERVICE <endpoint> { ?id ?p ?o . }

-- ❌ Bad
SERVICE <endpoint> {
  ?id ?p ?o .
  FILTER(?id IN (<Q42>, <Q123>))
}
```

### 2. Order SERVICE Blocks Efficiently

```sparql
-- ✅ Good: Local first, then remote
SELECT ?local ?remote WHERE {
  ?local local:property ?value .
  SERVICE <remote> {
    ?remote remote:property ?value .
  }
}

-- ❌ Bad: Remote first
SELECT ?local ?remote WHERE {
  SERVICE <remote> {
    ?remote remote:property ?value .
  }
  ?local local:property ?value .
}
```

### 3. Minimize SERVICE Block Count

```sparql
-- ✅ Good: Single SERVICE block
SERVICE <endpoint> {
  ?s dbo:abstract ?abstract ;
     dbo:thumbnail ?thumbnail .
}

-- ❌ Bad: Multiple SERVICE blocks
SERVICE <endpoint> { ?s dbo:abstract ?abstract . }
SERVICE <endpoint> { ?s dbo:thumbnail ?thumbnail . }
```

### 4. Use LIMIT Appropriately

```sparql
-- ✅ Good: Limit remote results
SERVICE <endpoint> {
  ?s ?p ?o .
}
LIMIT 10

-- ❌ Bad: Fetch all results
SERVICE <endpoint> {
  ?s ?p ?o .
}
```

## Optimization Checklist

- [ ] VALUES clauses used before SERVICE blocks
- [ ] Only necessary variables in VALUES
- [ ] SERVICE blocks ordered efficiently
- [ ] LIMIT clauses applied
- [ ] Query rewriting overhead < 100ms
- [ ] Result joining < 50ms
- [ ] Network calls minimized
- [ ] Caching enabled for repeated queries

## Performance Monitoring

### Key Metrics

1. **Query Duration**: Track average, min, max, p50, p95
2. **Network Calls**: Count SERVICE block executions
3. **Success Rate**: Percentage of successful queries
4. **Cache Hit Rate**: Percentage of cached results
5. **VALUES Impact**: Improvement with VALUES optimization

### Measurement Tools

- `test/performance-test.html` - Automated performance testing
- Browser DevTools - Network tab for real-time monitoring
- Performance API - `performance.now()` for precise timing

## Troubleshooting

### Slow Queries

1. **Check VALUES Usage**: Ensure VALUES constraints are applied
2. **Verify Query Rewriting**: Check if rewriting adds overhead
3. **Monitor Network**: Check for unnecessary SERVICE calls
4. **Review Joining**: Verify result joining efficiency

### High Network Usage

1. **Enable Caching**: Use query result caching
2. **Batch Queries**: Combine multiple queries
3. **Use VALUES**: Constrain remote queries
4. **Limit Results**: Apply LIMIT clauses

## Related Documentation

- **Federation Testing**: `docs/FEDERATION_TESTING.md`
- **Plugin Extension**: `docs/PLUGIN_EXTENSION_GUIDE.md`
- **Browser Compatibility**: `docs/BROWSER_COMPATIBILITY.md`
