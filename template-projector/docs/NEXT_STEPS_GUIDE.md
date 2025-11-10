# Federation System - Next Steps Guide

**Version**: 1.0  
**Last Updated**: 2025-01-07  
**Status**: ✅ Ready for Production Testing

## Overview

The federation system is now ready for manual testing, real-world endpoint verification, performance benchmarking, and production deployment. This guide provides step-by-step instructions for each phase.

---

## 1. Manual Browser Testing

### Quick Start

All test suites are ready to run. Start the preview server and run individual test suites:

```bash
# Build first (if not already built)
npm run build

# Run individual test suites
npm run test:federation-verify    # SERVICE parsing verification (8 tests)
npm run test:agent-protection      # Agent protection browser tests (7 tests)
npm run test:performance           # Performance measurement (5 tests)
npm run test:federation            # Full federation test suite (20 tests)
npm run test:e2e                   # End-to-end integration tests
npm run test:dbpedia               # DBpedia integration tests
npm run test:cors                  # CORS verification tests
npm run test:recovery              # Error recovery tests
```

### Test Suite Overview

| Test Suite | Tests | Purpose | Duration |
|------------|-------|---------|----------|
| `federation-verify` | 8 | SERVICE block parsing verification | ~30s |
| `agent-protection` | 7 | Consent management & access control | ~1min |
| `performance` | 5 | Performance measurement & optimization | ~2min |
| `federation` | 20 | Complete federation functionality | ~5min |
| `e2e` | 12 | End-to-end integration | ~3min |
| `dbpedia` | 5 | DBpedia-specific tests | ~1min |
| `cors` | 5 | CORS verification | ~30s |
| `recovery` | 5 | Error recovery scenarios | ~1min |

**Total**: 67+ test cases covering all federation functionality

### Manual Testing Checklist

- [ ] **SERVICE Block Parsing**
  - [ ] Single SERVICE block parses correctly
  - [ ] Multiple SERVICE blocks handled
  - [ ] Nested SERVICE blocks supported
  - [ ] Complex patterns with FILTER work

- [ ] **VALUES Optimization**
  - [ ] VALUES extraction works for single variables
  - [ ] VALUES extraction works for multiple variables
  - [ ] VALUES bindings passed to SERVICE blocks
  - [ ] Query rewriting includes VALUES correctly

- [ ] **Agent Protection**
  - [ ] Consent granting works
  - [ ] Consent revocation works
  - [ ] Access control blocks unauthorized queries
  - [ ] ProLog integration functions correctly
  - [ ] Multiple users supported

- [ ] **Performance**
  - [ ] Single SERVICE query < 5 seconds
  - [ ] Multiple SERVICE queries < 10 seconds
  - [ ] VALUES optimization improves performance
  - [ ] Query rewriting overhead < 100ms
  - [ ] Result joining < 50ms

---

## 2. Real-World Endpoint Verification

### Configured Endpoints

The system is pre-configured with these endpoints:

#### Public Endpoints (No Consent Required)

1. **DBpedia SPARQL**
   - URL: `https://dbpedia.org/sparql`
   - Status: ✅ Configured
   - Test Query:
     ```sparql
     SELECT ?abstract WHERE {
       SERVICE <https://dbpedia.org/sparql> {
         dbr:Albert_Einstein dbo:abstract ?abstract .
         FILTER(LANG(?abstract) = "en")
       }
     }
     LIMIT 1
     ```

2. **Wikidata SPARQL**
   - URL: `https://query.wikidata.org/sparql`
   - Status: ✅ Configured
   - Test Query:
     ```sparql
     SELECT ?label WHERE {
       SERVICE <https://query.wikidata.org/sparql> {
         wd:Q42 rdfs:label ?label .
         FILTER(LANG(?label) = "en")
       }
     }
     LIMIT 1
     ```

#### Private Endpoints (Consent Required)

3. **Local Blackboard**
   - URL: `local://blackboard`
   - Status: ✅ Configured (requires consent)
   - Usage: Test agent protection system

### Endpoint Verification Steps

#### Step 1: Verify DBpedia Endpoint

```bash
# Run DBpedia test suite
npm run test:dbpedia

# Expected: All tests pass, queries return results
```

**Manual Verification**:
1. Open browser DevTools (Network tab)
2. Run `npm run test:dbpedia`
3. Verify:
   - ✅ CORS requests succeed
   - ✅ SPARQL queries return JSON results
   - ✅ Response time < 5 seconds
   - ✅ No CORS errors in console

#### Step 2: Verify Wikidata Endpoint

```bash
# Run federation test suite (includes Wikidata)
npm run test:federation

# Expected: Test 5 (Full Federation) passes
```

**Manual Verification**:
1. Open browser DevTools (Network tab)
2. Run `npm run test:federation`
3. Check Test 5: "Full Federation (DBpedia + Wikidata)"
4. Verify:
   - ✅ Both endpoints queried successfully
   - ✅ Results joined correctly
   - ✅ Cross-dataset queries work

#### Step 3: Test VALUES Optimization

```bash
# Run performance test suite
npm run test:performance

# Expected: VALUES optimization shows improvement
```

**Manual Verification**:
1. Run `npm run test:performance`
2. Click "Test VALUES Optimization"
3. Verify:
   - ✅ With VALUES: Faster query execution
   - ✅ Query rewriting includes VALUES clause
   - ✅ Network calls minimized

### Adding Custom Endpoints

To add a new endpoint for testing:

```javascript
// In test file or application code
federation.registerEndpoint('https://your-endpoint.com/sparql', {
  url: 'https://your-endpoint.com/sparql',
  requiresConsent: false,  // or true for private endpoints
  timeout: 30000,
  retries: 3
});

// Test query
const query = `
  SELECT ?s ?p ?o WHERE {
    SERVICE <https://your-endpoint.com/sparql> {
      ?s ?p ?o .
    }
  }
  LIMIT 10
`;

const result = await federation.executeFederatedQuery(query);
```

---

## 3. Performance Benchmarking

### Performance Test Suite

The performance test suite (`test/performance-test.html`) provides comprehensive metrics:

#### Metrics Tracked

- **Average Duration**: Mean query execution time
- **Min/Max Duration**: Range of execution times
- **Total Queries**: Number of queries executed
- **Success Rate**: Percentage of successful queries
- **Network Calls**: Count of SERVICE block executions

#### Running Performance Tests

```bash
# Run performance test suite
npm run test:performance

# Interactive dashboard opens in browser
```

#### Performance Targets

| Metric | Target | Acceptable |
|--------|--------|------------|
| Single SERVICE Query | < 2s | < 5s |
| Multiple SERVICE Queries | < 5s | < 10s |
| VALUES Optimized Query | < 1s | < 3s |
| Query Rewriting Overhead | < 100ms | < 500ms |
| Result Joining | < 50ms | < 200ms |

#### Benchmarking Workflow

1. **Baseline Measurement**
   ```bash
   npm run test:performance
   # Click "Run Performance Tests"
   # Record baseline metrics
   ```

2. **VALUES Optimization Test**
   ```bash
   # In performance test page
   # Click "Test VALUES Optimization"
   # Compare with/without VALUES
   ```

3. **Query Rewriting Test**
   ```bash
   # In performance test page
   # Check "Query Rewriting Overhead" test
   # Verify < 100ms overhead
   ```

4. **Result Joining Test**
   ```bash
   # In performance test page
   # Check "Result Joining" test
   # Verify < 50ms for mock endpoints
   ```

### Performance Optimization Checklist

- [ ] Single SERVICE queries meet target (< 2s)
- [ ] Multiple SERVICE queries meet target (< 5s)
- [ ] VALUES optimization shows improvement (50-80% reduction)
- [ ] Query rewriting overhead acceptable (< 100ms)
- [ ] Result joining efficient (< 50ms)
- [ ] Network calls minimized via VALUES
- [ ] Cache hit rate > 90% for repeated queries

---

## 4. Production Deployment

### Pre-Deployment Checklist

#### Build Verification

- [x] Production build successful (`npm run build`)
- [x] No linting errors
- [x] All test scripts configured
- [x] Bundle size acceptable (< 200KB gzipped)

#### Configuration

- [ ] Endpoints configured correctly
- [ ] CORS settings verified
- [ ] Error handling tested
- [ ] Agent protection configured (if needed)

#### Testing

- [ ] All test suites pass
- [ ] Real-world endpoints verified
- [ ] Performance benchmarks met
- [ ] Error recovery tested

### Deployment Steps

#### Step 1: Build Production Bundle

```bash
cd template-projector
npm run build

# Verify build output
ls -lh dist/
# Expected: HTML, CSS, JS bundles
```

#### Step 2: Verify Build Output

```bash
# Check bundle sizes
du -sh dist/*

# Expected sizes:
# - main bundle: ~175KB (45KB gzipped)
# - dbpedia plugin: ~4.6KB (1.7KB gzipped)
# - Total: < 200KB gzipped
```

#### Step 3: Test Production Build

```bash
# Preview production build
npm run preview

# Run test suites against production build
npm run test:federation-verify
npm run test:performance
```

#### Step 4: Deploy to Production

**Option A: Static Hosting (Netlify, Vercel, GitHub Pages)**

```bash
# Build
npm run build

# Deploy dist/ directory
# - Netlify: Drag & drop dist/ folder
# - Vercel: vercel --prod
# - GitHub Pages: Push dist/ to gh-pages branch
```

**Option B: Docker Deployment**

```dockerfile
# Dockerfile (if needed)
FROM nginx:alpine
COPY dist/ /usr/share/nginx/html/
EXPOSE 80
CMD ["nginx", "-g", "daemon off;"]
```

**Option C: CDN Deployment**

```bash
# Upload dist/ to CDN
# Configure CORS headers
# Set up endpoint URLs
```

### Production Configuration

#### Environment Variables

```javascript
// Configure endpoints in production
const ENDPOINTS = {
  dbpedia: 'https://dbpedia.org/sparql',
  wikidata: 'https://query.wikidata.org/sparql',
  // Add custom endpoints
};

// Register endpoints
federation.registerEndpoint(ENDPOINTS.dbpedia, {
  requiresConsent: false,
  timeout: 30000
});
```

#### CORS Configuration

Ensure production server allows CORS:

```nginx
# nginx.conf
add_header Access-Control-Allow-Origin *;
add_header Access-Control-Allow-Methods "GET, POST, OPTIONS";
add_header Access-Control-Allow-Headers "Content-Type";
```

### Post-Deployment Verification

- [ ] Production build loads correctly
- [ ] DBpedia queries work
- [ ] Wikidata queries work
- [ ] Performance metrics acceptable
- [ ] Error handling works
- [ ] Agent protection functions (if enabled)
- [ ] CORS configured correctly

---

## Quick Reference

### Test Commands

```bash
# All test suites
npm run test:federation-verify    # Parsing verification
npm run test:agent-protection      # Agent protection
npm run test:performance           # Performance measurement
npm run test:federation            # Full federation suite
npm run test:e2e                   # End-to-end tests
npm run test:dbpedia               # DBpedia tests
npm run test:cors                  # CORS tests
npm run test:recovery              # Error recovery
```

### Endpoint URLs

- **DBpedia**: `https://dbpedia.org/sparql`
- **Wikidata**: `https://query.wikidata.org/sparql`
- **Local Blackboard**: `local://blackboard` (requires consent)

### Performance Targets

- Single SERVICE: < 2s (target), < 5s (acceptable)
- Multiple SERVICE: < 5s (target), < 10s (acceptable)
- VALUES Optimized: < 1s (target), < 3s (acceptable)
- Query Rewriting: < 100ms overhead
- Result Joining: < 50ms

### Documentation

- **Federation Testing**: `docs/FEDERATION_TESTING.md`
- **Optimization Guide**: `docs/OPTIMIZATION_GUIDE.md`
- **Test README**: `test/README.md`
- **Federation Implementation**: `FEDERATION_IMPLEMENTATION.md`

---

## Troubleshooting

### Common Issues

#### CORS Errors

**Problem**: CORS errors when querying endpoints

**Solution**:
1. Verify endpoint allows CORS
2. Check browser console for specific error
3. Test with `npm run test:cors`
4. Verify endpoint URL is correct

#### Slow Queries

**Problem**: Queries take too long

**Solution**:
1. Use VALUES optimization
2. Add LIMIT clauses
3. Check network tab for bottlenecks
4. Verify endpoint is responsive
5. Run performance tests to identify issues

#### Agent Protection Not Working

**Problem**: Consent not being checked

**Solution**:
1. Verify `requiresConsent: true` is set
2. Check ProLog facts are loaded
3. Run `npm run test:agent-protection`
4. Verify consent granting works

#### Query Rewriting Issues

**Problem**: VALUES not included in rewritten query

**Solution**:
1. Run `npm run test:federation-verify`
2. Check VALUES extraction works
3. Verify variables are used in SERVICE block
4. Check query rewriting logic

---

## Support

For issues or questions:

1. **Check Documentation**:
   - `docs/FEDERATION_TESTING.md`
   - `docs/OPTIMIZATION_GUIDE.md`
   - `test/README.md`

2. **Run Test Suites**:
   - Identify failing tests
   - Check browser console for errors
   - Review test output

3. **Performance Issues**:
   - Run `npm run test:performance`
   - Check metrics dashboard
   - Review optimization guide

---

**Status**: ✅ Ready for Production  
**Last Verified**: 2025-01-07  
**Build Status**: ✅ Passing  
**Test Coverage**: 67+ test cases
