# MetaLogDbBrowser E2E Test Suite

Comprehensive end-to-end tests for the browser-native `MetaLogDbBrowser` implementation.

## Overview

This test suite validates the complete functionality of `MetaLogDbBrowser` including:
- Initialization and configuration
- Canvas loading from URLs
- ProLog query execution
- DataLog query execution
- SPARQL query execution
- SHACL validation
- IndexedDB caching
- R5RS function execution
- Fact extraction and RDF conversion
- Error handling
- Cache strategies

## Running Tests

```bash
# Run all MetaLogDbBrowser tests
npm run test:playwright tests/meta-log-browser-db.spec.js

# Run with UI
npm run test:playwright:ui tests/meta-log-browser-db.spec.js

# Run in headed mode (see browser)
npm run test:playwright:headed tests/meta-log-browser-db.spec.js

# Run specific test
npm run test:playwright tests/meta-log-browser-db.spec.js -g "should initialize"
```

## Test Structure

The tests use Playwright to run in a real browser environment:

1. **Setup**: Each test navigates to `/viewer.html` and waits for the Projector to initialize
2. **Access**: Tests access `MetaLogDbBrowser` via `window.projector.metaLog.adapter`
3. **Execution**: Tests execute operations in the browser context using `page.evaluate()`
4. **Assertions**: Results are returned to Node.js context for assertions

## Test Coverage

### Core Functionality
- ✅ Initialization
- ✅ Canvas loading from URLs
- ✅ Fact extraction
- ✅ RDF triple conversion

### Query Engines
- ✅ ProLog queries
- ✅ DataLog queries
- ✅ SPARQL queries
- ✅ SHACL validation

### Advanced Features
- ✅ R5RS function execution
- ✅ ProLog rules
- ✅ DataLog rules
- ✅ RDF triple storage and querying

### Caching and Storage
- ✅ IndexedDB caching
- ✅ Cache clearing
- ✅ Different cache strategies (memory, IndexedDB, both)

### Error Handling
- ✅ Graceful error handling for missing files

## Prerequisites

- Development server running (`npm run dev`)
- `meta-log-db` package linked (`npm link meta-log-db`)
- Playwright installed (`npm run test:playwright:install`)

## Test Environment

Tests run against:
- **Base URL**: `http://localhost:3003` (or `TEMPLATE_PROJECTOR_PORT` env var)
- **Test Page**: `/viewer.html` (loads Projector with MetaLogDbBrowser)
- **Browser**: Chromium (headless by default)

## Debugging

### View Test Execution
```bash
npm run test:playwright:ui
```

### Run in Headed Mode
```bash
npm run test:playwright:headed tests/meta-log-browser-db.spec.js
```

### Debug Specific Test
```bash
npm run test:playwright:debug tests/meta-log-browser-db.spec.js -g "should initialize"
```

### Check Console Logs
Tests log to browser console. Check Playwright report for console output.

## Expected Results

All tests should pass when:
- `MetaLogDbBrowser` is properly initialized
- Canvas files are accessible at `/jsonl/automaton-kernel.jsonl`
- IndexedDB is available in the browser
- All engines (ProLog, DataLog, RDF, SHACL) are enabled

## Troubleshooting

### Tests Fail to Initialize
- Check that `meta-log-db` package is linked: `npm link meta-log-db`
- Verify browser console for import errors
- Check that `/viewer.html` loads successfully

### Canvas Loading Fails
- Verify `/jsonl/automaton-kernel.jsonl` exists and is accessible
- Check network tab for 404 errors
- Verify Vite is serving JSONL files correctly

### IndexedDB Errors
- Check browser console for IndexedDB errors
- Verify IndexedDB is enabled in browser
- Try clearing browser data and retrying

### Query Execution Fails
- Check that engines are enabled in adapter config
- Verify facts are loaded before querying
- Check browser console for query errors

## Related Documentation

- **MetaLogDbBrowser API**: `docs/27-Meta-Log-Browser-Db/BROWSER-API-REFERENCE.md`
- **Architecture**: `docs/27-Meta-Log-Browser-Db/ARCHITECTURE.md`
- **Migration Guide**: `docs/27-Meta-Log-Browser-Db/MIGRATION-GUIDE.md`

