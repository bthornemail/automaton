# Test Suite

This directory contains comprehensive test suites for the CanvasL Semantic Slides Projector.

## Test Files

### 1. `e2e-test.html` - End-to-End Test Suite
**Purpose**: Comprehensive testing of all major features

**Tests**:
- Meta-Log Bridge Initialization
- DBpedia Plugin Loading
- DBpedia Abstract Query (Albert Einstein)
- DBpedia Thumbnail Query
- DBpedia Related Entities Query
- ProLog Query Execution
- DataLog Query Execution
- SPARQL Local Query
- Macro Expansion
- @include Directive
- Error Handling
- Slide Loading

**Usage**:
```bash
npm run test:e2e
# or
npm run preview
# Navigate to: http://localhost:4173/test/e2e-test.html
```

### 2. `dbpedia-test.html` - Basic DBpedia Test
**Purpose**: Quick verification of DBpedia integration

**Tests**:
- Meta-Log Bridge Initialization
- DBpedia Plugin Loading
- DBpedia Abstract Query
- Macro Expansion
- @include Directive

**Usage**:
```bash
npm run test:dbpedia
```

### 3. `cors-test.html` - CORS Verification
**Purpose**: Verify CORS works with DBpedia endpoint

**Tests**:
- Direct Fetch to DBpedia SPARQL Endpoint
- SPARQL Query via Fetch API
- DBpedia Plugin Query
- Error Handling (Invalid Endpoint)
- CORS Headers Check

**Usage**:
```bash
npm run test:cors
```

### 4. `error-recovery-test.html` - Error Recovery Testing
**Purpose**: Test error handling and recovery strategies

**Tests**:
- Network Error Recovery (Retry)
- Rate Limit Recovery
- Error Classification
- Error History
- Projector Error Recovery

**Usage**:
```bash
npm run test:recovery
```

## Running Tests

### Development Mode
```bash
npm run dev
# Navigate to: http://localhost:5173/test/[test-file].html
```

### Production Build
```bash
npm run build
npm run preview
# Navigate to: http://localhost:4173/test/[test-file].html
```

### Individual Tests
```bash
# End-to-end tests
npm run test:e2e

# DBpedia tests
npm run test:dbpedia

# CORS tests
npm run test:cors

# Error recovery tests
npm run test:recovery
```

## Test Results

All tests display results in the browser with:
- ✅ **Success** (green) - Test passed
- ❌ **Failure** (red) - Test failed
- ⏳ **Running** (yellow) - Test in progress

## Browser Compatibility

Tests should work in:
- Chrome 90+
- Firefox 88+
- Safari 14+
- Edge 90+

## Troubleshooting

### CORS Errors
If you see CORS errors:
1. Check that DBpedia endpoint is accessible
2. Verify browser allows CORS requests
3. Check network tab for actual error messages

### Module Import Errors
If you see module import errors:
1. Ensure you're running from dev server (`npm run dev`) or preview (`npm run preview`)
2. Check browser console for detailed error messages
3. Verify all dependencies are installed (`npm install`)

### Test Failures
If tests fail:
1. Check browser console for errors
2. Verify network connectivity
3. Check that DBpedia endpoint is responding
4. Verify meta-log-db is properly linked (`npm link meta-log-db`)

## Test Coverage

- **Unit Tests**: Individual component testing (via Jest - TODO)
- **Integration Tests**: Component interaction testing (e2e-test.html)
- **CORS Tests**: Cross-origin request testing (cors-test.html)
- **Error Recovery Tests**: Error handling verification (error-recovery-test.html)
- **Federation Tests**: SPARQL federation with SERVICE keyword
  - `federation-test.html` - 20 comprehensive tests
  - `federation-verification.html` - 8 parsing verification tests
  - `agent-protection-browser-test.html` - 7 agent protection tests
  - `performance-test.html` - 5 performance measurement tests
  - Total: 40+ federation-related tests

## Adding New Tests

To add a new test:

1. Create test file in `test/` directory
2. Add test script to `package.json`
3. Follow existing test patterns
4. Update this README

Example:
```javascript
async function testMyFeature() {
  const resultDiv = document.getElementById('test-result');
  resultDiv.innerHTML = '<span class="info">⏳ Testing...</span>';

  try {
    // Test code
    resultDiv.innerHTML = '<span class="success">✅ PASSED</span>';
  } catch (error) {
    resultDiv.innerHTML = `<span class="error">❌ FAILED</span> - ${error.message}`;
  }
}
```
