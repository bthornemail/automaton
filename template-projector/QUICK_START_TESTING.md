# Quick Start Testing Guide

**Quick reference for executing federation system tests**

## Prerequisites Check

```bash
cd /home/main/automaton/template-projector
npm run test:verify
```

This verifies:
- ✅ Build artifacts exist
- ✅ Test files are present
- ✅ npm scripts are configured
- ✅ Documentation is available

## Start Testing (3 Steps)

### Step 1: Build & Start Server

```bash
npm run build
npm run preview
```

Server starts on `http://localhost:4173`

### Step 2: Open Test URLs

Copy and paste these URLs into your browser:

**Phase 1: Manual Browser Testing**
- Federation Verification: `http://localhost:4173/test/federation-verification.html`
- Agent Protection: `http://localhost:4173/test/agent-protection-browser-test.html`
- Full Federation: `http://localhost:4173/test/federation-test.html`

**Phase 2: Real-World Endpoint Verification**
- DBpedia: `http://localhost:4173/test/dbpedia-test.html`
- CORS: `http://localhost:4173/test/cors-test.html`
- Error Recovery: `http://localhost:4173/test/error-recovery-test.html`

**Phase 3: Performance Benchmarking**
- Performance: `http://localhost:4173/test/performance-test.html`

### Step 3: Document Results

Update these files with test results:
- `TEST_RESULTS.md` - Test outcomes
- `PERFORMANCE_REPORT.md` - Performance metrics

## Quick Test Commands

```bash
# Individual test suites (opens in browser)
npm run test:federation-verify    # 8 parsing tests
npm run test:agent-protection     # 7 consent tests
npm run test:federation           # 20 comprehensive tests
npm run test:dbpedia              # 5 DBpedia tests
npm run test:cors                 # 5 CORS tests
npm run test:recovery             # 5 error recovery tests
npm run test:performance          # Performance benchmarking
```

## Test Checklist

### Phase 1: Manual Browser Testing
- [ ] Federation Verification (8 tests)
- [ ] Agent Protection (7 tests)
- [ ] Full Federation Suite (20 tests)

### Phase 2: Real-World Endpoint Verification
- [ ] DBpedia Testing (5 tests)
- [ ] Wikidata Integration
- [ ] CORS Verification (5 tests)
- [ ] Error Recovery (5 tests)

### Phase 3: Performance Benchmarking
- [ ] Performance Suite Execution
- [ ] Browser Profiling
- [ ] Optimization Analysis

## What to Look For

### ✅ Success Indicators
- Green checkmarks (✅) in test results
- Test dashboard shows high success rate (>95%)
- Response times meet targets (<2s single, <5s multiple)
- No console errors
- CORS headers present in network tab

### ❌ Failure Indicators
- Red X marks (❌) in test results
- Console errors
- CORS errors in network tab
- Response times exceed targets
- Tests timing out

## Browser DevTools Tips

### Network Tab
- Filter by "sparql" or "dbpedia"
- Check response headers for CORS
- Monitor request/response sizes
- Verify query parameters

### Console Tab
- Check for JavaScript errors
- Look for federation warnings
- Verify ProLog/DataLog execution

### Performance Tab
- Record during performance tests
- Analyze main thread activity
- Check memory usage
- Identify bottlenecks

## Troubleshooting

**CORS Errors**:
- Verify DBpedia endpoint is accessible
- Check browser CORS settings
- Use proxy if needed

**Module Import Errors**:
- Ensure running from preview server
- Check browser console for details
- Verify dependencies installed

**Test Failures**:
- Check browser console
- Verify network connectivity
- Check endpoint status
- Review error messages

## Full Documentation

For detailed instructions, see:
- **TEST_EXECUTION_GUIDE.md** - Complete step-by-step guide
- **docs/FEDERATION_TESTING.md** - Federation testing documentation
- **test/README.md** - Test suite documentation

## Support

If you encounter issues:
1. Check browser console for errors
2. Verify network connectivity
3. Check endpoint status (DBpedia, Wikidata)
4. Review TEST_EXECUTION_GUIDE.md for detailed troubleshooting
