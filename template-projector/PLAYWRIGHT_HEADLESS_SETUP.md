# Playwright Headless Chrome Setup

**Status**: ✅ **Fully Configured**

This project is set up for automated Playwright headless Chrome testing.

## Quick Start

### 1. Verify Setup

```bash
npm run test:playwright:verify
```

This will verify:
- ✅ Playwright package installation
- ✅ Configuration file exists
- ✅ Headless mode is configured
- ✅ Test files are present
- ✅ Chromium browser is installed

### 2. Run Tests (Headless Chrome)

```bash
# Run all tests in headless Chrome
npm run test:playwright:headless

# Or use the default command (also headless)
npm run test:playwright
```

### 3. View Test Report

```bash
npm run test:playwright:report
```

## Configuration

### Playwright Config (`playwright.config.js`)

- **Browser**: Chromium (headless mode enabled)
- **Viewport**: 1280x720
- **Base URL**: `http://localhost:3003` (matches vite.config.js)
- **Screenshots**: On failure only
- **Videos**: Retained on failure
- **Traces**: On first retry
- **Retries**: 2 on CI, 0 locally

### Headless Chrome Settings

```javascript
{
  name: 'chromium',
  use: { 
    ...devices['Desktop Chrome'],
    headless: true,              // Explicitly enabled
    viewport: { width: 1280, height: 720 },
    reducedMotion: 'reduce',     // Faster tests
  },
}
```

## Available Scripts

| Script | Description |
|--------|-------------|
| `npm run test:playwright` | Run all tests (headless) |
| `npm run test:playwright:headless` | Run tests in headless Chrome |
| `npm run test:playwright:ui` | Run tests with Playwright UI |
| `npm run test:playwright:headed` | Run tests with visible browser |
| `npm run test:playwright:debug` | Debug tests with inspector |
| `npm run test:playwright:report` | View HTML test report |
| `npm run test:playwright:install` | Install Chromium browser |
| `npm run test:playwright:install-deps` | Install Chromium with system dependencies |
| `npm run test:playwright:verify` | Verify Playwright setup |

## Test Files

All test files are located in `tests/`:

- `headless-chrome-verify.spec.js` - Setup verification tests
- `federation-verification.spec.js` - Federation tests
- `agent-protection.spec.js` - Agent protection tests
- `federation-full.spec.js` - Full federation suite
- `dbpedia.spec.js` - DBpedia integration tests
- `cors.spec.js` - CORS tests
- `error-recovery.spec.js` - Error handling tests
- `performance.spec.js` - Performance benchmarks
- `all.spec.js` - Test suite summary

## Running Specific Tests

```bash
# Run a specific test file
npx playwright test tests/headless-chrome-verify.spec.js

# Run tests matching a pattern
npx playwright test federation

# Run tests in a specific directory
npx playwright test tests/
```

## CI/CD Integration

### GitHub Actions Example

```yaml
name: Playwright Tests

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: actions/setup-node@v3
        with:
          node-version: 18
      - run: npm install
      - run: npm run test:playwright:install-deps
      - run: npm run test:playwright:headless
      - uses: actions/upload-artifact@v3
        if: always()
        with:
          name: playwright-report
          path: playwright-report/
```

## Troubleshooting

### Chromium Not Installed

```bash
npm run test:playwright:install
```

### Dev Server Not Starting

The config automatically starts the dev server. If it fails:

1. Check if port 3003 is in use: `lsof -i :3003`
2. Start dev server manually: `npm run dev`
3. The config will reuse existing server (`reuseExistingServer: true`)
4. Note: Vite may use a different port if 3003 is busy - check the server output

### Tests Timing Out

Increase timeouts in `playwright.config.js`:

```javascript
use: {
  actionTimeout: 60000,      // 60 seconds
  navigationTimeout: 60000,   // 60 seconds
}
```

### Module Import Errors

1. Ensure dev server is running
2. Verify test URLs are correct
3. Check browser console in headed mode: `npm run test:playwright:headed`

## Verification Test

Run the verification test to ensure everything works:

```bash
npx playwright test tests/headless-chrome-verify.spec.js
```

This test verifies:
- ✅ Headless Chrome is running
- ✅ Viewport size is correct
- ✅ JavaScript execution works
- ✅ Network requests are handled

## Test Results

Test results are saved to:
- **HTML Report**: `playwright-report/index.html`
- **JSON Results**: `test-results/results.json`
- **Screenshots**: `test-results/*.png` (on failure)
- **Videos**: `test-results/*.webm` (on failure)

## Best Practices

1. **Always run verification first**: `npm run test:playwright:verify`
2. **Use headless mode for CI/CD**: Faster and more reliable
3. **Use headed mode for debugging**: `npm run test:playwright:headed`
4. **Check test reports**: Review HTML reports for detailed results
5. **Keep tests isolated**: Each test should be independent

## Next Steps

1. ✅ Setup complete
2. ✅ Verification script created
3. ✅ Headless Chrome configured
4. ⏳ Run tests: `npm run test:playwright:headless`
5. ⏳ Review test results
6. ⏳ Integrate with CI/CD

## Related Documentation

- **PLAYWRIGHT_TESTING.md**: Complete Playwright testing guide
- **TEST_EXECUTION_GUIDE.md**: Manual testing guide
- **TEST_RESULTS.md**: Test results template
- **PERFORMANCE_REPORT.md**: Performance report template
