# Playwright E2E Testing Setup

**Status**: ✅ **Fully Configured and Working**

This project has Playwright E2E tests set up and working with headless Chrome.

## Quick Start

### Run All Tests

```bash
npm run test:playwright
```

### Run Specific Test Suites

```bash
# Basic tests (no server required)
npm run test:playwright:example

# Init tests (verify setup)
npm run test:playwright:init

# E2E tests (full application tests)
npm run test:playwright:e2e

# Setup tests (configuration verification)
npm run test:playwright:setup
```

## Test Files

### 1. `tests/basic.spec.js`
Basic Playwright tests that verify the framework is working:
- ✅ Basic test execution
- ✅ Page object availability
- ✅ Navigation capabilities
- ✅ Screenshot functionality
- ✅ JavaScript evaluation

### 2. `tests/init.spec.js`
Initialization tests that verify the application loads:
- ✅ Application loads successfully
- ✅ Page title is present
- ✅ JavaScript execution works
- ✅ Network requests are handled

### 3. `tests/e2e.spec.js`
End-to-end tests for the viewer application:
- ✅ Viewer page loads
- ✅ Navigation controls are present
- ✅ Slide counter displays
- ✅ Status bar is visible
- ✅ Render canvas exists
- ✅ Interaction layer is present
- ✅ Button clicks work
- ✅ CSS styles load
- ✅ Page structure is correct

### 4. `tests/example.spec.js`
Simple example tests:
- ✅ Page loads
- ✅ Title check
- ✅ Screenshot capture

### 5. `tests/setup.spec.js`
Setup and configuration verification:
- ✅ Dev server connection
- ✅ Browser context working
- ✅ Page navigation handling

## Configuration

### Playwright Config (`playwright.config.js`)

- **Browser**: Chromium (headless mode)
- **Viewport**: 1280x720
- **Base URL**: `http://localhost:3003`
- **Server**: Automatically starts Vite dev server
- **Timeouts**: 30s for actions/navigation
- **Screenshots**: On failure only
- **Videos**: Retained on failure

### Web Server Configuration

The config automatically:
1. Starts the Vite dev server (`npm run dev`)
2. Waits for server to be ready
3. Reuses existing server if already running
4. Times out after 2 minutes if server doesn't start

## Running Tests

### Headless Mode (Default)

```bash
npm run test:playwright:headless
```

### With UI

```bash
npm run test:playwright:ui
```

### Headed Mode (See Browser)

```bash
npm run test:playwright:headed
```

### Debug Mode

```bash
npm run test:playwright:debug
```

### View Report

```bash
npm run test:playwright:report
```

## Test Results

Test results are saved to:
- **HTML Report**: `playwright-report/index.html`
- **JSON Results**: `test-results/results.json`
- **Screenshots**: `test-results/*.png` (on failure)
- **Videos**: `test-results/*.webm` (on failure)

## Example Test Output

```
Running 5 tests using 2 workers

  ✓  Basic Playwright Tests › should run a basic test
  ✓  Basic Playwright Tests › should have page object
  ✓  Basic Playwright Tests › should navigate to a page
  ✓  Basic Playwright Tests › should take a screenshot
  ✓  Basic Playwright Tests › should evaluate JavaScript

  5 passed (2.8s)
```

## Troubleshooting

### Server Not Starting

If the dev server doesn't start:

1. Check if port 3003 is in use:
   ```bash
   lsof -i :3003
   ```

2. Start server manually:
   ```bash
   npm run dev
   ```

3. The config will reuse the existing server

### Tests Timing Out

Increase timeouts in `playwright.config.js`:

```javascript
use: {
  actionTimeout: 60000,      // 60 seconds
  navigationTimeout: 60000,   // 60 seconds
}
```

### Module Import Errors

If you see import errors:

1. Ensure dependencies are installed:
   ```bash
   npm install
   ```

2. Check that the dev server is running
3. Verify test URLs are correct

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
      - run: npm run test:playwright
      - uses: actions/upload-artifact@v3
        if: always()
        with:
          name: playwright-report
          path: playwright-report/
```

## Next Steps

1. ✅ Basic tests created
2. ✅ Init tests created
3. ✅ E2E tests created
4. ✅ Setup tests created
5. ✅ Configuration verified
6. ⏳ Add more E2E tests as features are added
7. ⏳ Integrate with CI/CD pipeline

## Related Documentation

- **PLAYWRIGHT_TESTING.md**: Complete Playwright testing guide
- **PLAYWRIGHT_HEADLESS_SETUP.md**: Headless Chrome setup guide
- **TEST_EXECUTION_GUIDE.md**: Manual testing guide
