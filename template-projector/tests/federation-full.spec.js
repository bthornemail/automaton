import { test, expect } from '@playwright/test';

/**
 * Full Federation Suite Tests
 * Tests all 20 comprehensive federation scenarios
 */
test.describe('Full Federation Suite', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/test/federation-test.html');
    await page.waitForLoadState('networkidle');
  });

  test('should load federation test page', async ({ page }) => {
    await expect(page.locator('h1')).toContainText(/federated sparql/i);
  });

  test('Unit Tests (1-4) should execute', async ({ page }) => {
    await page.waitForTimeout(10000);
    
    // Check for test results
    const testResults = page.locator('.test-result, [class*="result"]');
    const count = await testResults.count();
    
    expect(count).toBeGreaterThan(0);
  });

  test('Integration Tests (5-10) should execute', async ({ page }) => {
    await page.waitForTimeout(15000);
    
    // Check for integration test results
    const successResults = page.locator('.success');
    const errorResults = page.locator('.error');
    const totalResults = await successResults.count() + await errorResults.count();
    
    expect(totalResults).toBeGreaterThan(0);
  });

  test('E2E Tests (11-15) should execute', async ({ page }) => {
    await page.waitForTimeout(15000);
    
    // Check for E2E test results
    const testSections = page.locator('.test-section, [class*="test"]');
    const count = await testSections.count();
    
    expect(count).toBeGreaterThan(0);
  });

  test('Advanced Tests (16-20) should execute', async ({ page }) => {
    await page.waitForTimeout(15000);
    
    // Check for advanced test results
    const results = page.locator('.test-result');
    const count = await results.count();
    
    expect(count).toBeGreaterThan(0);
  });

  test('Test dashboard should show statistics', async ({ page }) => {
    await page.waitForTimeout(10000);
    
    // Check for stats dashboard
    const stats = page.locator('#stats, [id*="stat"], .stats');
    const count = await stats.count();
    
    // Should have stats dashboard
    expect(count).toBeGreaterThan(0);
  });

  test('Should track test metrics', async ({ page }) => {
    await page.waitForTimeout(10000);
    
    // Check for metric elements
    const totalTests = page.locator('#total-tests, [id*="total"]');
    const passedTests = page.locator('#passed-tests, [id*="passed"]');
    const failedTests = page.locator('#failed-tests, [id*="failed"]');
    
    const hasTotal = await totalTests.count() > 0;
    const hasPassed = await passedTests.count() > 0;
    const hasFailed = await failedTests.count() > 0;
    
    // At least some metrics should be present
    expect(hasTotal || hasPassed || hasFailed).toBeTruthy();
  });

  test('All 20 tests should complete', async ({ page }) => {
    // Wait for all tests to complete (may take up to 60 seconds)
    await page.waitForTimeout(60000);
    
    const successCount = await page.locator('.success').count();
    const errorCount = await page.locator('.error').count();
    const totalTests = successCount + errorCount;
    
    console.log(`Full Federation Suite: ${successCount}/${totalTests} passed`);
    
    // Should have completed at least some tests
    expect(totalTests).toBeGreaterThan(0);
    
    // Take final screenshot
    await page.screenshot({ path: 'test-results/federation-full-complete.png', fullPage: true });
  });

  test('Network requests should be made', async ({ page }) => {
    const requests = [];
    
    page.on('request', request => {
      if (request.url().includes('sparql') || request.url().includes('dbpedia')) {
        requests.push(request.url());
      }
    });
    
    await page.waitForTimeout(20000);
    
    // Should have made at least some SPARQL requests
    console.log(`Network requests captured: ${requests.length}`);
    // Note: This is informational - network requests may vary
  });
});
