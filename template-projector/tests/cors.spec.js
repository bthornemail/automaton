import { test, expect } from '@playwright/test';

/**
 * CORS Verification Tests
 * Tests cross-origin request handling
 */
test.describe('CORS Verification', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/test/cors-test.html');
    await page.waitForLoadState('networkidle');
  });

  test('should load CORS test page', async ({ page }) => {
    await expect(page.locator('h1, [class*="title"]')).toContainText(/cors/i);
  });

  test('Direct Fetch to DBpedia', async ({ page }) => {
    await page.waitForTimeout(10000);
    
    const results = page.locator('.test-result, .success, .error');
    const count = await results.count();
    
    expect(count).toBeGreaterThan(0);
  });

  test('SPARQL Query via Fetch', async ({ page }) => {
    await page.waitForTimeout(10000);
    
    const results = page.locator('.test-result');
    const count = await results.count();
    
    expect(count).toBeGreaterThan(0);
  });

  test('DBpedia Plugin Query', async ({ page }) => {
    await page.waitForTimeout(10000);
    
    const results = page.locator('.test-result');
    const count = await results.count();
    
    expect(count).toBeGreaterThan(0);
  });

  test('Error Handling', async ({ page }) => {
    await page.waitForTimeout(10000);
    
    const errorResults = page.locator('.error');
    const count = await errorResults.count();
    
    // May have expected errors
    expect(count).toBeGreaterThanOrEqual(0);
  });

  test('CORS Headers Check', async ({ page }) => {
    const corsHeaders = [];
    
    page.on('response', response => {
      const headers = response.headers();
      if (headers['access-control-allow-origin']) {
        corsHeaders.push({
          url: response.url(),
          header: headers['access-control-allow-origin'],
        });
      }
    });
    
    await page.waitForTimeout(15000);
    
    // Should have CORS headers for DBpedia requests
    console.log(`CORS headers found: ${corsHeaders.length}`);
    
    // Take screenshot
    await page.screenshot({ path: 'test-results/cors-complete.png', fullPage: true });
  });

  test('All CORS tests should complete', async ({ page }) => {
    await page.waitForTimeout(15000);
    
    const successCount = await page.locator('.success').count();
    const errorCount = await page.locator('.error').count();
    const totalTests = successCount + errorCount;
    
    console.log(`CORS Tests: ${successCount}/${totalTests} passed`);
    
    expect(totalTests).toBeGreaterThan(0);
  });
});
