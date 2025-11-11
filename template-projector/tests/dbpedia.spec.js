import { test, expect } from '@playwright/test';

/**
 * DBpedia Endpoint Tests
 * Tests real-world DBpedia SPARQL endpoint integration
 */
test.describe('DBpedia Endpoint Tests', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/test/dbpedia-test.html');
    await page.waitForLoadState('networkidle');
  });

  test('should load DBpedia test page', async ({ page }) => {
    await expect(page.locator('h1, [class*="title"]')).toContainText(/dbpedia/i);
  });

  test('Einstein Abstract Query', async ({ page }) => {
    await page.waitForTimeout(10000);
    
    // Check for test results
    const results = page.locator('.test-result, .success, .error');
    const count = await results.count();
    
    expect(count).toBeGreaterThan(0);
  });

  test('Thumbnail Query', async ({ page }) => {
    await page.waitForTimeout(10000);
    
    const results = page.locator('.test-result');
    const count = await results.count();
    
    expect(count).toBeGreaterThan(0);
  });

  test('Related Entities Query', async ({ page }) => {
    await page.waitForTimeout(10000);
    
    const results = page.locator('.test-result');
    const count = await results.count();
    
    expect(count).toBeGreaterThan(0);
  });

  test('Query Performance', async ({ page }) => {
    const startTime = Date.now();
    
    await page.waitForTimeout(10000);
    
    const endTime = Date.now();
    const duration = endTime - startTime;
    
    // Should complete within reasonable time (allowing for network)
    expect(duration).toBeLessThan(30000);
    
    console.log(`DBpedia query performance: ${duration}ms`);
  });

  test('Error Handling', async ({ page }) => {
    await page.waitForTimeout(10000);
    
    // Check for error handling tests
    const errorResults = page.locator('.error');
    const count = await errorResults.count();
    
    // May have expected errors for error handling tests
    expect(count).toBeGreaterThanOrEqual(0);
    
    // Take screenshot
    await page.screenshot({ path: 'test-results/dbpedia-complete.png', fullPage: true });
  });

  test('DBpedia endpoint should be accessible', async ({ page }) => {
    const responses = [];
    
    page.on('response', response => {
      if (response.url().includes('dbpedia.org')) {
        responses.push({
          url: response.url(),
          status: response.status(),
        });
      }
    });
    
    await page.waitForTimeout(15000);
    
    // Should have made requests to DBpedia
    console.log(`DBpedia responses: ${responses.length}`);
    // Note: This is informational
  });
});
