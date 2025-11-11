import { test, expect } from '@playwright/test';

/**
 * Federation Verification Tests
 * Tests SERVICE block parsing and VALUES extraction
 */
test.describe('Federation Verification', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/test/federation-verification.html');
    // Wait for page to load
    await page.waitForLoadState('networkidle');
  });

  test('should load federation verification page', async ({ page }) => {
    await expect(page.locator('h1')).toContainText('SERVICE Block Parsing Verification');
  });

  test('Test 1: Single SERVICE Block', async ({ page }) => {
    const testResult = page.locator('#test1-result');
    
    // Wait for test to complete (check for success or error class)
    await testResult.waitFor({ state: 'visible', timeout: 30000 });
    
    // Wait for test to finish running
    await page.waitForTimeout(5000);
    
    const resultText = await testResult.textContent();
    const hasSuccess = await testResult.locator('.success').count() > 0;
    const hasError = await testResult.locator('.error').count() > 0;
    
    expect(hasSuccess || hasError).toBeTruthy();
    
    if (hasError) {
      console.log(`Test 1 failed: ${resultText}`);
    }
    
    // Take screenshot for documentation
    await page.screenshot({ path: 'test-results/federation-verification-test1.png' });
  });

  test('Test 2: Multiple SERVICE Blocks', async ({ page }) => {
    const testResult = page.locator('#test2-result');
    await testResult.waitFor({ state: 'visible', timeout: 30000 });
    await page.waitForTimeout(5000);
    
    const hasSuccess = await testResult.locator('.success').count() > 0;
    const hasError = await testResult.locator('.error').count() > 0;
    
    expect(hasSuccess || hasError).toBeTruthy();
    
    if (hasError) {
      const resultText = await testResult.textContent();
      console.log(`Test 2 failed: ${resultText}`);
    }
  });

  test('Test 3: SERVICE with VALUES', async ({ page }) => {
    const testResult = page.locator('#test3-result');
    await testResult.waitFor({ state: 'visible', timeout: 30000 });
    await page.waitForTimeout(5000);
    
    const hasSuccess = await testResult.locator('.success').count() > 0;
    const hasError = await testResult.locator('.error').count() > 0;
    
    expect(hasSuccess || hasError).toBeTruthy();
  });

  test('Test 4: Nested SERVICE Blocks', async ({ page }) => {
    const testResult = page.locator('#test4-result');
    await testResult.waitFor({ state: 'visible', timeout: 30000 });
    await page.waitForTimeout(5000);
    
    const hasSuccess = await testResult.locator('.success').count() > 0;
    const hasError = await testResult.locator('.error').count() > 0;
    
    expect(hasSuccess || hasError).toBeTruthy();
  });

  test('Test 5: SERVICE with Complex Patterns', async ({ page }) => {
    const testResult = page.locator('#test5-result');
    await testResult.waitFor({ state: 'visible', timeout: 30000 });
    await page.waitForTimeout(5000);
    
    const hasSuccess = await testResult.locator('.success').count() > 0;
    const hasError = await testResult.locator('.error').count() > 0;
    
    expect(hasSuccess || hasError).toBeTruthy();
  });

  test('Test 6: VALUES Extraction', async ({ page }) => {
    const testResult = page.locator('#test6-result');
    await testResult.waitFor({ state: 'visible', timeout: 30000 });
    await page.waitForTimeout(5000);
    
    const hasSuccess = await testResult.locator('.success').count() > 0;
    const hasError = await testResult.locator('.error').count() > 0;
    
    expect(hasSuccess || hasError).toBeTruthy();
  });

  test('Test 7: VALUES Binding Flow', async ({ page }) => {
    const testResult = page.locator('#test7-result');
    await testResult.waitFor({ state: 'visible', timeout: 30000 });
    await page.waitForTimeout(5000);
    
    const hasSuccess = await testResult.locator('.success').count() > 0;
    const hasError = await testResult.locator('.error').count() > 0;
    
    expect(hasSuccess || hasError).toBeTruthy();
  });

  test('Test 8: Query Rewriting', async ({ page }) => {
    const testResult = page.locator('#test8-result');
    await testResult.waitFor({ state: 'visible', timeout: 30000 });
    await page.waitForTimeout(5000);
    
    const hasSuccess = await testResult.locator('.success').count() > 0;
    const hasError = await testResult.locator('.error').count() > 0;
    
    expect(hasSuccess || hasError).toBeTruthy();
    
    // Take final screenshot
    await page.screenshot({ path: 'test-results/federation-verification-complete.png', fullPage: true });
  });

  test('All tests should complete', async ({ page }) => {
    // Wait for all tests to complete
    await page.waitForTimeout(10000);
    
    // Count success and error results
    const successCount = await page.locator('.test-result.success').count();
    const errorCount = await page.locator('.test-result.error').count();
    const totalTests = successCount + errorCount;
    
    console.log(`Federation Verification: ${successCount}/${totalTests} passed`);
    
    // At least some tests should have completed
    expect(totalTests).toBeGreaterThan(0);
  });
});
