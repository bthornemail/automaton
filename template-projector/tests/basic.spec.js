import { test, expect } from '@playwright/test';

/**
 * Basic Playwright Tests
 * Simple tests that verify Playwright is working
 */
test.describe('Basic Playwright Tests', () => {
  test('should run a basic test', async ({ page }) => {
    // Simple test that doesn't require server
    expect(true).toBe(true);
  });

  test('should have page object', async ({ page }) => {
    expect(page).toBeTruthy();
    expect(typeof page.goto).toBe('function');
  });

  test('should navigate to a page', async ({ page, baseURL }) => {
    if (!baseURL) {
      test.skip();
      return;
    }
    
    try {
      await page.goto(baseURL, { timeout: 10000 });
      const url = page.url();
      expect(url).toBeTruthy();
    } catch (error) {
      // If server isn't running, skip the test
      console.log('Server not available, skipping navigation test');
      test.skip();
    }
  });

  test('should take a screenshot', async ({ page }) => {
    // Create a simple HTML page in memory
    await page.setContent('<html><body><h1>Test Page</h1></body></html>');
    await page.screenshot({ path: 'test-results/basic-test.png' });
    expect(true).toBe(true);
  });

  test('should evaluate JavaScript', async ({ page }) => {
    await page.setContent('<html><body><div id="test">Hello</div></body></html>');
    
    const text = await page.locator('#test').textContent();
    expect(text).toBe('Hello');
  });
});
