import { test, expect } from '@playwright/test';

/**
 * Setup and Configuration Test
 * Verifies Playwright is properly configured
 */
test.describe('Setup Tests', () => {
  test('should connect to dev server', async ({ page, baseURL }) => {
    // Verify baseURL is set
    expect(baseURL).toBeTruthy();
    expect(baseURL).toContain('localhost');
    
    // Navigate to root
    await page.goto('/');
    
    // Wait for page load
    await page.waitForLoadState('domcontentloaded');
    
    // Verify we're on the right page
    expect(page.url()).toContain(baseURL);
  });

  test('should have working browser context', async ({ page, browserName }) => {
    expect(browserName).toBe('chromium');
    
    await page.goto('/');
    
    // Test browser APIs
    const browserInfo = await page.evaluate(() => {
      return {
        userAgent: navigator.userAgent,
        platform: navigator.platform,
        language: navigator.language,
      };
    });
    
    expect(browserInfo.userAgent).toContain('Chrome');
    expect(browserInfo.platform).toBeTruthy();
  });

  test('should handle page navigation', async ({ page }) => {
    await page.goto('/');
    
    // Check page is loaded
    const readyState = await page.evaluate(() => document.readyState);
    expect(readyState).toBe('complete');
  });
});
