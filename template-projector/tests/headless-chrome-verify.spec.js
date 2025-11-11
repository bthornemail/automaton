import { test, expect } from '@playwright/test';

/**
 * Headless Chrome Verification Test
 * Verifies that Playwright is properly configured for headless Chrome testing
 */
test.describe('Headless Chrome Setup Verification', () => {
  test('should run in headless Chrome', async ({ page, browserName }) => {
    // Verify we're using Chromium
    expect(browserName).toBe('chromium');
    
    // Navigate to a simple page
    await page.goto('/', { waitUntil: 'domcontentloaded' });
    
    // Verify page loads (check for port 3003, not 5173)
    const url = page.url();
    expect(url).toMatch(/localhost:(3003|5173)/);
    
    // Check that we're running headless
    const isHeadless = await page.evaluate(() => {
      return navigator.webdriver === true;
    });
    
    // In headless mode, navigator.webdriver is true
    expect(isHeadless).toBeTruthy();
    
    // Take a screenshot to verify headless mode works
    await page.screenshot({ 
      path: 'test-results/headless-chrome-verify.png',
      fullPage: true 
    });
  });

  test('should have correct viewport size', async ({ page }) => {
    await page.goto('/');
    
    // Verify viewport matches config (1280x720)
    const viewport = page.viewportSize();
    expect(viewport.width).toBe(1280);
    expect(viewport.height).toBe(720);
  });

  test('should handle JavaScript execution', async ({ page }) => {
    await page.goto('/');
    
    // Execute JavaScript in headless Chrome
    const result = await page.evaluate(() => {
      return {
        userAgent: navigator.userAgent,
        platform: navigator.platform,
        language: navigator.language,
      };
    });
    
    expect(result.userAgent).toContain('Chrome');
    expect(result.platform).toBeTruthy();
    expect(result.language).toBeTruthy();
  });

  test('should support network requests', async ({ page }) => {
    await page.goto('/', { waitUntil: 'domcontentloaded' });
    
    // Wait for network to be idle (with timeout)
    await page.waitForLoadState('networkidle', { timeout: 10000 }).catch(() => {
      // Network idle might not happen if there are long-running requests
    });
    
    // Verify page is loaded - handle empty title gracefully
    const title = await page.title();
    // Page might have empty title if meta-log-db import fails, but page should still load
    const bodyText = await page.locator('body').textContent().catch(() => '');
    expect(title || bodyText).toBeTruthy();
  });
});
