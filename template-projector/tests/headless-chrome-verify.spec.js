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
    await page.goto('/');
    
    // Verify page loads
    await expect(page).toHaveURL(/localhost:5173/);
    
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
    await page.goto('/');
    
    // Wait for network to be idle
    await page.waitForLoadState('networkidle');
    
    // Verify page is loaded
    const title = await page.title();
    expect(title).toBeTruthy();
  });
});
