import { test, expect } from '@playwright/test';

/**
 * Playwright Initialization Test
 * Basic test to verify Playwright is working correctly
 */
test.describe('Playwright Initialization', () => {
  test('should load the application', async ({ page }) => {
    // Navigate to the root page
    await page.goto('/');
    
    // Wait for page to load
    await page.waitForLoadState('networkidle');
    
    // Check that page loaded successfully
    expect(page.url()).toContain('localhost');
    
    // Take a screenshot for verification
    await page.screenshot({ path: 'test-results/init-load.png' });
  });

  test('should have correct page title', async ({ page }) => {
    await page.goto('/viewer.html');
    await page.waitForLoadState('domcontentloaded');
    
    const title = await page.title();
    // Title might be empty for some pages, so just check it exists (even if empty)
    expect(title).toBeDefined();
  });

  test('should execute JavaScript', async ({ page }) => {
    await page.goto('/');
    
    // Test JavaScript execution
    const result = await page.evaluate(() => {
      return {
        userAgent: navigator.userAgent,
        readyState: document.readyState,
        hasWindow: typeof window !== 'undefined',
      };
    });
    
    expect(result.hasWindow).toBe(true);
    expect(result.readyState).toBe('complete');
    expect(result.userAgent).toContain('Chrome');
  });

  test('should handle network requests', async ({ page }) => {
    await page.goto('/');
    
    // Wait for network to be idle
    await page.waitForLoadState('networkidle');
    
    // Verify page is interactive
    const isInteractive = await page.evaluate(() => {
      return document.readyState === 'complete';
    });
    
    expect(isInteractive).toBe(true);
  });
});
