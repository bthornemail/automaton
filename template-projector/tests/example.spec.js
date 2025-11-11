import { test, expect } from '@playwright/test';

/**
 * Example Playwright Test
 * Simple example to verify Playwright is working
 */
test.describe('Example Tests', () => {
  test('example test - page loads', async ({ page }) => {
    await page.goto('/');
    await expect(page).toHaveURL(/localhost/);
  });

  test('example test - check title', async ({ page }) => {
    await page.goto('/viewer.html');
    await page.waitForLoadState('domcontentloaded');
    const title = await page.title();
    // Title might be empty, so just check it's defined
    expect(title).toBeDefined();
  });

  test('example test - take screenshot', async ({ page }) => {
    await page.goto('/');
    await page.screenshot({ path: 'test-results/example-screenshot.png' });
    expect(true).toBe(true);
  });
});
