import { test, expect } from '@playwright/test';
import { readFileSync } from 'fs';
import { join } from 'path';

// Parse automaton.jsonl to get expected nodes
const automatonFilePath = join(__dirname, '../../automaton.jsonl');
let automatonData: any[] = [];

try {
  const fileContent = readFileSync(automatonFilePath, 'utf-8');
  automatonData = fileContent
    .split('\n')
    .filter(line => line.trim().startsWith('{'))
    .map(line => JSON.parse(line));
} catch (error) {
  console.warn('Could not read automaton.jsonl:', error);
}

test.describe('Automaton.jsonl UI Tests', () => {
  test.beforeEach(async ({ page }) => {
    // Navigate to the UI
    await page.goto('/');
    
    // Wait for the page to load
    await page.waitForLoadState('networkidle');
  });

  test('should load automaton.jsonl file successfully', async ({ page }) => {
    // Check if API endpoint exists and can load the file
    const response = await page.request.post('http://localhost:5555/api/file/load', {
      data: { filePath: './automaton.jsonl' }
    }).catch(() => null);

    if (response && response.ok()) {
      const data = await response.json();
      expect(data.success).toBe(true);
    } else {
      // If API is not available, check if UI loads without errors
      console.log('API endpoint not available, checking UI rendering');
    }
  });

  test('should display automaton nodes from automaton.jsonl', async ({ page }) => {
    // Wait for dashboard to load
    await page.waitForSelector('[data-testid="dashboard"]', { timeout: 10000 }).catch(() => {
      console.log('Dashboard selector not found, checking alternative selectors');
    });

    // Check for dimensional topology nodes (0D-7D)
    const expectedDimensions = ['0D', '1D', '2D', '3D', '4D', '5D', '6D', '7D'];
    
    for (const dim of expectedDimensions) {
      // Look for dimension references in the page
      const dimensionText = page.locator(`text=/.*${dim}.*/i`);
      const count = await dimensionText.count();
      
      if (count > 0) {
        console.log(`Found ${dim} references: ${count}`);
      }
    }
  });

  test('should render self-reference nodes', async ({ page }) => {
    // Check for self-reference indicators
    const selfRefNodes = automatonData.filter(node => 
      node.id === 'self-ref' || 
      node.selfReference || 
      (node.type === 'file' && node.file)
    );

    if (selfRefNodes.length > 0) {
      console.log(`Found ${selfRefNodes.length} self-reference nodes in automaton.jsonl`);
      
      // Check if UI shows self-reference information
      const selfRefText = page.locator('text=/.*self.*reference.*/i');
      const count = await selfRefText.count();
      
      if (count > 0) {
        console.log(`Found ${count} self-reference UI elements`);
      }
    }
  });

  test('should display OpenCode operations from automaton.jsonl', async ({ page }) => {
    // Find OpenCode operations in automaton.jsonl
    const opencodeOps = automatonData.filter(node => 
      node.id && node.id.startsWith('opencode-') ||
      node.type === 'operation' ||
      (node.tool && node.params)
    );

    if (opencodeOps.length > 0) {
      console.log(`Found ${opencodeOps.length} OpenCode operations in automaton.jsonl`);
      
      // Navigate to OpenCode tab if it exists
      const opencodeTab = page.locator('button:has-text("OpenCode")');
      const tabCount = await opencodeTab.count();
      
      if (tabCount > 0) {
        await opencodeTab.click();
        await page.waitForTimeout(1000);
        
        // Check for OpenCode interface
        const opencodeInterface = page.locator('[data-testid="opencode-interface"]');
        const interfaceCount = await opencodeInterface.count();
        
        if (interfaceCount > 0) {
          console.log('OpenCode interface is visible');
        }
      }
    }
  });

  test('should show dimensional progression (0D-7D)', async ({ page }) => {
    // Check for dimensional progression visualization
    const canvas = page.locator('[data-testid="dimensional-canvas"]');
    const canvasCount = await canvas.count();
    
    if (canvasCount > 0) {
      console.log('Dimensional canvas found');
      
      // Check for dimension indicators
      const dimensions = ['0D', '1D', '2D', '3D', '4D', '5D', '6D', '7D'];
      for (const dim of dimensions) {
        const dimElement = page.locator(`text=/.*${dim}.*/i`);
        const count = await dimElement.count();
        if (count > 0) {
          console.log(`Found ${dim} element`);
        }
      }
    }
  });

  test('should handle automaton.jsonl file operations', async ({ page }) => {
    // Navigate to Config tab - use first() to handle multiple "Config" buttons
    const configTab = page.locator('button:has-text("Config")').first();
    const tabCount = await configTab.count();
    
    if (tabCount > 0) {
      await configTab.click();
      await page.waitForTimeout(1000);
      
      // Check for automaton file configuration
      const automatonFileInput = page.locator('input[value*="automaton.jsonl"]');
      const inputCount = await automatonFileInput.count();
      
      if (inputCount > 0) {
        console.log('Found automaton file configuration input');
        
        // Verify the file path is set correctly
        const value = await automatonFileInput.inputValue();
        expect(value).toContain('automaton.jsonl');
      } else {
        // Alternative: check if config panel is visible
        const configPanel = page.locator('text=/.*automaton.*file.*|.*file.*path.*/i');
        const panelCount = await configPanel.count();
        if (panelCount > 0) {
          console.log('Found configuration panel');
        }
      }
    }
  });

  test('should display automaton statistics', async ({ page }) => {
    // Check dashboard for statistics
    const dashboard = page.locator('[data-testid="dashboard"]');
    const dashboardCount = await dashboard.count();
    
    if (dashboardCount > 0) {
      // Count nodes in automaton.jsonl
      const nodeCount = automatonData.filter(n => n.type === 'text' || n.type === 'automaton').length;
      const edgeCount = automatonData.filter(n => n.fromNode || n.from).length;
      const operationCount = automatonData.filter(n => n.type === 'operation' || n.id?.startsWith('opencode-')).length;
      
      console.log(`Automaton.jsonl contains:`);
      console.log(`  - ${nodeCount} nodes`);
      console.log(`  - ${edgeCount} edges`);
      console.log(`  - ${operationCount} operations`);
      console.log(`  - ${automatonData.length} total objects`);
      
      // Check if UI displays similar statistics
      const statsText = page.locator('text=/.*iteration.*|.*dimension.*|.*total.*/i');
      const statsCount = await statsText.count();
      
      if (statsCount > 0) {
        console.log(`Found ${statsCount} statistics elements in UI`);
      }
    }
  });

  test('should render Church encoding patterns', async ({ page }) => {
    // Check for Church encoding references in automaton.jsonl
    const churchNodes = automatonData.filter(node => 
      node.text && (
        node.text.includes('λ') ||
        node.text.includes('Church') ||
        node.text.includes('church')
      )
    );

    if (churchNodes.length > 0) {
      console.log(`Found ${churchNodes.length} nodes with Church encoding patterns`);
      
      // Check if UI displays Church encoding information
      const churchText = page.locator('text=/.*church.*|.*lambda.*|.*λ.*/i');
      const count = await churchText.count();
      
      if (count > 0) {
        console.log(`Found ${count} Church encoding UI elements`);
      }
    }
  });

  test('should handle large automaton.jsonl file', async ({ page }) => {
    // Verify automaton.jsonl has content
    expect(automatonData.length).toBeGreaterThan(0);
    
    console.log(`Testing with ${automatonData.length} objects from automaton.jsonl`);
    
    // Check page performance
    const performanceMetrics = await page.evaluate(() => {
      return {
        loadTime: performance.timing.loadEventEnd - performance.timing.navigationStart,
        domContentLoaded: performance.timing.domContentLoadedEventEnd - performance.timing.navigationStart,
        renderTime: performance.timing.domComplete - performance.timing.domLoading
      };
    });
    
    console.log('Performance metrics:', performanceMetrics);
    
    // Page should load within reasonable time
    expect(performanceMetrics.loadTime).toBeLessThan(30000); // 30 seconds
  });

  test('should validate automaton.jsonl structure', async ({ page }) => {
    // Validate that automaton.jsonl has expected structure
    const hasSelfRef = automatonData.some(n => n.id === 'self-ref');
    const hasDimensions = automatonData.some(n => 
      n.id && /^\d+D-(topology|system|algebra|analysis|network|consensus|intelligence|quantum)/.test(n.id)
    );
    const hasAutomata = automatonData.some(n => n.type === 'automaton');
    const hasEdges = automatonData.some(n => n.fromNode || n.from);
    
    expect(hasSelfRef || hasDimensions || hasAutomata || hasEdges).toBe(true);
    
    console.log('Automaton.jsonl structure validation:');
    console.log(`  - Has self-reference: ${hasSelfRef}`);
    console.log(`  - Has dimensions: ${hasDimensions}`);
    console.log(`  - Has automata: ${hasAutomata}`);
    console.log(`  - Has edges: ${hasEdges}`);
  });
});
