#!/usr/bin/env node

/**
 * Playwright Setup Verification Script
 * Verifies that Playwright is properly installed and configured for headless Chrome testing
 */

import { execSync } from 'child_process';
import { existsSync, readFileSync, readdirSync, mkdirSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const projectRoot = join(__dirname, '..');

console.log('🔍 Verifying Playwright Setup for Headless Chrome...\n');

let allChecksPassed = true;

// Check 1: Playwright package installed
console.log('1. Checking Playwright package installation...');
try {
  const packageJson = JSON.parse(
    readFileSync(join(projectRoot, 'package.json'), 'utf-8')
  );
  if (packageJson.devDependencies && packageJson.devDependencies['@playwright/test']) {
    console.log('   ✅ @playwright/test is installed');
  } else {
    console.log('   ❌ @playwright/test not found in package.json');
    allChecksPassed = false;
  }
} catch (error) {
  console.log('   ❌ Error reading package.json:', error.message);
  allChecksPassed = false;
}

// Check 2: Playwright config exists
console.log('\n2. Checking Playwright configuration...');
const configPath = join(projectRoot, 'playwright.config.js');
if (existsSync(configPath)) {
  console.log('   ✅ playwright.config.js exists');
  
  // Check if config has headless setting
  const configContent = readFileSync(configPath, 'utf-8');
  if (configContent.includes('headless: true')) {
    console.log('   ✅ Headless mode is configured');
  } else {
    console.log('   ⚠️  Headless mode not explicitly set (will default to headless)');
  }
} else {
  console.log('   ❌ playwright.config.js not found');
  allChecksPassed = false;
}

// Check 3: Test directory exists
console.log('\n3. Checking test directory...');
const testDir = join(projectRoot, 'tests');
if (existsSync(testDir)) {
  console.log('   ✅ tests/ directory exists');
  
  // Count test files
  const testFiles = readdirSync(testDir)
    .filter(f => f.endsWith('.spec.js') || f.endsWith('.spec.ts'));
  console.log(`   ✅ Found ${testFiles.length} test file(s)`);
} else {
  console.log('   ⚠️  tests/ directory not found');
}

// Check 4: Chromium browser installed
console.log('\n4. Checking Chromium browser installation...');
try {
  const result = execSync('npx playwright install chromium --dry-run', {
    cwd: projectRoot,
    encoding: 'utf-8',
    stdio: 'pipe'
  });
  
  if (result.includes('chromium version')) {
    // Extract version
    const versionMatch = result.match(/chromium version (\S+)/);
    if (versionMatch) {
      console.log(`   ✅ Chromium ${versionMatch[1]} is installed`);
    } else {
      console.log('   ✅ Chromium is installed');
    }
  } else {
    console.log('   ⚠️  Could not verify Chromium installation');
  }
} catch (error) {
  console.log('   ⚠️  Could not check Chromium installation:', error.message);
}

// Check 5: Test results directory
console.log('\n5. Checking test results directory...');
const testResultsDir = join(projectRoot, 'test-results');
if (!existsSync(testResultsDir)) {
  try {
    mkdirSync(testResultsDir, { recursive: true });
    console.log('   ✅ Created test-results/ directory');
  } catch (error) {
    console.log('   ⚠️  Could not create test-results/ directory');
  }
} else {
  console.log('   ✅ test-results/ directory exists');
}

// Summary
console.log('\n' + '='.repeat(50));
if (allChecksPassed) {
  console.log('✅ All checks passed! Playwright is ready for headless Chrome testing.');
  console.log('\nNext steps:');
  console.log('  1. Run tests: npm run test:playwright:headless');
  console.log('  2. View report: npm run test:playwright:report');
  console.log('  3. Debug tests: npm run test:playwright:debug');
  process.exit(0);
} else {
  console.log('❌ Some checks failed. Please review the output above.');
  console.log('\nTo fix:');
  console.log('  1. Install dependencies: npm install');
  console.log('  2. Install browsers: npm run test:playwright:install');
  process.exit(1);
}
