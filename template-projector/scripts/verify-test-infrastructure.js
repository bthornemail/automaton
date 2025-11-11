#!/usr/bin/env node

/**
 * Test Infrastructure Verification Script
 * 
 * Verifies that all test infrastructure is in place and ready for execution.
 */

import { existsSync, readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const projectRoot = join(__dirname, '..');

const checks = [];
let passed = 0;
let failed = 0;

function check(name, condition, message) {
  checks.push({ name, condition, message });
  if (condition) {
    console.log(`✅ ${name}`);
    passed++;
  } else {
    console.log(`❌ ${name}: ${message}`);
    failed++;
  }
}

console.log('🔍 Verifying Test Infrastructure...\n');

// Check build artifacts
check(
  'Build artifacts exist',
  existsSync(join(projectRoot, 'dist')),
  'Run `npm run build` first'
);

check(
  'dist/viewer.html exists',
  existsSync(join(projectRoot, 'dist/viewer.html')),
  'Build output missing'
);

// Check test files
const testFiles = [
  'test/federation-verification.html',
  'test/agent-protection-browser-test.html',
  'test/federation-test.html',
  'test/dbpedia-test.html',
  'test/cors-test.html',
  'test/error-recovery-test.html',
  'test/performance-test.html',
  'test/e2e-test.html'
];

testFiles.forEach(file => {
  check(
    `${file} exists`,
    existsSync(join(projectRoot, file)),
    `Test file missing: ${file}`
  );
});

// Check package.json scripts
try {
  const packageJson = JSON.parse(
    readFileSync(join(projectRoot, 'package.json'), 'utf-8')
  );
  
  const requiredScripts = [
    'test:federation-verify',
    'test:agent-protection',
    'test:federation',
    'test:dbpedia',
    'test:cors',
    'test:recovery',
    'test:performance',
    'preview'
  ];
  
  requiredScripts.forEach(script => {
    check(
      `npm script: ${script}`,
      packageJson.scripts && packageJson.scripts[script],
      `Missing script: ${script}`
    );
  });
} catch (error) {
  check('package.json readable', false, error.message);
}

// Check documentation
const docs = [
  'TEST_EXECUTION_GUIDE.md',
  'TEST_RESULTS.md',
  'PERFORMANCE_REPORT.md',
  'docs/FEDERATION_TESTING.md'
];

docs.forEach(doc => {
  check(
    `Documentation: ${doc}`,
    existsSync(join(projectRoot, doc)),
    `Missing documentation: ${doc}`
  );
});

// Summary
console.log('\n' + '='.repeat(50));
console.log(`Summary: ${passed} passed, ${failed} failed`);
console.log('='.repeat(50) + '\n');

if (failed === 0) {
  console.log('✅ All checks passed! Ready for testing.');
  console.log('\nNext steps:');
  console.log('1. Run: npm run preview');
  console.log('2. Open test URLs from TEST_EXECUTION_GUIDE.md');
  console.log('3. Document results in TEST_RESULTS.md');
  process.exit(0);
} else {
  console.log('❌ Some checks failed. Please fix issues before testing.');
  process.exit(1);
}
