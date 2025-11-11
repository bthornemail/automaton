/** @type {import('jest').Config} */
export default {
  // Use ESM support
  preset: 'default',
  extensionsToTreatAsEsm: ['.js'],
  testEnvironment: 'node',
  
  // Exclude Playwright tests - they should be run via 'npx playwright test'
  testPathIgnorePatterns: [
    '/node_modules/',
    '/tests/', // Exclude all Playwright tests
    '/dist/',
    '/build/',
  ],
  
  // Only look for Jest tests in specific directories, excluding Playwright tests
  testMatch: [
    '**/__tests__/**/*.js',
    '**/?(*.)+(spec|test).js',
  ],
  
  // Module name mapping for ESM
  moduleNameMapper: {
    '^(\\.{1,2}/.*)\\.js$': '$1',
  },
  
  // Transform configuration
  transform: {},
  
  // Coverage configuration (if needed)
  collectCoverageFrom: [
    'src/**/*.js',
    '!src/**/*.spec.js',
    '!src/**/*.test.js',
  ],
};
