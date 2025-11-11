/**
 * Browser polyfills for Node.js built-in modules
 * These are empty stubs since meta-log-db engines don't actually use fs/path/os
 */

// fs polyfill - empty stub
export const readFileSync = () => {
  throw new Error('fs.readFileSync is not available in browser. Use fetch() instead.');
};

export const writeFileSync = () => {
  throw new Error('fs.writeFileSync is not available in browser.');
};

export const existsSync = () => {
  throw new Error('fs.existsSync is not available in browser.');
};

export default {
  readFileSync,
  writeFileSync,
  existsSync
};

// path polyfill - minimal stub
export const join = (...args) => args.join('/');
export const dirname = (path) => path.split('/').slice(0, -1).join('/') || '.';
export const basename = (path) => path.split('/').pop() || '';

// os polyfill - empty stub
export const platform = 'browser';
export const arch = 'unknown';

