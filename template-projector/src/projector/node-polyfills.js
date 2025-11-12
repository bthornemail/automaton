/**
 * Browser polyfills for Node.js built-in modules
 * 
 * NOTE: MetaLogDbBrowser is browser-native and doesn't use Node.js modules.
 * These stubs are kept only for other dependencies that might need them.
 * They will throw errors if actually used, encouraging migration to browser APIs.
 */

// fs polyfill - empty stub (not used by MetaLogDbBrowser)
export const readFileSync = () => {
  throw new Error('fs.readFileSync is not available in browser. Use MetaLogDbBrowser.loadCanvas() or fetch() instead.');
};

export const writeFileSync = () => {
  throw new Error('fs.writeFileSync is not available in browser. Use IndexedDBStorage.set() instead.');
};

export const existsSync = () => {
  throw new Error('fs.existsSync is not available in browser. Use IndexedDBStorage.has() or fetch() instead.');
};

export default {
  readFileSync,
  writeFileSync,
  existsSync
};

// path polyfill - minimal stub (for other dependencies)
export const join = (...args) => args.join('/');
export const dirname = (path) => path.split('/').slice(0, -1).join('/') || '.';
export const basename = (path) => path.split('/').pop() || '';

// os polyfill - empty stub (for other dependencies)
export const platform = 'browser';
export const arch = 'unknown';

