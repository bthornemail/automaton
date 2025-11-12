/**
 * Rollup configuration for browser bundle
 */

const { nodeResolve } = require('@rollup/plugin-node-resolve');
const commonjs = require('@rollup/plugin-commonjs');
const typescript = require('@rollup/plugin-typescript');

module.exports = {
  input: 'src/browser/index.ts',
  output: [
    {
      file: 'dist/browser/index.js',
      format: 'es',
      sourcemap: true
    },
    {
      file: 'dist/browser/index.umd.js',
      format: 'umd',
      name: 'MetaLogDbBrowser',
      sourcemap: true
    }
  ],
  plugins: [
    nodeResolve({
      browser: true,
      preferBuiltins: false
    }),
    commonjs(),
    typescript({
      tsconfig: 'tsconfig.browser.json',
      declaration: false,
      declarationMap: false
    })
  ],
  external: []
};

