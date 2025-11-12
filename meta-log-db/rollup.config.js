/**
 * Rollup configuration for browser bundle
 * 
 * Bundles browser-specific code into a single ES module file.
 * The TypeScript plugin compiles on-the-fly, so the separate tsc step
 * in build:browser is mainly for type checking and declarations.
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
      sourcemap: true,
      exports: 'named'
    },
    {
      file: 'dist/browser/index.umd.js',
      format: 'umd',
      name: 'MetaLogDbBrowser',
      sourcemap: true,
      exports: 'named'
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
      declarationMap: false,
      sourceMap: true
    })
  ],
  external: []
};

