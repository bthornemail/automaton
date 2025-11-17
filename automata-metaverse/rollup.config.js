import typescript from '@rollup/plugin-typescript';
import resolve from '@rollup/plugin-node-resolve';
import commonjs from '@rollup/plugin-commonjs';

export default {
  input: 'src/browser-index.ts',
  output: {
    file: 'dist/browser/index.js',
    format: 'es',
    sourcemap: true
  },
  plugins: [
    resolve({ browser: true }),
    commonjs(),
    typescript({ 
      tsconfig: './tsconfig.browser.json',
      declaration: false,
      declarationMap: false
    })
  ],
  external: ['meta-log-db']
};

