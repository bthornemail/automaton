import { defineConfig } from 'vite';
import { resolve } from 'path';
import { fileURLToPath } from 'url';
import { copyFileSync, existsSync } from 'fs';

const __dirname = resolve(fileURLToPath(new URL('.', import.meta.url)));

// Vite plugin to replace Node.js modules with browser-compatible stubs
const nodeModulesPlugin = () => ({
  name: 'node-modules-polyfill',
  resolveId(id) {
    // Replace Node.js built-in modules with empty stubs
    if (id === 'fs' || id === 'path' || id === 'os') {
      return resolve(__dirname, 'src/projector/node-polyfills.js');
    }
    return null;
  }
});

// Vite plugin to copy content-index.jsonl to dist
const copyContentIndexPlugin = () => ({
  name: 'copy-content-index',
  writeBundle() {
    const src = resolve(__dirname, 'content-index.jsonl');
    const dest = resolve(__dirname, 'dist', 'content-index.jsonl');
    try {
      if (existsSync(src)) {
        copyFileSync(src, dest);
        console.log('✓ Copied content-index.jsonl to dist/');
      } else {
        console.warn('⚠ content-index.jsonl not found, skipping copy');
      }
    } catch (error) {
      console.warn('⚠ Failed to copy content-index.jsonl:', error.message);
    }
  }
});

export default defineConfig({
  root: '.',
  publicDir: 'assets',
  plugins: [nodeModulesPlugin(), copyContentIndexPlugin()],
  build: {
    outDir: 'dist',
    assetsDir: 'assets',
    sourcemap: true,
    rollupOptions: {
      input: {
        main: './viewer.html'
      },
      // Don't externalize meta-log-db - bundle it for browser
      output: {
        format: 'es'
      }
    },
    commonjsOptions: {
      // Transform CommonJS to ES modules
      transformMixedEsModules: true
    },
    copyPublicDir: true
  },
  optimizeDeps: {
    include: ['meta-log-db'],
    // Handle CommonJS modules
    esbuildOptions: {
      format: 'esm'
    }
  },
  resolve: {
    // Let npm resolve meta-log-db from node_modules symlink
    // Vite will automatically transform CommonJS to ESM
  },
  define: {
    // Polyfill Node.js globals for browser
    'process.env.NODE_ENV': JSON.stringify(process.env.NODE_ENV || 'development'),
    'global': 'globalThis'
  },
  server: {
    allowedHosts: ["universallifeprotocol.com"],
    port: parseInt(process.env.TEMPLATE_PROJECTOR_PORT || '3003', 10),
    open: true
  }
});
