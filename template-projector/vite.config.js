import { defineConfig } from 'vite';
import { resolve } from 'path';
import { fileURLToPath } from 'url';

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

export default defineConfig({
  root: '.',
  publicDir: 'assets',
  plugins: [nodeModulesPlugin()],
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
    }
  },
  optimizeDeps: {
    include: ['meta-log-db'],
    // Handle CommonJS modules
    esbuildOptions: {
      format: 'esm'
    }
  },
  resolve: {
    // Remove stub alias - use real meta-log-db package
    // The package is now linked via npm link
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
