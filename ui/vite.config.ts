import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'
import { resolve } from 'path'
import { existsSync } from 'fs'

// Check if gpu.js is installed
const gpuJsPath = resolve(__dirname, 'node_modules/gpu.js');
const gpuJsInstalled = existsSync(gpuJsPath);

// Check meta-log-db browser bundle paths
// Try node_modules first, then fall back to workspace root
const metaLogDbBuiltPath = existsSync(resolve(__dirname, 'node_modules/meta-log-db/dist/browser/index.js'))
  ? resolve(__dirname, 'node_modules/meta-log-db/dist/browser/index.js')
  : resolve(__dirname, '../meta-log-db/dist/browser/index.js');
const metaLogDbSourcePath = existsSync(resolve(__dirname, 'node_modules/meta-log-db/src/browser/index.ts'))
  ? resolve(__dirname, 'node_modules/meta-log-db/src/browser/index.ts')
  : resolve(__dirname, '../meta-log-db/src/browser/index.ts');

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [react()],
  resolve: {
    alias: {
      '@': resolve(__dirname, 'src'),
      // Force single instance of CodeMirror packages
      '@codemirror/state': resolve(__dirname, 'node_modules/@codemirror/state'),
      '@codemirror/view': resolve(__dirname, 'node_modules/@codemirror/view'),
      // Use stub module for gpu.js if not installed
      ...(gpuJsInstalled ? {} : {
        'gpu.js': resolve(__dirname, 'src/utils/gpu-stub.ts'),
      }),
      // Handle meta-log-db/browser import
      'meta-log-db/browser': existsSync(metaLogDbBuiltPath) 
        ? metaLogDbBuiltPath 
        : (existsSync(metaLogDbSourcePath) 
          ? metaLogDbSourcePath 
          : (() => {
            throw new Error(
              `meta-log-db/browser not found. ` +
              `Built path: ${metaLogDbBuiltPath} (exists: ${existsSync(metaLogDbBuiltPath)}), ` +
              `Source path: ${metaLogDbSourcePath} (exists: ${existsSync(metaLogDbSourcePath)}). ` +
              `Run: cd ../meta-log-db && npm run build:browser`
            );
          })()),
    },
    dedupe: [
      '@codemirror/state',
      '@codemirror/view',
      '@codemirror/commands',
      '@codemirror/lang-javascript',
      '@codemirror/lang-markdown',
      '@codemirror/theme-one-dark',
      '@lezer/common',
      '@lezer/highlight'
    ],
  },
  server: {
    host: '0.0.0.0',
    port: 5173,
    strictPort: true,
    allowedHosts: ["universallifeprotocol.com","universallifeprotocol.net","universallifeprotocol.store","universallifeprotocol.online"],
    proxy: {
      '/api': {
        target: 'http://localhost:3000',
        changeOrigin: true,
        secure: false,
      },
      '/socket.io': {
        target: 'http://localhost:3001',
        changeOrigin: true,
        ws: true,
      },
    },
  },
  publicDir: 'public',
  build: {
    outDir: 'dist',
    sourcemap: true,
    rollupOptions: {
      external: ['gpu.js'], // Externalize optional dependency
      output: {
        manualChunks: {
          vendor: ['react', 'react-dom'],
          three: ['three', '@react-three/fiber', '@react-three/drei'],
          ui: ['framer-motion', 'lucide-react'],
          charts: ['recharts', 'd3'],
          codemirror: [
            '@codemirror/state',
            '@codemirror/view',
            '@codemirror/commands',
            '@codemirror/lang-javascript',
            '@codemirror/lang-markdown',
            '@codemirror/theme-one-dark',
            '@lezer/common',
            '@lezer/highlight'
          ],
        },
      },
    },
    chunkSizeWarningLimit: 1000,
    worker: {
      format: 'es',
      // Ensure Three.js is bundled in worker context
      plugins: () => [react()],
      rollupOptions: {
        output: {
          // Create separate chunk for worker dependencies
          manualChunks: (id) => {
            // Bundle Three.js with worker
            if (id.includes('three')) {
              return 'worker-three';
            }
            // Keep worker code separate
            if (id.includes('workers/provenance-canvas-worker')) {
              return 'provenance-worker';
            }
          },
        },
      },
    },
  },
  optimizeDeps: {
    include: [
      'react', 
      'react-dom', 
      'three',
      '@codemirror/state',
      '@codemirror/view',
      '@codemirror/commands',
      '@codemirror/lang-javascript',
      '@codemirror/lang-markdown',
      '@codemirror/theme-one-dark',
      'meta-log-db'
    ],
    exclude: ['gpu.js'], // Exclude optional dependency from optimization
  },
})