import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'
import { resolve } from 'path'
import { existsSync } from 'fs'

// Check if gpu.js is installed
const gpuJsPath = resolve(__dirname, 'node_modules/gpu.js');
const gpuJsInstalled = existsSync(gpuJsPath);

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
      '@codemirror/theme-one-dark'
    ],
    exclude: ['gpu.js'], // Exclude optional dependency from optimization
  },
})