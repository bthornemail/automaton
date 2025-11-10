import { defineConfig } from 'vite';

export default defineConfig({
  root: '.',
  publicDir: 'assets',
  build: {
    outDir: 'dist',
    assetsDir: 'assets',
    sourcemap: true,
    rollupOptions: {
      input: {
        main: './viewer.html'
      }
    }
  },
  server: {
allowedHosts: ["universallifeprotocol.com"],
    port: 3003,
    open: true
  }
});
