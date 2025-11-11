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
      },
      external: ['meta-log-db'],
      output: {
        globals: {
          'meta-log-db': 'MetaLogDb'
        }
      }
    }
  },
  optimizeDeps: {
    exclude: ['meta-log-db']
  },
  server: {
    allowedHosts: ["universallifeprotocol.com"],
    port: parseInt(process.env.TEMPLATE_PROJECTOR_PORT || '3003', 10),
    open: true
  }
});
