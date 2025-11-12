# Provenance Canvas Worker

## Overview

The provenance canvas worker (`provenance-canvas-worker.ts`) is an offscreen canvas worker that renders 3D provenance chains using Three.js without blocking the main thread.

## Features

- **Offscreen Canvas Rendering**: Uses `OffscreenCanvas` API for rendering in a separate thread
- **Three.js Integration**: Full Three.js support for 3D rendering
- **Error Handling**: Comprehensive error handling for worker initialization and message passing
- **Performance**: Non-blocking rendering keeps main thread responsive

## Browser Support

The worker requires:
- **Worker API**: Modern browsers support Web Workers
- **OffscreenCanvas**: Supported in Chrome 69+, Firefox 105+, Safari 16.4+
- **ES Modules**: Required for `type: 'module'` worker

Use `ProvenanceCanvasWorkerService.isSupported()` to check browser support before initializing.

## Vite Configuration

The worker is configured in `vite.config.ts`:

```typescript
build: {
  worker: {
    format: 'es',
    rollupOptions: {
      output: {
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
}
```

This ensures:
- Three.js is bundled with the worker (not externalized)
- Worker code is in a separate chunk for optimal loading
- ES module format for modern browser support

## Error Handling

### Worker-Level Errors

The worker handles errors at multiple levels:

1. **Initialization Errors**: Caught during worker setup
2. **Message Errors**: Each message handler has try-catch
3. **Uncaught Errors**: Global error handler via `self.onerror`
4. **Render Loop Errors**: Fallback to setTimeout if requestAnimationFrame fails

### Service-Level Error Handling

The `ProvenanceCanvasWorkerService` provides:

- **Error Detection**: Checks for OffscreenCanvas support before initialization
- **Error Handlers**: Register error callbacks via `onMessage('error', handler)`
- **Graceful Fallback**: Continues without worker if initialization fails
- **Cleanup**: Properly disposes worker on errors

## Testing

### Unit Tests

Run worker bundling tests:

```bash
npm test -- src/workers/__tests__/worker-bundling.test.ts
```

### Production Build Test

Test worker bundling in production:

```bash
npm run test:worker-bundling
```

This script:
1. Checks Vite configuration
2. Verifies worker file and Three.js import
3. Tests production build
4. Checks bundle sizes

## Performance Considerations

### Bundle Size

- **Worker Bundle**: ~200-300KB (includes Three.js)
- **Main Bundle**: No Three.js duplication (separate chunk)
- **Lazy Loading**: Worker only loads when needed

### Rendering Performance

- **60 FPS**: Maintained in worker thread
- **No Main Thread Blocking**: All rendering in worker
- **Memory Efficient**: Proper cleanup on dispose

## Known Limitations

1. **devicePixelRatio**: Not available in worker context, defaults to 1
2. **requestAnimationFrame**: May not be available, falls back to setTimeout
3. **Window/DOM APIs**: Not available in worker context
4. **Browser Support**: OffscreenCanvas requires modern browsers

## Troubleshooting

### Worker Not Initializing

1. Check browser support: `ProvenanceCanvasWorkerService.isSupported()`
2. Check console for error messages
3. Verify Vite build includes worker bundle
4. Check network tab for worker file loading

### Three.js Not Available

1. Verify Three.js is in dependencies
2. Check Vite config includes Three.js in worker bundle
3. Verify import statement in worker file

### Rendering Issues

1. Check OffscreenCanvas support
2. Verify canvas dimensions are set correctly
3. Check for error messages in worker console
4. Verify provenance chain data is valid

## Future Improvements

- [ ] Main-thread fallback rendering
- [ ] Worker pool for multiple canvases
- [ ] Performance monitoring
- [ ] Bundle size optimization
- [ ] WebGL context sharing

