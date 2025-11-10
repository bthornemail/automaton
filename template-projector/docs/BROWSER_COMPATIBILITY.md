# Browser Compatibility Guide

**Version**: 1.0  
**Last Updated**: 2025-01-07

## Overview

This document describes browser compatibility requirements and testing status for the CanvasL Semantic Slides Projector.

## Required Features

The projector requires the following browser features:

### Core Features

- **ES6 Modules** (`import`/`export`)
- **Fetch API** (for loading files and SPARQL queries)
- **Promise/async-await**
- **Map/Set** (ES6 collections)
- **Template literals**
- **Arrow functions**

### Optional Features

- **Web Workers** (for background processing)
- **OffscreenCanvas** (for advanced rendering)
- **Service Workers** (for offline support)

## Browser Support Matrix

| Browser | Version | Status | Notes |
|---------|---------|--------|-------|
| Chrome | 90+ | ✅ Supported | Full support |
| Firefox | 88+ | ✅ Supported | Full support |
| Safari | 14+ | ✅ Supported | Full support |
| Edge | 90+ | ✅ Supported | Full support (Chromium-based) |
| Opera | 76+ | ✅ Supported | Full support (Chromium-based) |
| IE 11 | - | ❌ Not Supported | No ES6 modules support |

## Testing Status

### Automated Testing

- **Test Infrastructure**: ✅ Created (`test/e2e-test.html`)
- **Test Coverage**: 12 end-to-end tests
- **Test Types**:
  - Meta-Log bridge initialization
  - DBpedia plugin loading
  - DBpedia queries (abstract, thumbnail, related)
  - ProLog queries
  - DataLog queries
  - SPARQL local queries
  - Macro expansion
  - @include directive
  - Error handling
  - Slide loading

### Manual Testing

**Status**: ⚠️ **Pending**

To test browser compatibility:

1. **Start dev server**:
   ```bash
   npm run dev
   ```

2. **Open test page**:
   - Navigate to `http://localhost:5173/test/e2e-test.html`
   - Or run: `npm run test:e2e`

3. **Test in different browsers**:
   - Chrome: Open DevTools → Network tab → Check for errors
   - Firefox: Open DevTools → Network tab → Check for errors
   - Safari: Open Web Inspector → Network tab → Check for errors

## Known Issues

### Meta-Log DB Browser Compatibility

**Issue**: `meta-log-db` package uses Node.js `fs` module in some places.

**Solution**: `MetaLogBrowserAdapter` bypasses file system dependencies by using engines directly.

**Status**: ✅ **Resolved** - Adapter uses direct engine imports

### Dynamic Imports

**Issue**: Some browsers may not support dynamic `import()`.

**Solution**: Use static imports where possible, dynamic imports only for optional features.

**Status**: ✅ **Compatible** - All target browsers support dynamic imports

### CORS Issues

**Issue**: SPARQL endpoints may have CORS restrictions.

**Solution**: 
- Use proxy server for development
- Configure CORS headers on endpoints
- Use JSONP fallback (if available)

**Status**: ⚠️ **Needs Testing** - DBpedia endpoint should work, but needs verification

## Polyfills

No polyfills required for target browsers (Chrome 90+, Firefox 88+, Safari 14+).

If you need to support older browsers, consider:

- **Babel** for ES6 transpilation
- **core-js** for missing features
- **whatwg-fetch** for fetch API (not needed for target browsers)

## Performance Considerations

### Browser-Specific Optimizations

- **Chrome**: Uses V8 engine optimizations
- **Firefox**: Uses SpiderMonkey engine
- **Safari**: Uses JavaScriptCore engine

All engines handle ES6 features efficiently.

### Memory Management

- **Cache Management**: DBpedia plugin uses Map-based caching with TTL
- **Error History**: ErrorHandler limits history to 100 entries
- **Triple Store**: RDF triple store grows with usage (consider limits)

## Testing Checklist

- [ ] Chrome 90+ - All tests pass
- [ ] Firefox 88+ - All tests pass
- [ ] Safari 14+ - All tests pass
- [ ] Edge 90+ - All tests pass
- [ ] Mobile Chrome - Basic functionality works
- [ ] Mobile Safari - Basic functionality works
- [ ] CORS - DBpedia queries work
- [ ] Network errors - Error handling works
- [ ] Offline mode - Graceful degradation

## Reporting Issues

If you encounter browser compatibility issues:

1. **Check browser console** for errors
2. **Test in multiple browsers** to isolate issue
3. **Check network tab** for failed requests
4. **Report issue** with:
   - Browser name and version
   - Error message
   - Steps to reproduce
   - Console logs

## Resources

- **Test Page**: `test/e2e-test.html`
- **DBpedia Test**: `test/dbpedia-test.html`
- **Can I Use**: https://caniuse.com/
- **MDN Compatibility**: https://developer.mozilla.org/en-US/docs/Web/API
