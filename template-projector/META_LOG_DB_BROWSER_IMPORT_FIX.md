# MetaLogDbBrowser Import Resolution Fix

## Issue

Vite was unable to resolve the import `meta-log-db/browser` from `MetaLogBrowserAdapter.js`.

## Solution

### 1. Added Package.json Exports Map

Updated `meta-log-db/package.json` to include an `exports` field:

```json
"exports": {
  ".": {
    "node": "./dist/index.js",
    "browser": "./dist/browser/index.js",
    "default": "./dist/index.js"
  },
  "./browser": {
    "browser": "./dist/browser/index.js",
    "default": "./src/browser/index.ts"
  },
  "./package.json": "./package.json"
}
```

This allows Node.js and bundlers to resolve the browser entry point correctly.

### 2. Added Vite Alias

Updated `template-projector/vite.config.js` to add an alias for development:

```javascript
resolve: {
  alias: {
    'meta-log-db/browser': resolve(__dirname, 'node_modules/meta-log-db/src/browser/index.ts')
  }
}
```

This ensures Vite can resolve the import during development, pointing directly to the TypeScript source files.

### 3. Fixed Duplicate Export

Removed duplicate `BrowserConfig` export from `meta-log-db/src/browser/database.ts` that was causing TypeScript compilation errors.

## How It Works

1. **Development**: Vite uses the alias to resolve `meta-log-db/browser` to the TypeScript source files directly
2. **Production**: The `exports` map in package.json will resolve to the built bundle (`dist/browser/index.js`)
3. **TypeScript**: The source files use `.js` extensions in imports (TypeScript convention), which Vite handles correctly

## Testing

To verify the fix works:

1. Build the browser bundle:
   ```bash
   cd meta-log-db
   npm run build:browser
   ```

2. Start the dev server: `npm run dev`
3. Check browser console for import errors
4. Verify `MetaLogDbBrowser` initializes correctly

## Current Status (Updated 2025-01-12)

✅ **Browser build is working correctly**
- TypeScript compiles browser code to `dist/browser/` (fixed nested directory issue)
- Rollup bundles correctly to `dist/browser/index.js`
- Package.json exports resolve correctly
- Vite plugin handles both development (source) and production (built) paths
- Build verification script checks bundle output
- All TypeScript errors resolved (bip32.ts, shacl/validator.ts)

## Build Process

For production builds, ensure the browser bundle is built:

```bash
cd meta-log-db
npm run build:browser
```

This creates `dist/browser/index.js` which will be used in production.

## Related Files

- `meta-log-db/package.json` - Package exports configuration
- `template-projector/vite.config.js` - Vite alias configuration
- `meta-log-db/src/browser/database.ts` - Fixed duplicate export
- `template-projector/src/projector/MetaLogBrowserAdapter.js` - Uses `import('meta-log-db/browser')`

