# Migration Guide: Node.js to Browser

Guide for migrating from Node.js `MetaLogDb` to browser `MetaLogDbBrowser`.

## Overview

The browser version maintains API compatibility with the Node.js version, but requires some changes:

1. **Async Initialization**: Browser version requires `init()` call
2. **File Loading**: Uses URLs instead of file paths
3. **Storage**: Uses IndexedDB instead of file system
4. **Encryption**: Built-in encryption support

## Basic Migration

### Node.js Version

```typescript
import { MetaLogDb } from 'meta-log-db';

const db = new MetaLogDb({
  r5rsEnginePath: './r5rs-canvas-engine.scm',
  enableProlog: true,
  enableDatalog: true
});

await db.loadCanvas('automaton-kernel.jsonl');
const facts = db.extractFacts();
```

### Browser Version

```typescript
import { MetaLogDbBrowser } from 'meta-log-db/browser';

const db = new MetaLogDbBrowser({
  r5rsEngineURL: '/r5rs-canvas-engine.scm',
  enableProlog: true,
  enableDatalog: true
});

// Initialize (required)
await db.init();

// Load from URL instead of file path
await db.loadCanvas('automaton-kernel.jsonl', '/jsonl/automaton-kernel.jsonl');
const facts = db.extractFacts();
```

## Key Differences

### 1. Import Path

**Node.js:**
```typescript
import { MetaLogDb } from 'meta-log-db';
```

**Browser:**
```typescript
import { MetaLogDbBrowser } from 'meta-log-db/browser';
```

### 2. Initialization

**Node.js:**
```typescript
const db = new MetaLogDb(config);
// Ready to use immediately
```

**Browser:**
```typescript
const db = new MetaLogDbBrowser(config);
await db.init(); // Must initialize first
```

### 3. File Loading

**Node.js:**
```typescript
await db.loadCanvas('./automaton-kernel.jsonl');
```

**Browser:**
```typescript
await db.loadCanvas('automaton-kernel.jsonl', '/jsonl/automaton-kernel.jsonl');
// Or use relative URL
await db.loadCanvas('automaton-kernel.jsonl', './jsonl/automaton-kernel.jsonl');
```

### 4. R5RS Engine Path

**Node.js:**
```typescript
const db = new MetaLogDb({
  r5rsEnginePath: './r5rs-canvas-engine.scm'
});
```

**Browser:**
```typescript
const db = new MetaLogDbBrowser({
  r5rsEngineURL: '/r5rs-canvas-engine.scm'
  // Or use r5rsEnginePath for IndexedDB lookup
});
```

### 5. Storage

**Node.js:**
- Files stored on file system
- No caching (reads from disk each time)

**Browser:**
- Files cached in IndexedDB
- Automatic caching with `cacheStrategy` option

## Configuration Mapping

| Node.js Config | Browser Config | Notes |
|---------------|----------------|-------|
| `r5rsEnginePath` | `r5rsEngineURL` or `r5rsEnginePath` | Use URL for browser |
| `enableProlog` | `enableProlog` | Same |
| `enableDatalog` | `enableDatalog` | Same |
| `enableRdf` | `enableRdf` | Same |
| `enableShacl` | `enableShacl` | Same |
| N/A | `enableEncryption` | Browser-only |
| N/A | `mnemonic` | Browser-only |
| N/A | `indexedDBName` | Browser-only |
| N/A | `cacheStrategy` | Browser-only |

## Complete Migration Example

### Before (Node.js)

```typescript
import { MetaLogDb } from 'meta-log-db';
import * as fs from 'fs';

const db = new MetaLogDb({
  r5rsEnginePath: './r5rs-canvas-engine.scm',
  enableProlog: true,
  enableDatalog: true,
  enableRdf: true,
  enableShacl: true
});

// Load R5RS engine
await db.loadR5RSEngine('./r5rs-canvas-engine.scm');

// Load canvas
await db.loadCanvas('./automaton-kernel.jsonl');

// Extract facts
const facts = db.extractFacts();

// Execute queries
const prologResults = await db.prologQuery('(node ?Id ?Type)');
const sparqlResults = await db.sparqlQuery('SELECT ?id WHERE { ?id rdf:type ?type }');

// Validate
const validation = await db.validateShacl();
```

### After (Browser)

```typescript
import { MetaLogDbBrowser } from 'meta-log-db/browser';

const db = new MetaLogDbBrowser({
  r5rsEngineURL: '/r5rs-canvas-engine.scm',
  enableProlog: true,
  enableDatalog: true,
  enableRdf: true,
  enableShacl: true,
  cacheStrategy: 'both' // Use IndexedDB caching
});

// Initialize (required)
await db.init();

// Load R5RS engine (from URL)
await db.loadR5RSEngine('/r5rs-canvas-engine.scm');

// Load canvas (from URL)
await db.loadCanvas('automaton-kernel.jsonl', '/jsonl/automaton-kernel.jsonl');

// Extract facts (same API)
const facts = db.extractFacts();

// Execute queries (same API)
const prologResults = await db.prologQuery('(node ?Id ?Type)');
const sparqlResults = await db.sparqlQuery('SELECT ?id WHERE { ?id rdf:type ?type }');

// Validate (same API)
const validation = await db.validateShacl();
```

## Conditional Import Pattern

For code that works in both Node.js and browser:

```typescript
let MetaLogDb: any;
let isBrowser = false;

if (typeof window !== 'undefined') {
  // Browser environment
  const browser = await import('meta-log-db/browser');
  MetaLogDb = browser.MetaLogDbBrowser;
  isBrowser = true;
} else {
  // Node.js environment
  const node = await import('meta-log-db');
  MetaLogDb = node.MetaLogDb;
}

const db = new MetaLogDb(config);

if (isBrowser) {
  await db.init();
}

// Rest of code works the same
```

## Feature Parity

### Supported Features

- ✅ ProLog queries
- ✅ DataLog queries
- ✅ RDF/SPARQL queries
- ✅ SHACL validation
- ✅ R5RS function execution
- ✅ JSONL/CanvasL parsing
- ✅ Fact extraction

### Browser-Only Features

- ✅ IndexedDB persistence
- ✅ File caching
- ✅ BIP32/39/44 cryptography
- ✅ Storage encryption
- ✅ URL-based file loading

### Node.js-Only Features

- ❌ Direct file system access
- ❌ Synchronous file operations
- ❌ File system-based storage

## Common Issues

### Issue 1: Forgetting to Initialize

**Error:**
```
Database not initialized
```

**Solution:**
```typescript
await db.init(); // Call before use
```

### Issue 2: File Not Found

**Error:**
```
Failed to fetch /jsonl/file.jsonl: 404
```

**Solution:**
- Check file exists in public directory
- Use correct URL path
- Check CORS settings if loading from different origin

### Issue 3: IndexedDB Quota Exceeded

**Error:**
```
QuotaExceededError
```

**Solution:**
```typescript
// Clear cache
await db.clearCache();

// Or clear specific files
await db.getFileIO().clearFileCache('file.jsonl');
```

## Testing Migration

1. **Test File Loading**: Verify files load from URLs
2. **Test Caching**: Verify IndexedDB caching works
3. **Test Queries**: Verify all queries work the same
4. **Test Encryption**: If using encryption, verify encrypt/decrypt works
5. **Test Performance**: Compare performance with Node.js version

## Rollback Plan

If migration issues occur:

1. Keep Node.js version as fallback
2. Use conditional imports
3. Gradually migrate features
4. Test thoroughly before full migration

