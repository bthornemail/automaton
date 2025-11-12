---
id: meta-log-browser-db-indexeddb-guide
title: "IndexedDB Persistence Guide"
level: practical
type: guide
tags: [meta-log-browser-db, indexeddb, persistence, browser-storage, caching]
keywords: [indexeddb-persistence, browser-storage, persistent-cache, object-stores, async-storage, storage-quotas]
prerequisites: [meta-log-browser-db-readme]
enables: [meta-log-browser-db-migration-guide, meta-log-browser-db-architecture]
related: [meta-log-browser-db-readme, meta-log-browser-db-api-reference, meta-log-browser-db-bip32-39-44-guide]
readingTime: 35
difficulty: 3
blackboard:
  status: active
  assignedAgent: "4D-Network-Agent"
  lastUpdate: "2025-01-07"
  dependencies: [meta-log-browser-db-readme]
  watchers: ["6D-Intelligence-Agent"]
  storageImplementation:
    objectStores: ["files", "triples", "facts"]
    cacheStrategies: ["memory", "indexeddb", "both"]
    encryption: "Optional BIP32/39/44 encryption"
---

# IndexedDB Persistence Guide

Complete guide to using IndexedDB persistence in Meta-Log Browser Database.

## Overview

IndexedDB provides persistent storage for:

- **Files**: Cached JSONL/CanvasL files
- **Triples**: RDF triples for SPARQL queries
- **Facts**: DataLog facts for queries

## IndexedDBStorage

### Basic Usage

```typescript
import { IndexedDBStorage } from 'meta-log-db/browser';

// Create storage instance
const storage = new IndexedDBStorage({
  dbName: 'meta-log-db',
  version: 1
});

// Initialize (async)
await storage.init();

// Store data
await storage.set('files', 'automaton-kernel.jsonl', jsonlContent);

// Retrieve data
const content = await storage.get('files', 'automaton-kernel.jsonl');

// Delete data
await storage.delete('files', 'automaton-kernel.jsonl');

// Clear store
await storage.clear('files');
```

### Object Stores

The database includes three object stores:

1. **`files`**: Cached file content
2. **`triples`**: RDF triples
3. **`facts`**: DataLog facts

### Methods

#### `init(): Promise<void>`

Initialize IndexedDB connection and create object stores if needed.

```typescript
await storage.init();
```

#### `get(storeName: string, key: string): Promise<any | null>`

Get value from object store.

```typescript
const value = await storage.get('files', 'automaton-kernel.jsonl');
if (value === null) {
  console.log('File not found in IndexedDB');
}
```

#### `set(storeName: string, key: string, value: any): Promise<void>`

Set value in object store.

```typescript
await storage.set('files', 'automaton-kernel.jsonl', jsonlContent);
```

#### `delete(storeName: string, key: string): Promise<void>`

Delete value from object store.

```typescript
await storage.delete('files', 'automaton-kernel.jsonl');
```

#### `clear(storeName: string): Promise<void>`

Clear all values from object store.

```typescript
await storage.clear('files');
```

#### `keys(storeName: string): Promise<string[]>`

Get all keys from object store.

```typescript
const keys = await storage.keys('files');
console.log(`Stored files: ${keys.length}`);
```

#### `has(storeName: string, key: string): Promise<boolean>`

Check if key exists in object store.

```typescript
const exists = await storage.has('files', 'automaton-kernel.jsonl');
```

#### `close(): void`

Close database connection.

```typescript
storage.close();
```

## BrowserFileIO Integration

`BrowserFileIO` automatically uses IndexedDB for caching:

```typescript
import { BrowserFileIO } from 'meta-log-db/browser';

const fileIO = new BrowserFileIO({
  enableCache: true,
  cacheStrategy: 'both' // Use both memory and IndexedDB
});

await fileIO.init();

// Load file (checks IndexedDB cache first, then fetches from URL)
const content = await fileIO.loadFile('automaton-kernel.jsonl', '/jsonl/automaton-kernel.jsonl');

// File is automatically cached in IndexedDB
```

### Cache Strategies

- **`memory`**: Cache only in memory (lost on page reload)
- **`indexeddb`**: Cache only in IndexedDB (persistent)
- **`both`**: Cache in both memory and IndexedDB (fast + persistent)

## MetaLogDbBrowser Integration

The database automatically uses IndexedDB for persistence:

```typescript
import { MetaLogDbBrowser } from 'meta-log-db/browser';

const db = new MetaLogDbBrowser({
  cacheStrategy: 'both', // Use IndexedDB caching
  indexedDBName: 'meta-log-db'
});

await db.init();

// Loaded files are cached in IndexedDB
await db.loadCanvas('automaton-kernel.jsonl', '/jsonl/automaton-kernel.jsonl');

// Subsequent loads use IndexedDB cache
await db.loadCanvas('automaton-kernel.jsonl'); // Uses cache
```

## Storage Patterns

### Pattern 1: File Caching

```typescript
// Load file with automatic caching
const content = await fileIO.loadFile('file.jsonl', '/jsonl/file.jsonl');

// Check if cached
const isCached = await fileIO.isCached('file.jsonl');

// Clear cache
await fileIO.clearFileCache('file.jsonl');
```

### Pattern 2: Manual Storage

```typescript
// Store data manually
await storage.set('files', 'custom-data.jsonl', jsonlContent);

// Retrieve manually
const data = await storage.get('files', 'custom-data.jsonl');
```

### Pattern 3: Triple Storage

```typescript
// Store RDF triples
const triples = db.jsonlToRdf(facts);
for (const triple of triples) {
  await storage.set('triples', triple.subject, triple);
}

// Retrieve triples
const storedTriples = await storage.keys('triples');
```

### Pattern 4: Fact Storage

```typescript
// Store facts
const facts = db.extractFacts();
for (let i = 0; i < facts.length; i++) {
  await storage.set('facts', `fact-${i}`, facts[i]);
}

// Retrieve facts
const factKeys = await storage.keys('facts');
const storedFacts = await Promise.all(
  factKeys.map(key => storage.get('facts', key))
);
```

## Encryption with IndexedDB

When encryption is enabled, data is encrypted before storing:

```typescript
import { MetaLogDbBrowser, generateMnemonic } from 'meta-log-db/browser';

const mnemonic = await generateMnemonic(256);

const db = new MetaLogDbBrowser({
  enableEncryption: true,
  mnemonic: mnemonic,
  cacheStrategy: 'indexeddb' // Use IndexedDB for persistent encrypted storage
});

await db.init();

// Files are encrypted before storing in IndexedDB
await db.loadCanvas('automaton-kernel.jsonl', '/jsonl/automaton-kernel.jsonl');
```

## Database Management

### Check Database Size

```typescript
// Get all keys to estimate size
const fileKeys = await storage.keys('files');
const tripleKeys = await storage.keys('triples');
const factKeys = await storage.keys('facts');

console.log(`Files: ${fileKeys.length}, Triples: ${tripleKeys.length}, Facts: ${factKeys.length}`);
```

### Clear All Data

```typescript
// Clear all object stores
await storage.clear('files');
await storage.clear('triples');
await storage.clear('facts');
```

### Delete Database

```typescript
// Close connection
storage.close();

// Delete database (browser API)
const deleteRequest = indexedDB.deleteDatabase('meta-log-db');
deleteRequest.onsuccess = () => {
  console.log('Database deleted');
};
```

## Best Practices

1. **Initialize Before Use**: Always call `init()` before using storage
2. **Handle Errors**: Wrap storage operations in try-catch
3. **Check Existence**: Use `has()` before `get()` to avoid null checks
4. **Clear Unused Data**: Periodically clear old cached files
5. **Use Encryption**: Enable encryption for sensitive data
6. **Monitor Size**: Check database size to avoid quota issues

## Browser Quotas

IndexedDB has storage quotas:

- **Chrome/Edge**: ~60% of available disk space
- **Firefox**: ~50% of available disk space
- **Safari**: ~1GB (can be increased)

Check quota usage:

```typescript
if ('storage' in navigator && 'estimate' in navigator.storage) {
  const estimate = await navigator.storage.estimate();
  console.log(`Used: ${estimate.usage}, Quota: ${estimate.quota}`);
}
```

## Error Handling

```typescript
try {
  await storage.init();
  await storage.set('files', 'test.jsonl', 'content');
} catch (error) {
  if (error.name === 'QuotaExceededError') {
    console.error('Storage quota exceeded');
    // Clear old data or request more quota
  } else {
    console.error('Storage error:', error);
  }
}
```

