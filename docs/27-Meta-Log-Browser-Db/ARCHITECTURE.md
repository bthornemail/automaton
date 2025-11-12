---
id: meta-log-browser-db-architecture
title: "Browser Architecture"
level: foundational
type: architecture-explanation
tags: [meta-log-browser-db, architecture, browser-architecture, design-patterns, component-diagram]
keywords: [browser-architecture, component-design, data-flow, encryption-flow, cache-strategy, browser-apis, web-crypto-api, indexeddb-api]
prerequisites: [meta-log-browser-db-readme]
enables: [meta-log-browser-db-api-reference, meta-log-browser-db-migration-guide]
related: [meta-log-browser-db-readme, meta-log-browser-db-api-reference, meta-log-browser-db-bip32-39-44-guide, meta-log-browser-db-indexeddb-guide]
readingTime: 45
difficulty: 4
blackboard:
  status: active
  assignedAgent: "6D-Intelligence-Agent"
  lastUpdate: "2025-01-07"
  dependencies: [meta-log-browser-db-readme]
  watchers: ["4D-Network-Agent", "5D-Consensus-Agent"]
  architectureLayers:
    - "MetaLogDbBrowser (API Layer)"
    - "ProLog/DataLog/R5RS/RDF/SHACL Engines"
    - "BrowserJsonlParser / BrowserR5RSRegistry"
    - "BrowserFileIO / IndexedDBStorage"
    - "BIP32/39/44 / Storage Encryption"
    - "Fetch API / IndexedDB / Web Crypto API"
---

# Browser Architecture

Architecture explanation for Meta-Log Browser Database.

## Overview

The browser version is designed to work entirely in the browser without Node.js dependencies, using:

- **Web APIs**: Fetch API, IndexedDB, Web Crypto API
- **Browser Storage**: IndexedDB for persistence
- **Browser Cryptography**: Web Crypto API for BIP32/39/44
- **Browser File I/O**: Fetch API for loading files

## Architecture Layers

```
┌─────────────────────────────────────────┐
│      MetaLogDbBrowser (API Layer)      │
├─────────────────────────────────────────┤
│  ProLog │ DataLog │ R5RS │ RDF │ SHACL │
├─────────────────────────────────────────┤
│      BrowserJsonlParser                 │
│      BrowserR5RSRegistry                │
├─────────────────────────────────────────┤
│      BrowserFileIO                      │
│      IndexedDBStorage                   │
├─────────────────────────────────────────┤
│  BIP32/39/44 │ Storage Encryption       │
├─────────────────────────────────────────┤
│  Fetch API │ IndexedDB │ Web Crypto API │
└─────────────────────────────────────────┘
```

## Component Overview

### 1. MetaLogDbBrowser

Main database class that coordinates all components.

**Responsibilities:**
- Initialize all engines
- Coordinate file loading
- Manage cache strategy
- Handle encryption

**Dependencies:**
- BrowserJsonlParser
- BrowserR5RSRegistry
- BrowserFileIO
- IndexedDBStorage
- Crypto utilities

### 2. BrowserFileIO

File I/O abstraction layer.

**Responsibilities:**
- Load files from URLs
- Cache files in IndexedDB
- Manage cache strategies
- Handle encryption/decryption

**Implementation:**
- Uses `fetch()` API for URL loading
- Uses `IndexedDBStorage` for caching
- Supports memory, IndexedDB, or both cache strategies

### 3. IndexedDBStorage

IndexedDB persistence layer.

**Responsibilities:**
- Manage IndexedDB connections
- Store/retrieve data from object stores
- Handle database initialization
- Provide async storage API

**Object Stores:**
- `files`: Cached file content
- `triples`: RDF triples
- `facts`: DataLog facts

### 4. BrowserJsonlParser

Browser-specific JSONL parser.

**Responsibilities:**
- Parse JSONL/CanvasL files
- Extract facts from canvas
- Convert facts to RDF triples
- Handle encrypted content

**Differences from Node.js version:**
- Uses `BrowserFileIO` instead of `fs`
- Supports URL-based loading
- Handles encryption/decryption

### 5. BrowserR5RSRegistry

Browser-specific R5RS registry.

**Responsibilities:**
- Load R5RS engine from URL
- Register R5RS functions
- Execute R5RS functions
- Cache parsed expressions

**Differences from Node.js version:**
- Uses `BrowserFileIO` instead of `fs`
- Supports URL-based loading
- Handles encryption/decryption

### 6. Crypto Layer

BIP32/39/44 cryptographic implementation.

**Components:**
- **BIP32**: HD key derivation
- **BIP39**: Mnemonic generation/validation
- **BIP44**: Standard derivation paths
- **Storage Encryption**: Encrypt/decrypt utilities

**Implementation:**
- Uses Web Crypto API
- No external dependencies
- Pure browser implementation

## Data Flow

### File Loading Flow

```
1. loadCanvas(path, url)
   ↓
2. BrowserFileIO.loadFile(path, url)
   ↓
3. Check IndexedDB cache
   ├─→ Found: Return cached content
   └─→ Not found: Fetch from URL
       ↓
4. Cache in IndexedDB (if enabled)
   ↓
5. Decrypt if encryption enabled
   ↓
6. Parse JSONL/CanvasL
   ↓
7. Extract facts
   ↓
8. Load into engines (ProLog, DataLog, RDF)
```

### Encryption Flow

```
1. Generate mnemonic
   ↓
2. Convert mnemonic to seed (BIP39)
   ↓
3. Derive storage key (BIP32/BIP44)
   ↓
4. Encrypt data with key
   ↓
5. Store encrypted data in IndexedDB
   ↓
6. On load: Decrypt data with same key
```

### Query Flow

```
1. Execute query (ProLog/DataLog/SPARQL)
   ↓
2. Query engine processes query
   ├─→ ProLog: Unification and resolution
   ├─→ DataLog: Bottom-up evaluation
   └─→ SPARQL: Triple pattern matching
   ↓
3. Return results
```

## Cache Strategy

### Memory-Only

- Fast access
- Lost on page reload
- No persistence

### IndexedDB-Only

- Persistent storage
- Slower than memory
- Survives page reloads

### Both (Default)

- Fast memory access
- Persistent IndexedDB backup
- Best of both worlds

## Encryption Architecture

### Key Derivation

```
Mnemonic (BIP39)
  ↓
Seed (PBKDF2)
  ↓
Storage Key (BIP32/BIP44)
  ↓
Encryption Key (AES-GCM)
```

### Encryption Process

```
Plain Data
  ↓
AES-GCM Encrypt (with IV)
  ↓
Base64 Encode
  ↓
Store in IndexedDB
```

### Decryption Process

```
Load from IndexedDB
  ↓
Base64 Decode
  ↓
Extract IV
  ↓
AES-GCM Decrypt
  ↓
Plain Data
```

## Browser Compatibility

### Required APIs

- **Fetch API**: File loading
- **IndexedDB**: Persistent storage
- **Web Crypto API**: Cryptography
- **ES2020**: Modern JavaScript features

### Browser Support

- Chrome/Edge: ✅ Full support
- Firefox: ✅ Full support
- Safari: ✅ Full support (iOS 13+)
- Opera: ✅ Full support

### Polyfills

No polyfills required - uses native browser APIs.

## Performance Considerations

### File Loading

- **First Load**: Fetch from URL (network latency)
- **Cached Load**: Load from IndexedDB (fast)
- **Memory Cache**: Instant (if using 'both' strategy)

### IndexedDB Operations

- **Read**: ~1-5ms per operation
- **Write**: ~5-10ms per operation
- **Batch Operations**: More efficient than individual

### Encryption

- **Key Derivation**: ~10-50ms (one-time)
- **Encryption**: ~1-5ms per file
- **Decryption**: ~1-5ms per file

## Security Considerations

### Encryption

- Uses AES-GCM (authenticated encryption)
- Random IV for each encryption
- Keys derived from mnemonic (never stored)

### Storage

- Encrypted data in IndexedDB
- Keys never stored
- Mnemonic must be stored securely by user

### Best Practices

1. Never store mnemonic in code
2. Use strong passphrases
3. Derive separate keys for different purposes
4. Validate mnemonic before use
5. Clear sensitive data when done

## Extension Points

### Custom File Loaders

```typescript
class CustomFileIO extends BrowserFileIO {
  async loadFromURL(url: string): Promise<string> {
    // Custom loading logic
  }
}
```

### Custom Storage

```typescript
class CustomStorage extends IndexedDBStorage {
  async set(storeName: string, key: string, value: any): Promise<void> {
    // Custom storage logic
  }
}
```

### Custom Encryption

```typescript
class CustomEncryption {
  async encrypt(data: string, key: CryptoKey): Promise<string> {
    // Custom encryption logic
  }
}
```

## Future Enhancements

1. **Service Worker Support**: Offline file caching
2. **WebAssembly**: Faster crypto operations
3. **SharedArrayBuffer**: Parallel processing
4. **Web Locks API**: Concurrent access control
5. **File System Access API**: Direct file system access (when available)

