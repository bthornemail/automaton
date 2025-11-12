---
id: meta-log-browser-db-readme
title: "Meta-Log Browser Database"
level: foundational
type: navigation
tags: [meta-log-browser-db, browser-native, indexeddb, bip32, bip39, bip44, encryption, prolog, datalog, r5rs]
keywords: [meta-log-browser-db, browser-native-database, indexeddb-persistence, bip32-bip39-bip44, storage-encryption, browser-file-io, fetch-api, web-crypto-api]
prerequisites: [meta-log-db-progress-readme, meta-log-docs-readme]
enables: [meta-log-browser-db-api-reference, meta-log-browser-db-bip32-39-44-guide, meta-log-browser-db-indexeddb-guide, meta-log-browser-db-migration-guide]
related: [meta-log-db-progress-readme, meta-log-docs-readme, canvasl-rfc2119-spec, multiverse-canvas-rfc2119-spec]
readingTime: 20
difficulty: 4
blackboard:
  status: active
  assignedAgent: "4D-Network-Agent"
  lastUpdate: "2025-01-07"
  dependencies: [meta-log-db]
  watchers: ["6D-Intelligence-Agent", "5D-Consensus-Agent"]
  r5rsEngine: "r5rs-canvas-engine.scm"
  selfBuilding:
    enabled: true
    source: "meta-log-db/src/browser"
    pattern: "browser-native-implementation"
    regeneration:
      function: "r5rs:parse-jsonl-canvas"
      args: ["generate.metaverse.jsonl"]
      context:
        module: "MODULE 2: JSONL Parser & Canvas Loader"
        functions: ["r5rs:parse-jsonl-canvas", "r5rs:extract-facts", "r5rs:jsonl-to-rdf"]
  browserFeatures:
    fileIO: "BrowserFileIO with fetch API"
    storage: "IndexedDBStorage for persistence"
    encryption: "BIP32/39/44 cryptographic support"
    caching: "Memory + IndexedDB cache strategies"
---

# Meta-Log Browser Database

Browser-native implementation of Meta-Log Database with BIP32/39/44 cryptographic support, IndexedDB persistence, and browser file I/O.

## Overview

The Meta-Log Browser Database (`MetaLogDbBrowser`) provides a complete browser-native implementation of the Meta-Log Database system, enabling:

- **Browser File I/O**: Load files from URLs/public directories using fetch API
- **IndexedDB Persistence**: Cache and persist data in browser storage
- **BIP32/39/44 Cryptography**: HD wallet derivation, mnemonic generation, and storage encryption
- **Full Meta-Log Features**: ProLog, DataLog, R5RS, RDF/SPARQL, and SHACL validation

## Quick Start

### Installation

```bash
npm install meta-log-db
```

### Basic Usage

```typescript
import { MetaLogDbBrowser } from 'meta-log-db/browser';

// Create browser database instance
const db = new MetaLogDbBrowser({
  enableProlog: true,
  enableDatalog: true,
  enableRdf: true,
  enableShacl: true
});

// Initialize (async)
await db.init();

// Load JSONL canvas from URL
await db.loadCanvas('automaton-kernel.jsonl', '/jsonl/automaton-kernel.jsonl');

// Extract facts
const facts = db.extractFacts();

// Execute ProLog query
const results = await db.prologQuery('(node ?Id ?Type)');

// Execute SPARQL query
const sparqlResults = await db.sparqlQuery(`
  SELECT ?id ?type WHERE {
    ?id rdf:type ?type
  }
`);
```

### With Encryption

```typescript
import { MetaLogDbBrowser } from 'meta-log-db/browser';
import { generateMnemonic } from 'meta-log-db/browser';

// Generate mnemonic for encryption
const mnemonic = await generateMnemonic(256);

// Create database with encryption enabled
const db = new MetaLogDbBrowser({
  enableEncryption: true,
  mnemonic: mnemonic,
  cacheStrategy: 'both' // Use both memory and IndexedDB cache
});

await db.init();

// Files will be encrypted before storing in IndexedDB
await db.loadCanvas('automaton-kernel.jsonl', '/jsonl/automaton-kernel.jsonl');
```

## Key Features

### Browser File I/O

- **URL Loading**: Fetch files from URLs or public directories
- **IndexedDB Caching**: Automatic caching of loaded files
- **Cache Strategies**: Memory-only, IndexedDB-only, or both

### IndexedDB Persistence

- **Persistent Storage**: Store files, triples, and facts in IndexedDB
- **Object Stores**: Separate stores for files, triples, and facts
- **Async Operations**: All storage operations are asynchronous

### BIP32/39/44 Cryptography

- **HD Key Derivation**: Derive keys from seeds using BIP32
- **Mnemonic Generation**: Generate and validate BIP39 mnemonic phrases
- **Storage Encryption**: Encrypt/decrypt data using derived keys
- **Standard Paths**: BIP44 derivation paths for different purposes

### Full Meta-Log Support

- **ProLog Engine**: Logic programming with unification
- **DataLog Engine**: Fact extraction and bottom-up evaluation
- **R5RS Functions**: Scheme function registry and execution
- **RDF/SPARQL**: Triple storage and SPARQL queries
- **SHACL Validation**: Shape constraint validation

## Documentation

- **[BROWSER-API-REFERENCE.md](./BROWSER-API-REFERENCE.md)**: Complete API documentation
- **[BIP32-39-44-INTEGRATION.md](./BIP32-39-44-INTEGRATION.md)**: Cryptographic features guide
- **[INDEXEDDB-PERSISTENCE.md](./INDEXEDDB-PERSISTENCE.md)**: IndexedDB storage guide
- **[MIGRATION-GUIDE.md](./MIGRATION-GUIDE.md)**: Migrating from Node.js version
- **[ARCHITECTURE.md](./ARCHITECTURE.md)**: Architecture explanation

## Browser vs Node.js

| Feature | Node.js (`MetaLogDb`) | Browser (`MetaLogDbBrowser`) |
|---------|----------------------|------------------------------|
| File I/O | `fs.readFileSync` | `fetch` API + IndexedDB |
| Storage | File system | IndexedDB |
| Encryption | Optional (external) | Built-in BIP32/39/44 |
| Cache | Memory only | Memory + IndexedDB |
| Initialization | Synchronous | Async (`init()`) |

## Examples

See the [BROWSER-API-REFERENCE.md](./BROWSER-API-REFERENCE.md) for complete examples.

## License

MIT

