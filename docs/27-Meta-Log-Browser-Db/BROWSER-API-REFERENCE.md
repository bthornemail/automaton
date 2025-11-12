# Browser API Reference

Complete API documentation for Meta-Log Browser Database.

## MetaLogDbBrowser

Main browser database class.

### Constructor

```typescript
new MetaLogDbBrowser(config?: BrowserConfig)
```

**Configuration Options:**

```typescript
interface BrowserConfig {
  // Meta-Log engine options
  enableProlog?: boolean;        // Enable ProLog engine (default: true)
  enableDatalog?: boolean;       // Enable DataLog engine (default: true)
  enableRdf?: boolean;           // Enable RDF engine (default: true)
  enableShacl?: boolean;         // Enable SHACL validator (default: true)
  
  // Browser-specific options
  enableEncryption?: boolean;    // Enable encryption (default: false)
  mnemonic?: string;             // BIP39 mnemonic for encryption
  indexedDBName?: string;        // IndexedDB database name (default: 'meta-log-db')
  cacheStrategy?: 'memory' | 'indexeddb' | 'both'; // Cache strategy (default: 'both')
  r5rsEnginePath?: string;       // Path for R5RS engine file
  r5rsEngineURL?: string;        // URL for R5RS engine file
}
```

### Methods

#### `init(): Promise<void>`

Initialize browser database (must be called before use).

```typescript
await db.init();
```

#### `loadCanvas(path: string, url?: string): Promise<void>`

Load JSONL or CanvasL canvas file.

```typescript
// Load from URL
await db.loadCanvas('automaton-kernel.jsonl', '/jsonl/automaton-kernel.jsonl');

// Load from IndexedDB cache (if cached)
await db.loadCanvas('automaton-kernel.jsonl');
```

#### `parseJsonlCanvas(path: string, url?: string): Promise<Canvas>`

Parse JSONL canvas file without loading into engines.

```typescript
const canvas = await db.parseJsonlCanvas('automaton-kernel.jsonl', '/jsonl/automaton-kernel.jsonl');
```

#### `parseCanvasL(path: string, url?: string): Promise<Canvas>`

Parse CanvasL file with extensions.

```typescript
const canvas = await db.parseCanvasL('automaton.canvasl', '/jsonl/automaton.canvasl');
```

#### `extractFacts(): Fact[]`

Extract facts from loaded canvas.

```typescript
const facts = db.extractFacts();
```

#### `prologQuery(query: string): Promise<any>`

Execute ProLog query.

```typescript
const results = await db.prologQuery('(node ?Id ?Type)');
```

#### `datalogQuery(query: string, program?: any): Promise<any>`

Execute DataLog query.

```typescript
const results = await db.datalogQuery('(missing_implementation ?N)');
```

#### `sparqlQuery(query: string): Promise<any>`

Execute SPARQL query.

```typescript
const results = await db.sparqlQuery(`
  SELECT ?id ?type WHERE {
    ?id rdf:type ?type
  }
`);
```

#### `validateShacl(shapes?: any, triples?: any): Promise<any>`

Validate SHACL constraints.

```typescript
const validation = await db.validateShacl();
```

#### `executeR5RS(functionName: string, args: any[]): Promise<any>`

Execute R5RS function.

```typescript
const result = await db.executeR5RS('r5rs:church-add', [2, 3]);
```

#### `clearCache(): Promise<void>`

Clear file cache.

```typescript
await db.clearCache();
```

## BrowserFileIO

Browser file I/O abstraction.

### Constructor

```typescript
new BrowserFileIO(config?: BrowserFileIOConfig)
```

### Methods

#### `loadFromURL(url: string): Promise<string>`

Load file from URL.

```typescript
const content = await fileIO.loadFromURL('/jsonl/automaton-kernel.jsonl');
```

#### `loadFile(path: string, url?: string): Promise<string>`

Load file with cache fallback.

```typescript
const content = await fileIO.loadFile('automaton-kernel.jsonl', '/jsonl/automaton-kernel.jsonl');
```

#### `saveToIndexedDB(key: string, content: string): Promise<void>`

Save content to IndexedDB.

```typescript
await fileIO.saveToIndexedDB('automaton-kernel.jsonl', content);
```

#### `loadFromIndexedDB(key: string): Promise<string | null>`

Load content from IndexedDB.

```typescript
const content = await fileIO.loadFromIndexedDB('automaton-kernel.jsonl');
```

## IndexedDBStorage

IndexedDB persistence layer.

### Constructor

```typescript
new IndexedDBStorage(config?: IndexedDBStorageConfig)
```

### Methods

#### `init(): Promise<void>`

Initialize IndexedDB connection.

```typescript
await storage.init();
```

#### `get(storeName: string, key: string): Promise<any | null>`

Get value from object store.

```typescript
const value = await storage.get('files', 'automaton-kernel.jsonl');
```

#### `set(storeName: string, key: string, value: any): Promise<void>`

Set value in object store.

```typescript
await storage.set('files', 'automaton-kernel.jsonl', content);
```

#### `delete(storeName: string, key: string): Promise<void>`

Delete value from object store.

```typescript
await storage.delete('files', 'automaton-kernel.jsonl');
```

#### `clear(storeName: string): Promise<void>`

Clear object store.

```typescript
await storage.clear('files');
```

## Crypto Utilities

### BIP39

```typescript
import { generateMnemonic, validateMnemonic, mnemonicToSeed } from 'meta-log-db/browser';

// Generate mnemonic
const mnemonic = await generateMnemonic(256); // 256-bit entropy

// Validate mnemonic
const isValid = validateMnemonic(mnemonic);

// Convert to seed
const seed = await mnemonicToSeed(mnemonic, 'passphrase');
```

### BIP32

```typescript
import { deriveKey } from 'meta-log-db/browser';

// Derive key from seed
const key = await deriveKey(seed, "m/44'/60'/0'/0/0");
```

### BIP44

```typescript
import { deriveStorageKey, StorageDerivationPaths } from 'meta-log-db/browser';

// Derive storage key
const storageKey = await deriveStorageKey(mnemonic, 'local');

// Use standard paths
const path = StorageDerivationPaths.LOCAL_PRIVATE;
```

### Storage Encryption

```typescript
import { encryptData, decryptData, encryptDataWithMnemonic, decryptDataWithMnemonic } from 'meta-log-db/browser';

// Encrypt/decrypt with key
const encrypted = await encryptData(data, key);
const decrypted = await decryptData(encrypted, key);

// Encrypt/decrypt with mnemonic
const encrypted = await encryptDataWithMnemonic(data, mnemonic, 'local');
const decrypted = await decryptDataWithMnemonic(encrypted, mnemonic, 'local');
```

## Complete Example

```typescript
import { MetaLogDbBrowser, generateMnemonic } from 'meta-log-db/browser';

async function example() {
  // Generate mnemonic for encryption
  const mnemonic = await generateMnemonic(256);

  // Create database
  const db = new MetaLogDbBrowser({
    enableEncryption: true,
    mnemonic: mnemonic,
    cacheStrategy: 'both'
  });

  // Initialize
  await db.init();

  // Load canvas
  await db.loadCanvas('automaton-kernel.jsonl', '/jsonl/automaton-kernel.jsonl');

  // Extract facts
  const facts = db.extractFacts();
  console.log(`Extracted ${facts.length} facts`);

  // ProLog query
  const prologResults = await db.prologQuery('(node ?Id ?Type)');
  console.log('ProLog results:', prologResults);

  // SPARQL query
  const sparqlResults = await db.sparqlQuery(`
    SELECT ?id ?type WHERE {
      ?id rdf:type ?type
    }
  `);
  console.log('SPARQL results:', sparqlResults);

  // SHACL validation
  const validation = await db.validateShacl();
  console.log('SHACL validation:', validation);
}

example();
```

