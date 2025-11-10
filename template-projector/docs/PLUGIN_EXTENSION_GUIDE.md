# Plugin Extension Guide

**Version**: 1.0  
**Last Updated**: 2025-01-07

## Overview

This guide explains how to create custom plugins for the CanvasL Semantic Slides Projector. Plugins extend the projector's capabilities by adding new data sources, rendering methods, and Meta-Log integrations.

## Table of Contents

1. [Plugin Architecture](#plugin-architecture)
2. [Creating a Plugin](#creating-a-plugin)
3. [Plugin Lifecycle](#plugin-lifecycle)
4. [Meta-Log Integration](#meta-log-integration)
5. [Error Handling](#error-handling)
6. [Plugin Examples](#plugin-examples)
7. [Best Practices](#best-practices)

## Plugin Architecture

### BasePlugin Class

All plugins extend the `BasePlugin` class, which provides:

- **Lifecycle Management**: `onInit()`, `onRender()`, `onEvolve()`
- **Meta-Log Hooks**: Access to ProLog, DataLog, SPARQL, R5RS
- **Plugin Registry**: Automatic registration with projector
- **Error Handling**: Structured error reporting

### Plugin Structure

```javascript
import { BasePlugin } from './BasePlugin.js';

export class MyPlugin extends BasePlugin {
  constructor(config = {}) {
    super({
      name: 'MyPlugin',
      version: '0.1.0',
      hooks: ['sparql', 'prolog'], // Required Meta-Log hooks
      ...config
    });
    
    // Plugin-specific initialization
  }
}
```

## Creating a Plugin

### Step 1: Define Plugin Class

```javascript
import { BasePlugin } from './BasePlugin.js';

export class WikidataPlugin extends BasePlugin {
  constructor(config = {}) {
    super({
      name: 'Wikidata',
      version: '0.1.0',
      hooks: ['sparql'],
      endpoint: config.endpoint || 'https://query.wikidata.org/sparql',
      ...config
    });
    
    this.cache = new Map();
  }
}
```

### Step 2: Implement Lifecycle Methods

```javascript
async onInit() {
  console.log(`WikidataPlugin initialized with endpoint: ${this.config.endpoint}`);
  // Initialize plugin resources
}

async onRender(slide, mode = 'static') {
  // Render slide content
  return slide;
}

async onEvolve(slide) {
  // Modify slide based on context
  return slide;
}
```

### Step 3: Add Plugin Methods

```javascript
async queryEntity(wikidataId, property) {
  const query = `
    SELECT ?value WHERE {
      wd:${wikidataId} wdt:${property} ?value .
    }
    LIMIT 1
  `;
  
  const result = await this.hook('sparql', {
    query,
    endpoint: this.config.endpoint
  });
  
  return this.processResult(result);
}
```

### Step 4: Register Plugin

```javascript
// In Projector.js or your application
import { WikidataPlugin } from './plugin/wikidata-plugin.js';

const projector = new Projector();
const wikidataPlugin = new WikidataPlugin();
await projector.registerPlugin(wikidataPlugin);
```

## Plugin Lifecycle

### Initialization (`onInit`)

Called when plugin is registered:

```javascript
async onInit() {
  // Initialize plugin resources
  // Access Meta-Log bridge via this.metaLog
  // Set up event listeners
  // Load configuration
}
```

### Rendering (`onRender`)

Called for each slide during rendering:

```javascript
async onRender(slide, mode = 'static') {
  // mode: 'static' | 'interactive' | 'offscreen'
  
  // Enrich slide with plugin data
  if (slide.plugin === this.name) {
    slide.data = await this.fetchData(slide);
  }
  
  return slide;
}
```

### Evolution (`onEvolve`)

Called when slide evolves (self-modification):

```javascript
async onEvolve(slide) {
  // Modify slide based on context
  // Use Meta-Log queries for decision-making
  const shouldEnrich = await this.metaLog.prologQuery(
    '(should_enrich ?Slide)',
    [{ predicate: 'slide', args: [slide.id] }]
  );
  
  if (shouldEnrich.length > 0) {
    return await this.enrichSlide(slide);
  }
  
  return slide;
}
```

## Meta-Log Integration

### Available Hooks

Plugins can use Meta-Log hooks via `this.hook()`:

#### SPARQL Hook

```javascript
const result = await this.hook('sparql', {
  query: 'SELECT ?s ?p ?o WHERE { ?s ?p ?o }',
  endpoint: 'https://dbpedia.org/sparql' // Optional
});
```

#### ProLog Hook

```javascript
const result = await this.hook('prolog', {
  query: '(parent ?X ?Y)',
  facts: [
    { predicate: 'parent', args: ['alice', 'bob'] }
  ]
});
```

#### DataLog Hook

```javascript
const result = await this.hook('datalog', {
  goal: '(ancestor ?X ?Y)',
  program: {
    rules: [{ head: 'ancestor(X,Y)', body: ['parent(X,Y)'] }],
    facts: [{ predicate: 'parent', args: ['alice', 'bob'] }]
  }
});
```

#### R5RS Hook

```javascript
const result = await this.hook('r5rs', {
  expression: '(church-add 2 3)',
  context: {}
});
```

### Direct Meta-Log Access

Plugins can also access Meta-Log bridge directly:

```javascript
// Direct access (if needed)
const prologEngine = this.metaLog.adapter.getProlog();
const datalogEngine = this.metaLog.adapter.getDatalog();
const rdfStore = this.metaLog.adapter.getRdf();
```

## Error Handling

### Error Types

Use structured error types:

```javascript
import { ErrorHandler } from '../utils/ErrorHandler.js';

const errorHandler = new ErrorHandler();

try {
  const result = await this.queryEntity(id);
} catch (error) {
  const recovery = await errorHandler.handle(error, {
    retry: () => this.queryEntity(id),
    fallback: () => this.getCachedEntity(id)
  });
  
  if (recovery.recovered) {
    return recovery.recovery;
  }
  
  throw error;
}
```

### Error Recovery Strategies

Register recovery strategies:

```javascript
errorHandler.registerRecoveryStrategy('network', async (error, context) => {
  // Retry with exponential backoff
  for (let i = 0; i < 3; i++) {
    await new Promise(resolve => setTimeout(resolve, 1000 * Math.pow(2, i)));
    try {
      return await context.retry();
    } catch (retryError) {
      if (i === 2) throw retryError;
    }
  }
});
```

## Plugin Examples

### Example 1: Wikidata Plugin

```javascript
import { BasePlugin } from './BasePlugin.js';

export class WikidataPlugin extends BasePlugin {
  constructor(config = {}) {
    super({
      name: 'Wikidata',
      version: '0.1.0',
      hooks: ['sparql'],
      endpoint: config.endpoint || 'https://query.wikidata.org/sparql',
      ...config
    });
    
    this.cache = new Map();
  }

  async onInit() {
    console.log('WikidataPlugin initialized');
  }

  async queryProperty(wikidataId, propertyId) {
    const cacheKey = `wikidata:${wikidataId}:${propertyId}`;
    
    if (this.cache.has(cacheKey)) {
      return this.cache.get(cacheKey);
    }
    
    const query = `
      SELECT ?value WHERE {
        wd:${wikidataId} wdt:${propertyId} ?value .
      }
      LIMIT 1
    `;
    
    const result = await this.hook('sparql', {
      query,
      endpoint: this.config.endpoint
    });
    
    const data = {
      value: result.results?.bindings[0]?.value?.value || null
    };
    
    this.cache.set(cacheKey, data);
    return data;
  }
}
```

### Example 2: GeoNames Plugin

```javascript
import { BasePlugin } from './BasePlugin.js';

export class GeoNamesPlugin extends BasePlugin {
  constructor(config = {}) {
    super({
      name: 'GeoNames',
      version: '0.1.0',
      hooks: ['sparql'],
      endpoint: config.endpoint || 'https://www.geonames.org/sparql',
      ...config
    });
  }

  async queryLocation(geonameId) {
    const query = `
      SELECT ?name ?lat ?lng WHERE {
        geonames:${geonameId} geonames:name ?name ;
                              geonames:lat ?lat ;
                              geonames:lng ?lng .
      }
    `;
    
    const result = await this.hook('sparql', {
      query,
      endpoint: this.config.endpoint
    });
    
    return result.results?.bindings[0] || null;
  }
}
```

### Example 3: Custom Renderer Plugin

```javascript
import { BasePlugin } from './BasePlugin.js';

export class CustomRendererPlugin extends BasePlugin {
  constructor(config = {}) {
    super({
      name: 'CustomRenderer',
      version: '0.1.0',
      hooks: [],
      ...config
    });
  }

  async onRender(slide, mode = 'static') {
    if (slide.renderer === 'custom') {
      // Custom rendering logic
      slide.rendered = await this.renderCustom(slide);
    }
    
    return slide;
  }

  async renderCustom(slide) {
    // Use canvas API, WebGL, etc.
    const canvas = document.createElement('canvas');
    const ctx = canvas.getContext('2d');
    
    // Render slide content
    ctx.fillText(slide.title, 10, 10);
    
    return canvas.toDataURL();
  }
}
```

## Best Practices

### 1. Caching

Always implement caching for external API calls:

```javascript
this.cache = new Map();
const cacheKey = `${source}:${id}:${property}`;

if (this.cache.has(cacheKey)) {
  const cached = this.cache.get(cacheKey);
  if (Date.now() - cached.timestamp < this.config.cacheTTL) {
    return cached.data;
  }
}
```

### 2. Error Handling

Use structured error handling:

```javascript
try {
  const result = await this.query();
} catch (error) {
  if (error instanceof NetworkError) {
    // Handle network error
  } else if (error instanceof ParseError) {
    // Handle parse error
  } else {
    // Handle unknown error
  }
}
```

### 3. Configuration

Make plugins configurable:

```javascript
constructor(config = {}) {
  super({
    name: 'MyPlugin',
    version: '0.1.0',
    hooks: ['sparql'],
    endpoint: config.endpoint || 'https://default.endpoint.org',
    cacheTTL: config.cacheTTL || 900000,
    maxRetries: config.maxRetries || 3,
    ...config
  });
}
```

### 4. Documentation

Document plugin methods:

```javascript
/**
 * Query entity property
 * @param {string} entityId - Entity ID
 * @param {string} property - Property to query
 * @returns {Promise<Object>} Query result
 * @throws {PluginError} If query fails
 */
async queryProperty(entityId, property) {
  // Implementation
}
```

### 5. Testing

Create test files for plugins:

```javascript
// test/wikidata-plugin.test.js
import { WikidataPlugin } from '../src/plugin/wikidata-plugin.js';

describe('WikidataPlugin', () => {
  let plugin;
  
  beforeEach(() => {
    plugin = new WikidataPlugin();
  });
  
  test('should query property', async () => {
    const result = await plugin.queryProperty('Q42', 'P31');
    expect(result.value).toBeDefined();
  });
});
```

## Plugin Manifest

Create a `plugin-manifest.json` for your plugin:

```json
{
  "name": "MyPlugin",
  "version": "0.1.0",
  "description": "Plugin description",
  "author": "Your Name",
  "hooks": ["sparql", "prolog"],
  "dependencies": [],
  "metaLogFeatures": {
    "sparql": true,
    "prolog": false,
    "datalog": false,
    "r5rs": false
  }
}
```

## Integration with Projector

### Loading Plugins

```javascript
// In Projector.js
async loadBuiltInPlugins() {
  try {
    const { DBpediaPlugin } = await import('../plugin/dbpedia-plugin.js');
    const dbpediaPlugin = new DBpediaPlugin();
    await this.registerPlugin(dbpediaPlugin);
  } catch (error) {
    console.warn('Failed to load DBpedia plugin:', error);
  }
}
```

### Using Plugins

```javascript
// In slide rendering
const plugin = projector.getPlugin('DBpedia');
if (plugin) {
  const data = await plugin.queryAbstract('Albert_Einstein');
  slide.enriched = data;
}
```

## Resources

- **BasePlugin**: `src/plugin/BasePlugin.js`
- **DBpediaPlugin Example**: `src/plugin/dbpedia-plugin.js`
- **ErrorHandler**: `src/utils/ErrorHandler.js`
- **Projector**: `src/projector/Projector.js`

## Support

For questions or issues, please refer to:
- Project documentation: `docs/26-CanvasL-Semantic-Slides-Project/`
- Status report: `docs/26-CanvasL-Semantic-Slides-Project/03-STATUS.md`
