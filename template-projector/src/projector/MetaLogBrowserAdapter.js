/**
 * MetaLogBrowserAdapter - Browser-compatible adapter for meta-log-db
 * 
 * @deprecated Use CanvasLMetaverseBrowser from 'meta-log-db/browser' instead.
 * This class is kept for backward compatibility but will be removed in a future version.
 * 
 * Migration:
 * ```javascript
 * // Old:
 * import { MetaLogBrowserAdapter } from './MetaLogBrowserAdapter.js';
 * const adapter = new MetaLogBrowserAdapter();
 * await adapter.loadCanvas(url, path);
 * 
 * // New:
 * import { CanvasLMetaverseBrowser } from 'meta-log-db/browser';
 * const browser = new CanvasLMetaverseBrowser();
 * await browser.loadCanvas(path, url); // Note: parameter order changed
 * ```
 * 
 * Uses browser-native MetaLogDbBrowser implementation:
 * - Native browser file I/O with fetch API
 * - IndexedDB caching for performance
 * - Built-in encryption support (optional)
 * - No Node.js polyfills required
 * 
 * @see {@link https://github.com/automaton-system/meta-log-db/blob/main/docs/27-Meta-Log-Browser-Db/README.md Meta-Log Browser Database Documentation}
 * @see {@link https://github.com/automaton-system/meta-log-db/blob/main/docs/27-Meta-Log-Browser-Db/BROWSER-API-REFERENCE.md Browser API Reference}
 * @see {@link https://github.com/automaton-system/meta-log-db/blob/main/docs/27-Meta-Log-Browser-Db/MIGRATION-GUIDE.md Migration Guide}
 * 
 * Related Documentation:
 * - meta-log-browser-db-readme: Browser database overview
 * - meta-log-browser-db-api-reference: Complete API documentation
 * - meta-log-browser-db-migration-guide: Migration from Node.js version
 * - meta-log-browser-db-architecture: Architecture explanation
 */

export class MetaLogBrowserAdapter {
  constructor(config = {}) {
    this.db = null;
    this.initialized = false;
    this.config = {
      enableProlog: true,
      enableDatalog: true,
      enableRdf: true,
      enableShacl: true,
      cacheStrategy: 'both', // Use both memory and IndexedDB cache
      ...config
    };
  }

  /**
   * Initialize meta-log-db with browser-native MetaLogDbBrowser
   */
  async init() {
    if (this.initialized) {
      return;
    }

    try {
      // Import browser-native MetaLogDbBrowser
      const { MetaLogDbBrowser } = await import('meta-log-db/browser');
      
      // Create browser-native database instance
      this.db = new MetaLogDbBrowser(this.config);
      
      // Initialize (sets up IndexedDB, file I/O, etc.)
      await this.db.init();

      this.initialized = true;
      console.log('MetaLogBrowserAdapter initialized with MetaLogDbBrowser');
    } catch (error) {
      console.error('Failed to initialize MetaLogDbBrowser:', error);
      throw new Error(`Meta-log-db browser initialization failed: ${error.message}`);
    }
  }

  /**
   * Load canvas from URL (browser-compatible)
   * @param {string} url - URL to CanvasL/JSONL file (or path if url not provided)
   * @param {string} path - Optional file path identifier (for caching)
   * @returns {Promise<void>}
   */
  async loadCanvas(url, path = null) {
    await this.init();
    
    try {
      // Use path as identifier, url as the actual URL to fetch
      // If path is not provided, use url as both
      const filePath = path || url;
      await this.db.loadCanvas(filePath, url);
    } catch (error) {
      throw new Error(`Failed to load canvas from ${url}: ${error.message}`);
    }
  }

  /**
   * Execute ProLog query
   * @param {string} query - ProLog query
   * @returns {Promise<Object>} Query results
   */
  async prologQuery(query) {
    await this.init();
    return await this.db.prologQuery(query);
  }

  /**
   * Execute DataLog query
   * @param {string} goal - DataLog goal
   * @param {Object} program - Optional DataLog program
   * @returns {Promise<Object>} Query results
   */
  async datalogQuery(goal, program = null) {
    await this.init();
    return await this.db.datalogQuery(goal, program);
  }

  /**
   * Execute SPARQL query
   * @param {string} query - SPARQL query
   * @returns {Promise<Object>} Query results
   */
  async sparqlQuery(query) {
    await this.init();
    return await this.db.sparqlQuery(query);
  }

  /**
   * Validate with SHACL
   * @param {Object} shapes - SHACL shapes (loaded from string, not file)
   * @param {Array} triples - Optional RDF triples to validate
   * @returns {Promise<Object>} Validation result
   */
  async shaclValidate(shapes, triples = null) {
    await this.init();
    return await this.db.validateShacl(shapes, triples);
  }

  /**
   * Add ProLog rule
   * @param {string} rule - ProLog rule string
   */
  addPrologRule(rule) {
    if (!this.db) {
      throw new Error('MetaLogDbBrowser not initialized');
    }
    this.db.addPrologRule(rule);
  }

  /**
   * Add DataLog rule
   * @param {string} rule - DataLog rule string
   */
  addDatalogRule(rule) {
    if (!this.db) {
      throw new Error('MetaLogDbBrowser not initialized');
    }
    
    // Build DataLog program from rule
    const program = this.db.buildDatalogProgram([rule]);
    // Note: The program is built but not automatically applied
    // For immediate effect, we'd need to add facts/rules directly
    // This maintains API compatibility
  }

  /**
   * Store RDF triples
   * @param {Array} triples - RDF triples array
   */
  storeTriples(triples) {
    if (!this.db) {
      throw new Error('MetaLogDbBrowser not initialized');
    }
    this.db.storeTriples(triples);
  }

  /**
   * Get R5RS registry (if available)
   * @returns {Object|null} R5RS registry
   */
  getR5RS() {
    // Access internal R5RS registry if available
    // Note: MetaLogDbBrowser doesn't expose r5rs directly, but we can execute R5RS functions
    return null; // Return null for compatibility, use executeR5RS instead
  }

  /**
   * Execute R5RS function
   * @param {string} functionName - R5RS function name
   * @param {Array} args - Function arguments
   * @returns {Promise<any>} Function result
   */
  async executeR5RS(functionName, args = []) {
    await this.init();
    return await this.db.executeR5RS(functionName, args);
  }

  /**
   * Add facts to ProLog engine
   * @param {Array} facts - Facts array
   */
  addPrologFacts(facts) {
    if (!this.db) {
      throw new Error('MetaLogDbBrowser not initialized');
    }
    // Build ProLog database from facts
    this.db.buildPrologDb(facts);
  }

  /**
   * Add facts to DataLog engine
   * @param {Array} facts - Facts array
   */
  addDatalogFacts(facts) {
    if (!this.db) {
      throw new Error('MetaLogDbBrowser not initialized');
    }
    // Extract facts and add via DataLog query with program
    // Note: This is a workaround since MetaLogDbBrowser doesn't expose engines directly
    // For now, facts are added when loading canvas files
  }

  /**
   * Get ProLog engine (for direct access if needed)
   * @returns {Object|null} ProLog engine wrapper
   */
  getProlog() {
    if (!this.db) {
      return null;
    }
    // Return a wrapper object that mimics the engine API
    const self = this;
    return {
      addFacts: (facts) => self.addPrologFacts(facts),
      clear: async () => await self.clear(),
      query: async (query) => await self.db.prologQuery(query)
    };
  }

  /**
   * Get DataLog engine (for direct access if needed)
   * @returns {Object|null} DataLog engine wrapper
   */
  getDatalog() {
    if (!this.db) {
      return null;
    }
    // Return a wrapper object that mimics the engine API
    const self = this;
    return {
      addFacts: (facts) => self.addDatalogFacts(facts),
      addRule: (rule) => {
        // Parse rule and add via buildDatalogProgram
        const ruleStr = `${rule.head} :- ${rule.body.join(', ')}`;
        self.db.buildDatalogProgram([ruleStr]);
      },
      clear: async () => await self.clear(),
      query: async (goal, program) => await self.db.datalogQuery(goal, program)
    };
  }

  /**
   * Get RDF triple store (for direct access if needed)
   * @returns {Object|null} Triple store wrapper
   */
  getRdf() {
    if (!this.db) {
      return null;
    }
    // Return a wrapper object that mimics the triple store API
    const self = this;
    return {
      addTriples: (triples) => self.db.storeTriples(triples),
      getTriples: () => {
        // Note: MetaLogDbBrowser doesn't expose getTriples directly
        // Query via SPARQL to get all triples if needed
        // For now, return empty array (triples are stored internally)
        return [];
      },
      clear: async () => await self.clear(),
      sparql: async (query) => await self.db.sparqlQuery(query)
    };
  }

  /**
   * Get SHACL validator (for direct access if needed)
   * @returns {Object|null} SHACL validator wrapper
   */
  getShacl() {
    if (!this.db) {
      return null;
    }
    // Return a wrapper object that mimics the validator API
    const self = this;
    return {
      validate: async (shapes, triples) => await self.db.validateShacl(shapes, triples),
      clear: async () => await self.clear()
    };
  }

  /**
   * Clear all data
   */
  async clear() {
    if (this.db) {
      await this.db.clearCache();
      // Note: MetaLogDbBrowser doesn't have a clear() method for engines
      // This would need to be implemented if needed
    }
  }

  /**
   * Extract facts from canvas (delegates to db)
   * @returns {Array} Facts array
   */
  extractFacts() {
    if (!this.db) {
      return [];
    }
    return this.db.extractFacts();
  }

  /**
   * Convert facts to RDF (delegates to db)
   * @param {Array} facts - Optional facts array
   * @returns {Array} RDF triples
   */
  jsonlToRdf(facts = null) {
    if (!this.db) {
      return [];
    }
    return this.db.jsonlToRdf(facts);
  }
}
