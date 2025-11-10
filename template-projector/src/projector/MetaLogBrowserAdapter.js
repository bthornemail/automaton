/**
 * MetaLogBrowserAdapter - Browser-compatible adapter for meta-log-db
 * 
 * Adapts meta-log-db Node.js APIs to browser-compatible APIs:
 * - Replaces fs.readFileSync with fetch()
 * - Provides browser-compatible file loading
 * - Wraps meta-log-db engines for browser use
 */

export class MetaLogBrowserAdapter {
  constructor() {
    this.metaLogDb = null;
    this.initialized = false;
  }

  /**
   * Initialize meta-log-db with browser-compatible configuration
   */
  async init() {
    if (this.initialized) {
      return;
    }

    try {
      // Dynamic import of meta-log-db engines directly (bypass MetaLogDb which uses fs)
      const { 
        PrologEngine, 
        DatalogEngine, 
        TripleStore, 
        ShaclValidator 
      } = await import('meta-log-db');
      
      // Create engines directly (browser-compatible, no file system)
      this.prologEngine = new PrologEngine();
      this.datalogEngine = new DatalogEngine();
      this.tripleStore = new TripleStore();
      this.shaclValidator = new ShaclValidator();

      this.initialized = true;
      console.log('MetaLogBrowserAdapter initialized with engines');
    } catch (error) {
      console.error('Failed to initialize meta-log-db engines:', error);
      // Try fallback: use MetaLogDb if engines not available
      try {
        const { MetaLogDb } = await import('meta-log-db');
        this.metaLogDb = new MetaLogDb({
          enableProlog: true,
          enableDatalog: true,
          enableRdf: true,
          enableShacl: true,
          r5rsEnginePath: null
        });
        this.initialized = true;
        console.log('MetaLogBrowserAdapter initialized with MetaLogDb (fallback)');
      } catch (fallbackError) {
        throw new Error(`Meta-log-db initialization failed: ${error.message}`);
      }
    }
  }

  /**
   * Load canvas from URL (browser-compatible)
   * @param {string} url - URL to CanvasL/JSONL file
   * @returns {Promise<void>}
   */
  async loadCanvas(url) {
    if (!this.metaLogDb) {
      await this.init();
    }

    try {
      // Fetch file content
      const response = await fetch(url);
      const content = await response.text();
      
      // Parse content directly (bypass file system)
      const lines = content.split('\n').filter(line => line.trim());
      const objects = [];

      for (const line of lines) {
        if (line.trim() && line.trim().startsWith('{')) {
          try {
            const obj = JSON.parse(line);
            objects.push(obj);
          } catch (error) {
            console.warn(`Failed to parse line: ${line}`, error);
          }
        }
      }

      // Extract facts and add to engines
      const facts = this.extractFactsFromObjects(objects);
      
      // Add to ProLog engine
      if (this.prologEngine) {
        this.prologEngine.addFacts(facts);
      } else if (this.metaLogDb?.prolog) {
        this.metaLogDb.prolog.addFacts(facts);
      }
      
      // Add to DataLog engine
      if (this.datalogEngine) {
        this.datalogEngine.addFacts(facts);
      } else if (this.metaLogDb?.datalog) {
        this.metaLogDb.datalog.addFacts(facts);
      }
      
      // Add to RDF triple store
      const triples = this.jsonlToRdf(facts);
      if (this.tripleStore) {
        this.tripleStore.addTriples(triples);
      } else if (this.metaLogDb?.rdf) {
        this.metaLogDb.rdf.addTriples(triples);
      }
    } catch (error) {
      throw new Error(`Failed to load canvas from ${url}: ${error.message}`);
    }
  }

  /**
   * Extract facts from parsed objects
   * @param {Array} objects - Parsed CanvasL objects
   * @returns {Array} Facts
   */
  extractFactsFromObjects(objects) {
    const facts = [];
    
    for (const obj of objects) {
      if (obj.type === 'node') {
        facts.push({
          predicate: 'node',
          args: [obj.id, obj.type, obj.x || 0, obj.y || 0, obj.text || '']
        });
      } else if (obj.type === 'edge') {
        facts.push({
          predicate: 'edge',
          args: [obj.id, obj.type, obj.fromNode, obj.toNode]
        });
      }
    }
    
    return facts;
  }

  /**
   * Convert facts to RDF triples
   * @param {Array} facts - Facts array
   * @returns {Array} RDF triples
   */
  jsonlToRdf(facts) {
    const triples = [];
    
    for (const fact of facts) {
      if (fact.predicate === 'node') {
        triples.push({
          subject: fact.args[0],
          predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          object: fact.args[1]
        });
      }
    }
    
    return triples;
  }

  /**
   * Execute ProLog query
   * @param {string} query - ProLog query
   * @returns {Promise<Object>} Query results
   */
  async prologQuery(query) {
    await this.init();
    
    if (this.prologEngine) {
      return await this.prologEngine.query(query);
    } else if (this.metaLogDb) {
      return await this.metaLogDb.prologQuery(query);
    }
    
    throw new Error('ProLog engine not available');
  }

  /**
   * Execute DataLog query
   * @param {string} goal - DataLog goal
   * @param {Object} program - Optional DataLog program
   * @returns {Promise<Object>} Query results
   */
  async datalogQuery(goal, program = null) {
    await this.init();
    
    if (this.datalogEngine) {
      return await this.datalogEngine.query(goal, program);
    } else if (this.metaLogDb) {
      return await this.metaLogDb.datalogQuery(goal, program);
    }
    
    throw new Error('DataLog engine not available');
  }

  /**
   * Execute SPARQL query
   * @param {string} query - SPARQL query
   * @returns {Promise<Object>} Query results
   */
  async sparqlQuery(query) {
    await this.init();
    
    if (this.tripleStore) {
      return await this.tripleStore.sparql(query);
    } else if (this.metaLogDb) {
      return await this.metaLogDb.sparqlQuery(query);
    }
    
    throw new Error('SPARQL engine not available');
  }

  /**
   * Validate with SHACL
   * @param {Object} shapes - SHACL shapes (loaded from string, not file)
   * @param {Array} triples - Optional RDF triples to validate
   * @returns {Promise<Object>} Validation result
   */
  async shaclValidate(shapes, triples = null) {
    await this.init();
    
    if (this.shaclValidator) {
      // Get triples from store if not provided
      const targetTriples = triples || (this.tripleStore ? this.tripleStore.getTriples() : []);
      return await this.shaclValidator.validate(shapes, targetTriples);
    } else if (this.metaLogDb) {
      return await this.metaLogDb.validateShacl(shapes, triples);
    }
    
    throw new Error('SHACL validator not available');
  }

  /**
   * Add ProLog rule
   * @param {string} rule - ProLog rule string
   */
  addPrologRule(rule) {
    if (!this.metaLogDb) {
      throw new Error('MetaLogDb not initialized');
    }
    
    this.metaLogDb.addPrologRule(rule);
  }

  /**
   * Add DataLog rule
   * @param {string} rule - DataLog rule string
   */
  addDatalogRule(rule) {
    if (!this.metaLogDb) {
      throw new Error('MetaLogDb not initialized');
    }
    
    // Parse rule and add via DataLog engine
    const datalogEngine = this.getDatalog();
    if (datalogEngine) {
      const match = rule.match(/^(.+?)\s*:-\s*(.+)$/);
      if (match) {
        const head = match[1].trim();
        const body = match[2].split(',').map(b => b.trim());
        datalogEngine.addRule({ head, body });
      }
    }
  }

  /**
   * Store RDF triples
   * @param {Array} triples - RDF triples array
   */
  storeTriples(triples) {
    if (!this.metaLogDb) {
      throw new Error('MetaLogDb not initialized');
    }
    
    this.metaLogDb.storeTriples(triples);
  }

  /**
   * Get R5RS registry (if available)
   * @returns {Object|null} R5RS registry
   */
  getR5RS() {
    return this.metaLogDb?.r5rs || null;
  }

  /**
   * Get ProLog engine
   * @returns {Object|null} ProLog engine
   */
  getProlog() {
    return this.prologEngine || this.metaLogDb?.prolog || null;
  }

  /**
   * Get DataLog engine
   * @returns {Object|null} DataLog engine
   */
  getDatalog() {
    return this.datalogEngine || this.metaLogDb?.datalog || null;
  }

  /**
   * Get RDF triple store
   * @returns {Object|null} Triple store
   */
  getRdf() {
    return this.tripleStore || this.metaLogDb?.rdf || null;
  }

  /**
   * Get SHACL validator
   * @returns {Object|null} SHACL validator
   */
  getShacl() {
    return this.shaclValidator || this.metaLogDb?.shacl || null;
  }

  /**
   * Clear all data
   */
  clear() {
    if (this.prologEngine) {
      this.prologEngine.clear();
    }
    if (this.datalogEngine) {
      this.datalogEngine.clear();
    }
    if (this.tripleStore) {
      this.tripleStore.clear();
    }
    
    if (this.metaLogDb) {
      if (this.metaLogDb.prolog) {
        this.metaLogDb.prolog.clear();
      }
      if (this.metaLogDb.datalog) {
        this.metaLogDb.datalog.clear();
      }
      if (this.metaLogDb.rdf) {
        this.metaLogDb.rdf.clear();
      }
    }
  }
}
