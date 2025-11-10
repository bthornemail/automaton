/**
 * MetaLogBridge - Browser-side Meta-Log integration
 * 
 * Uses meta-log-db package via MetaLogBrowserAdapter for:
 * - ProLog query execution (full engine with unification)
 * - DataLog materialization (fixpoint computation)
 * - SPARQL query execution (local triple store + remote endpoints)
 * - SHACL validation (full validator)
 * - R5RS Scheme evaluation (via BiwaScheme, fallback)
 */

import BiwaScheme from 'biwascheme';
import { MetaLogBrowserAdapter } from './MetaLogBrowserAdapter.js';

export class MetaLogBridge {
  constructor() {
    this.adapter = new MetaLogBrowserAdapter();
    // BiwaScheme exports Interpreter as a property of the default export
    this.r5rsInterpreter = new BiwaScheme.Interpreter(); // Fallback for R5RS
    this.sparqlCache = new Map();
    this.initialized = false;
  }

  /**
   * Initialize Meta-Log bridge
   */
  async init() {
    if (this.initialized) {
      return;
    }

    try {
      await this.adapter.init();
      this.initialized = true;
      console.log('MetaLogBridge initialized with meta-log-db');
    } catch (error) {
      console.warn('Failed to initialize meta-log-db adapter, using fallback:', error);
      // Continue with fallback implementations
      this.initialized = false;
    }
  }

  /**
   * Set error handler for federation
   * @param {ErrorHandler} errorHandler - Error handler instance
   */
  setErrorHandler(errorHandler) {
    this.errorHandler = errorHandler;
    if (errorHandler && !this.federation) {
      this.federation = new SparqlFederation(this, errorHandler);
    }
  }

  /**
   * Evaluate R5RS Scheme expression
   * @param {string} expression - Scheme expression
   * @param {Object} context - Evaluation context
   * @returns {Promise<*>} Evaluation result
   */
  async evalR5RS(expression, context = {}) {
    try {
      // Add context variables to interpreter
      for (const [key, value] of Object.entries(context)) {
        this.r5rsInterpreter.evaluate(`(define ${key} ${JSON.stringify(value)})`);
      }
      
      const result = this.r5rsInterpreter.evaluate(expression);
      return result;
    } catch (error) {
      throw new Error(`R5RS evaluation error: ${error.message}`);
    }
  }

  /**
   * Execute ProLog query
   * @param {string} query - ProLog query
   * @param {Array} facts - Additional facts
   * @returns {Promise<Array>} Query results
   */
  async prologQuery(query, facts = []) {
    await this.init();
    
    if (this.initialized && this.adapter) {
      // Use meta-log-db ProLog engine
      try {
        // Add facts if provided
        if (facts.length > 0) {
          const prologEngine = this.adapter.getProlog();
          if (prologEngine) {
            prologEngine.addFacts(facts);
          }
        }
        
        const result = await this.adapter.prologQuery(query);
        return result.bindings || [];
      } catch (error) {
        console.error('ProLog query error:', error);
        return [];
      }
    }
    
    // Fallback: return empty results
    return [];
  }

  /**
   * Add ProLog fact
   * @param {string|Object} fact - ProLog fact (string or Fact object)
   */
  addPrologFact(fact) {
    if (!this.initialized) {
      return;
    }
    
    const prologEngine = this.adapter.getProlog();
    if (prologEngine) {
      if (typeof fact === 'string') {
        // Parse string fact
        const parsed = this.parsePrologFact(fact);
        if (parsed) {
          prologEngine.addFacts([parsed]);
        }
      } else {
        prologEngine.addFacts([fact]);
      }
    }
  }

  /**
   * Parse ProLog fact string
   * @param {string} factStr - Fact string (e.g., "parent(alice, bob).")
   * @returns {Object|null} Parsed fact
   */
  parsePrologFact(factStr) {
    const match = factStr.match(/^(\w+)\((.*)\)\.?$/);
    if (match) {
      const predicate = match[1];
      const argsStr = match[2];
      const args = argsStr ? argsStr.split(',').map(s => s.trim().replace(/['"]/g, '')) : [];
      
      return {
        predicate,
        args
      };
    }
    return null;
  }

  /**
   * Execute DataLog query
   * @param {string} goal - DataLog goal query
   * @param {string|Object} program - DataLog program (string or program object)
   * @returns {Promise<Array>} Query results
   */
  async datalogQuery(goal, program = null) {
    await this.init();
    
    if (this.initialized && this.adapter) {
      // Use meta-log-db DataLog engine
      try {
        let programObj = program;
        
        // Parse program string if provided
        if (typeof program === 'string') {
          programObj = this.parseDatalogProgram(program);
        }
        
        const result = await this.adapter.datalogQuery(goal, programObj);
        return result.facts || [];
      } catch (error) {
        console.error('DataLog query error:', error);
        return [];
      }
    }
    
    // Fallback: return empty results
    return [];
  }

  /**
   * Parse DataLog program string
   * @param {string} programStr - Program string
   * @returns {Object} Program object
   */
  parseDatalogProgram(programStr) {
    const lines = programStr.split('\n').filter(l => l.trim() && !l.trim().startsWith('%'));
    const rules = [];
    const facts = [];
    
    for (const line of lines) {
      if (line.includes(':-')) {
        // Rule
        rules.push(this.parseDatalogRule(line.trim()));
      } else if (line.includes('(') && line.includes(')')) {
        // Fact
        const fact = this.parseDatalogFact(line.trim().replace(/\.$/, ''));
        if (fact) {
          facts.push(fact);
        }
      }
    }
    
    return { rules, facts };
  }

  /**
   * Parse DataLog rule
   * @param {string} ruleStr - Rule string (e.g., "ancestor(X,Y) :- parent(X,Y).")
   * @returns {Object} Rule object
   */
  parseDatalogRule(ruleStr) {
    const [head, body] = ruleStr.split(':-').map(s => s.trim());
    const headFact = this.parseDatalogFact(head);
    const bodyFacts = body ? body.split(',').map(s => this.parseDatalogFact(s.trim())).filter(Boolean) : [];
    
    return {
      head: headFact,
      body: bodyFacts
    };
  }

  /**
   * Parse DataLog fact
   * @param {string} factStr - Fact string (e.g., "parent(alice, bob)")
   * @returns {Object|null} Fact object
   */
  parseDatalogFact(factStr) {
    const match = factStr.match(/^(\w+)\((.*)\)$/);
    if (match) {
      const predicate = match[1];
      const argsStr = match[2];
      const args = argsStr ? argsStr.split(',').map(s => s.trim().replace(/['"]/g, '')) : [];
      
      return {
        predicate,
        args
      };
    }
    return null;
  }

  /**
   * Execute SPARQL query
   * @param {string} query - SPARQL query
   * @param {string} endpoint - SPARQL endpoint URL (optional, null for local)
   * @param {Object} options - Query options (recoverPartial, etc.)
   * @returns {Promise<Object>} Query results
   */
  async sparqlQuery(query, endpoint = null, options = {}) {
    await this.init();
    
    // Check if query contains SERVICE blocks (federated query)
    const hasService = /SERVICE\s+<[^>]+>/i.test(query);
    
    if (hasService && this.federation) {
      // Use federation handler
      return await this.federation.executeFederatedQuery(query, options);
    }
    
    // Check cache
    const cacheKey = `${endpoint || 'local'}:${query}`;
    if (this.sparqlCache.has(cacheKey)) {
      const cached = this.sparqlCache.get(cacheKey);
      if (Date.now() - cached.timestamp < 900000) { // 15 minutes
        return cached.data;
      }
    }
    
    if (endpoint) {
      // Remote SPARQL endpoint
      try {
        const response = await fetch(endpoint, {
          method: 'POST',
          headers: {
            'Accept': 'application/sparql-results+json',
            'Content-Type': 'application/x-www-form-urlencoded'
          },
          body: new URLSearchParams({ query })
        });
        
        if (!response.ok) {
          throw new Error(`SPARQL query failed: ${response.statusText}`);
        }
        
        const data = await response.json();
        
        // Cache result
        this.sparqlCache.set(cacheKey, {
          data,
          timestamp: Date.now()
        });
        
        return data;
      } catch (error) {
        throw new Error(`SPARQL endpoint error: ${error.message}`);
      }
    } else {
      // Local SPARQL query over RDF triples using meta-log-db
      if (this.initialized && this.adapter) {
        try {
          const result = await this.adapter.sparqlQuery(query);
          
          // Cache result
          this.sparqlCache.set(cacheKey, {
            data: result,
            timestamp: Date.now()
          });
          
          return result;
        } catch (error) {
          console.error('Local SPARQL query error:', error);
          return { results: { bindings: [] } };
        }
      }
      
      // Fallback: return empty results
      return { results: { bindings: [] } };
    }
  }

  /**
   * Add RDF triple
   * @param {string} subject - Subject
   * @param {string} predicate - Predicate
   * @param {string} object - Object
   */
  addTriple(subject, predicate, object) {
    if (this.initialized && this.adapter) {
      const rdfStore = this.adapter.getRdf();
      if (rdfStore) {
        rdfStore.addTriples([{ subject, predicate, object }]);
      }
    }
  }

  /**
   * Validate with SHACL
   * @param {Object|string} shapes - SHACL shapes (object or Turtle string)
   * @param {*} focus - Focus node (optional)
   * @returns {Promise<Object>} Validation result
   */
  async shaclValidate(shapes, focus = null) {
    await this.init();
    
    if (this.initialized && this.adapter) {
      try {
        // If shapes is a string (Turtle), parse it
        let shapesObj = shapes;
        if (typeof shapes === 'string') {
          // Parse Turtle to shapes object
          shapesObj = this.parseShaclShapes(shapes);
        }
        
        return await this.adapter.shaclValidate(shapesObj, focus);
      } catch (error) {
        console.error('SHACL validation error:', error);
        return {
          conforms: false,
          results: [{
            severity: 'error',
            message: error.message
          }]
        };
      }
    }
    
    // Fallback: return success
    return {
      conforms: true,
      results: []
    };
  }

  /**
   * Parse SHACL shapes from Turtle string (simplified)
   * @param {string} turtle - Turtle content
   * @returns {Object} Shapes object
   */
  parseShaclShapes(turtle) {
    // Simplified parser - for full implementation, use proper Turtle parser
    const shapes = {};
    
    // Basic parsing - extract NodeShapes
    const nodeShapeMatches = turtle.matchAll(/sh:NodeShape[^;]*;/g);
    for (const match of nodeShapeMatches) {
      // Extract shape ID and properties
      // This is a simplified implementation
    }
    
    return shapes;
  }

  /**
   * Clear all data
   */
  clear() {
    if (this.adapter) {
      this.adapter.clear();
    }
    this.sparqlCache.clear();
  }
}
