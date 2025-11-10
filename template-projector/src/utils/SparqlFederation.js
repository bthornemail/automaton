/**
 * SparqlFederation - SPARQL Federation Utilities
 * 
 * Provides utilities for federated SPARQL queries:
 * - SERVICE endpoint resolution
 * - VALUES binding optimization
 * - Agent protection integration
 * - Error recovery for partial failures
 */

export class SparqlFederation {
  constructor(metaLogBridge, errorHandler) {
    this.metaLog = metaLogBridge;
    this.errorHandler = errorHandler;
    this.serviceEndpoints = new Map();
    this.mockEndpoints = new Map();
  }

  /**
   * Register SERVICE endpoint
   * @param {string} uri - Endpoint URI (e.g., 'https://dbpedia.org/sparql')
   * @param {Object} config - Endpoint configuration
   */
  registerEndpoint(uri, config = {}) {
    this.serviceEndpoints.set(uri, {
      url: config.url || uri,
      requiresAuth: config.requiresAuth || false,
      requiresConsent: config.requiresConsent || false,
      timeout: config.timeout || 30000,
      retries: config.retries || 3,
      ...config
    });
  }

  /**
   * Register mock endpoint for testing
   * @param {string} uri - Mock endpoint URI
   * @param {Function} handler - Mock handler function
   */
  registerMockEndpoint(uri, handler) {
    this.mockEndpoints.set(uri, handler);
  }

  /**
   * Parse SPARQL query to extract SERVICE blocks
   * @param {string} query - SPARQL query
   * @returns {Array} SERVICE blocks with endpoints
   */
  parseServiceBlocks(query) {
    const serviceBlocks = [];
    
    // Enhanced regex to handle nested braces and quoted strings
    // Match: SERVICE <endpoint> { ... }
    const serviceRegex = /SERVICE\s+<([^>]+)>\s*\{/gi;
    let lastIndex = 0;
    let match;

    while ((match = serviceRegex.exec(query)) !== null) {
      const startPos = match.index;
      const endpoint = match[1];
      
      // Find matching closing brace (handles nested braces)
      let braceCount = 1;
      let pos = match.index + match[0].length;
      let inString = false;
      let stringChar = null;
      
      while (pos < query.length && braceCount > 0) {
        const char = query[pos];
        
        // Handle string literals
        if ((char === '"' || char === "'") && (pos === 0 || query[pos - 1] !== '\\')) {
          if (!inString) {
            inString = true;
            stringChar = char;
          } else if (char === stringChar) {
            inString = false;
            stringChar = null;
          }
        }
        
        // Count braces only when not in string
        if (!inString) {
          if (char === '{') braceCount++;
          if (char === '}') braceCount--;
        }
        
        pos++;
      }
      
      if (braceCount === 0) {
        const endPos = pos;
        const serviceQuery = query.substring(match.index + match[0].length, endPos - 1).trim();
        
        serviceBlocks.push({
          endpoint,
          query: serviceQuery,
          fullMatch: query.substring(startPos, endPos),
          startPos,
          endPos
        });
      }
    }

    return serviceBlocks;
  }

  /**
   * Extract VALUES bindings from query
   * @param {string} query - SPARQL query
   * @returns {Object} VALUES bindings map
   */
  extractValuesBindings(query) {
    const bindings = {};
    
    // Enhanced regex to handle VALUES with multiple variables and quoted strings
    // Match: VALUES ?var { value1 value2 ... }
    // Or: VALUES (?var1 ?var2) { (val1 val2) (val3 val4) }
    const valuesRegex = /VALUES\s+(?:\(([^)]+)\)|(\?[\w]+))\s*\{([^}]+)\}/gi;
    let match;

    while ((match = valuesRegex.exec(query)) !== null) {
      const multiVar = match[1]; // Multiple variables: (?var1 ?var2)
      const singleVar = match[2]; // Single variable: ?var
      const valuesStr = match[3];
      
      if (multiVar) {
        // Multiple variables: VALUES (?var1 ?var2) { (val1 val2) (val3 val4) }
        const vars = multiVar.split(/\s+/).map(v => v.trim()).filter(v => v);
        const tuples = this.parseValueTuples(valuesStr);
        
        vars.forEach((varName, index) => {
          if (!bindings[varName]) {
            bindings[varName] = [];
          }
          tuples.forEach(tuple => {
            if (tuple[index]) {
              bindings[varName].push(tuple[index]);
            }
          });
        });
      } else if (singleVar) {
        // Single variable: VALUES ?var { val1 val2 ... }
        const values = this.parseValueList(valuesStr);
        bindings[singleVar] = values;
      }
    }

    return bindings;
  }

  /**
   * Parse VALUES list (single variable)
   * @param {string} valuesStr - Values string
   * @returns {Array} Parsed values
   */
  parseValueList(valuesStr) {
    const values = [];
    let current = '';
    let inQuotes = false;
    let quoteChar = null;
    
    for (let i = 0; i < valuesStr.length; i++) {
      const char = valuesStr[i];
      
      if ((char === '"' || char === "'") && (i === 0 || valuesStr[i - 1] !== '\\')) {
        if (!inQuotes) {
          inQuotes = true;
          quoteChar = char;
        } else if (char === quoteChar) {
          inQuotes = false;
          quoteChar = null;
        }
        current += char;
      } else if (!inQuotes && /\s/.test(char)) {
        if (current.trim()) {
          values.push(current.trim().replace(/^["']|["']$/g, ''));
          current = '';
        }
      } else {
        current += char;
      }
    }
    
    if (current.trim()) {
      values.push(current.trim().replace(/^["']|["']$/g, ''));
    }
    
    return values.filter(v => v);
  }

  /**
   * Parse VALUES tuples (multiple variables)
   * @param {string} valuesStr - Values string
   * @returns {Array} Array of tuples
   */
  parseValueTuples(valuesStr) {
    const tuples = [];
    let currentTuple = [];
    let current = '';
    let depth = 0;
    let inQuotes = false;
    let quoteChar = null;
    
    for (let i = 0; i < valuesStr.length; i++) {
      const char = valuesStr[i];
      
      if ((char === '"' || char === "'") && (i === 0 || valuesStr[i - 1] !== '\\')) {
        if (!inQuotes) {
          inQuotes = true;
          quoteChar = char;
        } else if (char === quoteChar) {
          inQuotes = false;
          quoteChar = null;
        }
        current += char;
      } else if (!inQuotes && char === '(') {
        if (depth === 0 && current.trim()) {
          // Start new tuple
          currentTuple = [];
        }
        depth++;
        current += char;
      } else if (!inQuotes && char === ')') {
        depth--;
        current += char;
        if (depth === 0) {
          // End tuple
          const tupleValues = current.slice(1, -1).split(/\s+/)
            .map(v => v.trim().replace(/^["']|["']$/g, ''))
            .filter(v => v);
          tuples.push(tupleValues);
          current = '';
        }
      } else if (!inQuotes && depth === 0 && /\s/.test(char)) {
        if (current.trim()) {
          currentTuple.push(current.trim().replace(/^["']|["']$/g, ''));
          current = '';
        }
      } else {
        current += char;
      }
    }
    
    if (current.trim() && depth === 0) {
      currentTuple.push(current.trim().replace(/^["']|["']$/g, ''));
    }
    
    if (currentTuple.length > 0) {
      tuples.push(currentTuple);
    }
    
    return tuples;
  }

  /**
   * Check agent consent for endpoint
   * @param {string} endpoint - Endpoint URI
   * @param {string} query - Query string
   * @returns {Promise<boolean>} Consent status
   */
  async checkAgentConsent(endpoint, query) {
    // Check if endpoint requires consent
    const endpointConfig = this.serviceEndpoints.get(endpoint);
    if (!endpointConfig || !endpointConfig.requiresConsent) {
      return true; // Public endpoint, no consent needed
    }

    // Query ProLog for consent
    try {
      // Try different consent query formats
      const consentQueries = [
        `consent(user, ${endpoint}, true)`,
        `consent("user", "${endpoint}", true)`,
        `consent(user, '${endpoint}', true)`
      ];

      for (const consentQuery of consentQueries) {
        try {
          const result = await this.metaLog.prologQuery(consentQuery);
          if (result && result.length > 0) {
            return true;
          }
        } catch (queryError) {
          // Try next format
          continue;
        }
      }

      return false; // No consent found
    } catch (error) {
      console.warn('Consent check failed:', error);
      return false; // Deny by default
    }
  }

  /**
   * Execute federated SPARQL query
   * @param {string} query - SPARQL query with SERVICE blocks
   * @param {Object} options - Query options
   * @returns {Promise<Object>} Query results
   */
  async executeFederatedQuery(query, options = {}) {
    const serviceBlocks = this.parseServiceBlocks(query);
    const valuesBindings = this.extractValuesBindings(query);

    if (serviceBlocks.length === 0) {
      // No SERVICE blocks, execute as normal query
      return await this.metaLog.sparqlQuery(query, options.endpoint);
    }

    // Execute each SERVICE block
    const serviceResults = [];
    const errors = [];

    for (const block of serviceBlocks) {
      try {
        // Check agent consent
        const hasConsent = await this.checkAgentConsent(block.endpoint, block.query);
        if (!hasConsent) {
          errors.push({
            endpoint: block.endpoint,
            error: 'Agent consent denied',
            type: 'permission'
          });
          continue;
        }

        // Check for mock endpoint
        if (this.mockEndpoints.has(block.endpoint)) {
          const mockHandler = this.mockEndpoints.get(block.endpoint);
          const mockResult = await mockHandler(block.query, valuesBindings);
          serviceResults.push({
            endpoint: block.endpoint,
            result: mockResult,
            success: true
          });
          continue;
        }

        // Execute real SERVICE query
        const endpointConfig = this.serviceEndpoints.get(block.endpoint);
        const endpointUrl = endpointConfig?.url || block.endpoint;

        // Build SERVICE query with VALUES bindings
        const serviceQuery = this.buildServiceQuery(block.query, valuesBindings);

        const result = await this.metaLog.sparqlQuery(serviceQuery, endpointUrl);
        serviceResults.push({
          endpoint: block.endpoint,
          result,
          success: true
        });
      } catch (error) {
        errors.push({
          endpoint: block.endpoint,
          error: error.message,
          type: this.classifyError(error)
        });

        // Try error recovery
        if (options.recoverPartial) {
          const recovery = await this.errorHandler.handle(error, {
            context: 'federated_query',
            endpoint: block.endpoint,
            retry: () => this.executeServiceBlock(block, valuesBindings)
          });

          if (recovery.recovered) {
            serviceResults.push({
              endpoint: block.endpoint,
              result: recovery.recovery,
              success: true,
              recovered: true
            });
            errors.pop(); // Remove from errors since recovered
          }
        }
      }
    }

    // Join results from all SERVICE blocks
    const joinedResults = this.joinServiceResults(serviceResults, query);

    return {
      results: {
        bindings: joinedResults
      },
      errors: errors.length > 0 ? errors : undefined,
      partial: errors.length > 0 && serviceResults.length > 0
    };
  }

  /**
   * Build SERVICE query with VALUES bindings
   * @param {string} serviceQuery - Query inside SERVICE block
   * @param {Object} valuesBindings - VALUES bindings
   * @returns {string} Complete query (optimized)
   */
  buildServiceQuery(serviceQuery, valuesBindings) {
    let query = serviceQuery;

    // Optimize: Only add VALUES for variables actually used in query
    const usedVariables = this.extractVariables(serviceQuery);
    const valuesClauses = [];
    
    for (const [variable, values] of Object.entries(valuesBindings)) {
      if (usedVariables.has(variable) && values.length > 0) {
        // Format VALUES clause efficiently
        const formattedValues = values.map(v => {
          // Keep quotes if present, otherwise add quotes for strings
          if (/^["']/.test(v)) {
            return v;
          }
          // Check if it's a number or URI
          if (/^\d+$/.test(v) || /^<.*>$/.test(v)) {
            return v;
          }
          return `"${v}"`;
        }).join(' ');
        
        valuesClauses.push(`VALUES ${variable} { ${formattedValues} }`);
      }
    }

    // Prepend VALUES clauses before the query for optimal binding
    if (valuesClauses.length > 0) {
      query = valuesClauses.join('\n') + '\n' + query;
    }

    return query;
  }

  /**
   * Extract variables used in query
   * @param {string} query - SPARQL query
   * @returns {Set} Set of variable names
   */
  extractVariables(query) {
    const variables = new Set();
    const varRegex = /\?[\w]+/g;
    let match;
    
    while ((match = varRegex.exec(query)) !== null) {
      variables.add(match[0]);
    }
    
    return variables;
  }

  /**
   * Join results from multiple SERVICE blocks
   * @param {Array} serviceResults - Results from each SERVICE block
   * @param {string} originalQuery - Original SPARQL query
   * @returns {Array} Joined bindings
   */
  joinServiceResults(serviceResults, originalQuery) {
    if (serviceResults.length === 0) {
      return [];
    }

    if (serviceResults.length === 1) {
      return serviceResults[0].result.results?.bindings || [];
    }

    // Optimized join: For SELECT queries, perform natural join on common variables
    // For now, use simple combination with deduplication
    const allBindings = [];
    
    // Collect all bindings
    for (const serviceResult of serviceResults) {
      const bindings = serviceResult.result.results?.bindings || [];
      allBindings.push(...bindings);
    }

    // Deduplicate bindings efficiently
    const seen = new Set();
    const uniqueBindings = [];
    
    for (const binding of allBindings) {
      // Create normalized key for comparison
      const key = this.createBindingKey(binding);
      if (!seen.has(key)) {
        seen.add(key);
        uniqueBindings.push(binding);
      }
    }

    return uniqueBindings;
  }

  /**
   * Create normalized key for binding comparison
   * @param {Object} binding - SPARQL binding
   * @returns {string} Normalized key
   */
  createBindingKey(binding) {
    const keys = Object.keys(binding).sort();
    return keys.map(key => {
      const value = binding[key];
      const valueStr = typeof value === 'object' && value.value ? value.value : value;
      return `${key}:${valueStr}`;
    }).join('|');
  }

  /**
   * Execute single SERVICE block
   * @param {Object} block - SERVICE block
   * @param {Object} valuesBindings - VALUES bindings
   * @returns {Promise<Object>} Query result
   */
  async executeServiceBlock(block, valuesBindings) {
    const endpointConfig = this.serviceEndpoints.get(block.endpoint);
    const endpointUrl = endpointConfig?.url || block.endpoint;
    const serviceQuery = this.buildServiceQuery(block.query, valuesBindings);
    return await this.metaLog.sparqlQuery(serviceQuery, endpointUrl);
  }

  /**
   * Classify error type
   * @param {Error} error - Error object
   * @returns {string} Error type
   */
  classifyError(error) {
    const message = error.message.toLowerCase();
    if (message.includes('network') || message.includes('fetch') || message.includes('timeout')) {
      return 'network';
    }
    if (message.includes('429') || message.includes('rate limit')) {
      return 'ratelimit';
    }
    if (message.includes('403') || message.includes('permission') || message.includes('forbidden')) {
      return 'permission';
    }
    return 'unknown';
  }
}
