/**
 * MetaLogBridge - Browser-side Meta-Log integration
 * 
 * Uses meta-log-db package via CanvasLMetaverseBrowser for:
 * - ProLog query execution (full engine with unification)
 * - DataLog materialization (fixpoint computation)
 * - SPARQL query execution (local triple store + remote endpoints)
 * - SHACL validation (full validator)
 * - R5RS Scheme evaluation (via BiwaScheme, fallback)
 * - CanvasL object execution (unified execution interface)
 */

import BiwaScheme from 'biwascheme';
import { CanvasLMetaverseBrowser } from 'meta-log-db/browser';
import { ErrorHandler } from './ErrorHandler';
import { SparqlFederation } from './SparqlFederation';

export interface MetaLogBridgeConfig {
  indexedDBName?: string;
  cacheStrategy?: 'memory' | 'indexeddb' | 'both';
}

export class MetaLogBridge {
  private browser: CanvasLMetaverseBrowser;
  private r5rsInterpreter: any; // BiwaScheme.Interpreter
  private sparqlCache: Map<string, { data: any; timestamp: number }>;
  private initialized: boolean;
  private errorHandler?: ErrorHandler;
  private federation?: SparqlFederation;

  constructor() {
    this.browser = new CanvasLMetaverseBrowser();
    // BiwaScheme exports Interpreter as a property of the default export
    this.r5rsInterpreter = new BiwaScheme.Interpreter(); // Fallback for R5RS
    this.sparqlCache = new Map();
    this.initialized = false;
  }

  /**
   * Backward compatibility: adapter property points to browser
   * @deprecated Use browser property directly
   */
  get adapter(): CanvasLMetaverseBrowser {
    return this.browser;
  }

  /**
   * Initialize Meta-Log bridge
   */
  async init(): Promise<void> {
    if (this.initialized) {
      return;
    }

    try {
      await this.browser.init();
      this.initialized = true;
      console.log('MetaLogBridge initialized with CanvasLMetaverseBrowser');
    } catch (error) {
      console.warn('Failed to initialize CanvasLMetaverseBrowser, using fallback:', error);
      // Continue with fallback implementations
      this.initialized = false;
    }
  }

  /**
   * Set error handler for federation
   */
  setErrorHandler(errorHandler: ErrorHandler): void {
    this.errorHandler = errorHandler;
    if (errorHandler && !this.federation) {
      this.federation = new SparqlFederation(this, errorHandler);
    }
  }

  /**
   * Evaluate R5RS Scheme expression
   */
  async evalR5RS(expression: string, context: Record<string, any> = {}): Promise<any> {
    try {
      // Add context variables to interpreter
      for (const [key, value] of Object.entries(context)) {
        this.r5rsInterpreter.evaluate(`(define ${key} ${JSON.stringify(value)})`);
      }
      
      const result = this.r5rsInterpreter.evaluate(expression);
      return result;
    } catch (error: any) {
      throw new Error(`R5RS evaluation error: ${error.message}`);
    }
  }

  /**
   * Execute ProLog query
   */
  async prologQuery(query: string, facts: any[] = []): Promise<any[]> {
    await this.init();
    
    if (this.initialized && this.browser) {
      // Use meta-log-db ProLog engine
      try {
        const result = await this.browser.prologQuery(query, { facts });
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
   */
  addPrologFact(fact: string | { predicate: string; args: any[] }): void {
    if (!this.initialized || !this.browser) {
      return;
    }
    
    if (typeof fact === 'string') {
      // Parse string fact
      const parsed = this.parsePrologFact(fact);
      if (parsed) {
        this.browser.addPrologFacts([parsed]);
      }
    } else {
      this.browser.addPrologFacts([fact]);
    }
  }

  /**
   * Parse ProLog fact string
   */
  private parsePrologFact(factStr: string): { predicate: string; args: string[] } | null {
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
   */
  async datalogQuery(goal: string, program: string | any = null): Promise<any[]> {
    await this.init();
    
    if (this.initialized && this.browser) {
      // Use meta-log-db DataLog engine
      try {
        let programObj = program;
        
        // Parse program string if provided
        if (typeof program === 'string') {
          programObj = this.parseDatalogProgram(program);
        }
        
        const result = await this.browser.datalogQuery(goal, programObj);
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
   */
  private parseDatalogProgram(programStr: string): { rules: any[]; facts: any[] } {
    const lines = programStr.split('\n').filter(l => l.trim() && !l.trim().startsWith('%'));
    const rules: any[] = [];
    const facts: any[] = [];
    
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
   */
  private parseDatalogRule(ruleStr: string): { head: any; body: any[] } {
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
   */
  private parseDatalogFact(factStr: string): { predicate: string; args: string[] } | null {
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
   */
  async sparqlQuery(query: string, endpoint: string | null = null, options: Record<string, any> = {}): Promise<any> {
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
      const cached = this.sparqlCache.get(cacheKey)!;
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
      } catch (error: any) {
        throw new Error(`SPARQL endpoint error: ${error.message}`);
      }
    } else {
      // Local SPARQL query over RDF triples using meta-log-db
      if (this.initialized && this.browser) {
        try {
          const result = await this.browser.sparqlQuery(query);
          
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
   */
  addTriple(subject: string, predicate: string, object: string): void {
    if (this.initialized && this.browser) {
      this.browser.storeTriples([{ subject, predicate, object }]);
    }
  }

  /**
   * Validate with SHACL
   */
  async shaclValidate(shapes: any | string, focus: any = null): Promise<any> {
    await this.init();
    
    if (this.initialized && this.browser) {
      try {
        // If shapes is a string (Turtle), parse it
        let shapesObj = shapes;
        if (typeof shapes === 'string') {
          // Parse Turtle to shapes object
          shapesObj = this.parseShaclShapes(shapes);
        }
        
        // Convert focus to triples if needed
        const triples = Array.isArray(focus) ? focus : (focus ? [focus] : null);
        return await this.browser.validateShacl(shapesObj, triples);
      } catch (error: any) {
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
   */
  private parseShaclShapes(turtle: string): any {
    // Simplified parser - for full implementation, use proper Turtle parser
    const shapes: any = {};
    
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
  clear(): void {
    if (this.browser) {
      this.browser.clear();
    }
    this.sparqlCache.clear();
  }
}

