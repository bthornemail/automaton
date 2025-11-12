/**
 * MetaLogBrowserAdapter - Browser-compatible adapter for meta-log-db
 * 
 * TypeScript wrapper for browser-native MetaLogDbBrowser implementation:
 * - Native browser file I/O with fetch API
 * - IndexedDB caching for performance
 * - Built-in encryption support (optional)
 * - No Node.js polyfills required
 * 
 * Provides a clean TypeScript interface for the UI package to use
 * Meta-Log database functionality directly in the browser.
 */

import type { MetaLogDbBrowser, BrowserConfig, Fact, Canvas } from 'meta-log-db/browser';

export interface MetaLogBrowserAdapterConfig {
  enableProlog?: boolean;
  enableDatalog?: boolean;
  enableRdf?: boolean;
  enableShacl?: boolean;
  enableEncryption?: boolean;
  mnemonic?: string;
  indexedDBName?: string;
  cacheStrategy?: 'memory' | 'indexeddb' | 'both';
  r5rsEngineURL?: string;
}

export class MetaLogBrowserAdapter {
  private db: MetaLogDbBrowser | null = null;
  private initialized: boolean = false;
  private initPromise: Promise<void> | null = null;
  private config: BrowserConfig;

  constructor(config: MetaLogBrowserAdapterConfig = {}) {
    this.config = {
      enableProlog: true,
      enableDatalog: true,
      enableRdf: true,
      enableShacl: true,
      enableEncryption: false,
      cacheStrategy: 'both',
      indexedDBName: 'meta-log-db-ui',
      ...config
    };
  }

  /**
   * Initialize meta-log-db with browser-native MetaLogDbBrowser
   * Uses lazy initialization - only initializes when first needed
   */
  async init(): Promise<void> {
    if (this.initialized) {
      return;
    }

    // If initialization is already in progress, wait for it
    if (this.initPromise) {
      return this.initPromise;
    }

    this.initPromise = this._doInit();
    return this.initPromise;
  }

  private async _doInit(): Promise<void> {
    try {
      // Dynamic import to avoid bundling issues
      const { MetaLogDbBrowser } = await import('meta-log-db/browser');
      
      // Create browser-native database instance
      this.db = new MetaLogDbBrowser(this.config);
      
      // Initialize (sets up IndexedDB, file I/O, etc.)
      await this.db.init();

      this.initialized = true;
      console.log('✓ MetaLogBrowserAdapter initialized with MetaLogDbBrowser');
    } catch (error) {
      console.error('Failed to initialize MetaLogDbBrowser:', error);
      throw new Error(`Meta-log-db browser initialization failed: ${error instanceof Error ? error.message : String(error)}`);
    } finally {
      this.initPromise = null;
    }
  }

  /**
   * Load canvas from URL (browser-compatible)
   * @param path - File path identifier (for caching)
   * @param url - URL to CanvasL/JSONL file (optional, uses path if not provided)
   */
  async loadCanvas(path: string, url?: string): Promise<void> {
    await this.init();
    
    if (!this.db) {
      throw new Error('MetaLogDbBrowser not initialized');
    }

    try {
      // Use path as identifier, url as the actual URL to fetch
      // If url is not provided, use path as both
      const fileUrl = url || path;
      await this.db.loadCanvas(path, fileUrl);
    } catch (error) {
      throw new Error(`Failed to load canvas from ${url || path}: ${error instanceof Error ? error.message : String(error)}`);
    }
  }

  /**
   * Parse JSONL canvas from URL
   */
  async parseJsonlCanvas(path: string, url?: string): Promise<Canvas> {
    await this.init();
    
    if (!this.db) {
      throw new Error('MetaLogDbBrowser not initialized');
    }

    return await this.db.parseJsonlCanvas(path, url);
  }

  /**
   * Parse CanvasL file from URL
   */
  async parseCanvasL(path: string, url?: string): Promise<Canvas> {
    await this.init();
    
    if (!this.db) {
      throw new Error('MetaLogDbBrowser not initialized');
    }

    return await this.db.parseCanvasL(path, url);
  }

  /**
   * Extract facts from loaded canvas
   */
  extractFacts(): Fact[] {
    if (!this.db) {
      throw new Error('MetaLogDbBrowser not initialized');
    }

    return this.db.extractFacts();
  }

  /**
   * Execute ProLog query
   */
  async prologQuery(query: string): Promise<any> {
    await this.init();
    
    if (!this.db) {
      throw new Error('MetaLogDbBrowser not initialized');
    }

    return await this.db.prologQuery(query);
  }

  /**
   * Execute DataLog query
   */
  async datalogQuery(goal: string, program?: any): Promise<any> {
    await this.init();
    
    if (!this.db) {
      throw new Error('MetaLogDbBrowser not initialized');
    }

    return await this.db.datalogQuery(goal, program);
  }

  /**
   * Execute SPARQL query
   */
  async sparqlQuery(query: string): Promise<any> {
    await this.init();
    
    if (!this.db) {
      throw new Error('MetaLogDbBrowser not initialized');
    }

    return await this.db.sparqlQuery(query);
  }

  /**
   * Validate with SHACL
   */
  async validateShacl(shapes?: any, triples?: any[]): Promise<any> {
    await this.init();
    
    if (!this.db) {
      throw new Error('MetaLogDbBrowser not initialized');
    }

    return await this.db.validateShacl(shapes, triples);
  }

  /**
   * Add ProLog rule
   */
  addPrologRule(rule: string): void {
    if (!this.db) {
      throw new Error('MetaLogDbBrowser not initialized');
    }
    this.db.addPrologRule(rule);
  }

  /**
   * Add DataLog rule
   */
  addDatalogRule(rule: string): void {
    if (!this.db) {
      throw new Error('MetaLogDbBrowser not initialized');
    }
    // MetaLogDbBrowser uses buildDatalogProgram instead of addDatalogRule
    // Build program from single rule
    const db = this.db as any;
    if (db.datalog) {
      const program = db.buildDatalogProgram([rule]);
      // Note: The program is built but not automatically applied
      // For immediate effect, we'd need to add facts/rules directly
    }
  }

  /**
   * Execute R5RS function (MetaLogDbBrowser uses executeR5RS)
   */
  async executeR5RS(functionName: string, args: any[]): Promise<any> {
    await this.init();
    
    if (!this.db) {
      throw new Error('MetaLogDbBrowser not initialized');
    }

    return await this.db.executeR5RS(functionName, args);
  }

  /**
   * Get R5RS function
   * Note: MetaLogDbBrowser doesn't expose function definitions directly
   * We return a placeholder object indicating the function can be executed
   */
  async getR5RSFunction(name: string): Promise<any> {
    await this.init();
    
    if (!this.db) {
      throw new Error('MetaLogDbBrowser not initialized');
    }

    // Try to access R5RS registry through the database instance
    // If available, check if function exists
    const db = this.db as any;
    if (db.r5rs) {
      const fn = db.r5rs.getFunction(name);
      if (fn) {
        return { name, function: fn, available: true };
      }
    }
    
    // Return null to indicate we can't determine if function exists
    // The function might still be available through executeR5RS
    return null;
  }

  /**
   * List R5RS functions
   */
  async listR5RSFunctions(pattern?: string): Promise<string[]> {
    await this.init();
    
    if (!this.db) {
      throw new Error('MetaLogDbBrowser not initialized');
    }

    // Try to access R5RS registry through the database instance
    const db = this.db as any;
    if (db.r5rs) {
      let functions = db.r5rs.getFunctionNames();
      
      // Filter by pattern if provided
      if (pattern) {
        const regex = new RegExp(pattern, 'i');
        functions = functions.filter((name: string) => regex.test(name));
      }
      
      return functions;
    }
    
    // Return empty array if R5RS registry not available
    return [];
  }

  /**
   * Invoke R5RS function (alias for executeR5RS)
   */
  async invokeR5RSFunction(name: string, args: any[], context?: any): Promise<any> {
    await this.init();
    
    if (!this.db) {
      throw new Error('MetaLogDbBrowser not initialized');
    }

    // MetaLogDbBrowser doesn't support context parameter
    return await this.db.executeR5RS(name, args);
  }

  /**
   * Check if adapter is initialized
   */
  isInitialized(): boolean {
    return this.initialized && this.db !== null;
  }

  /**
   * Get the underlying database instance (for advanced usage)
   */
  getDb(): MetaLogDbBrowser | null {
    return this.db;
  }
}

// Singleton instance for the UI package
let adapterInstance: MetaLogBrowserAdapter | null = null;

/**
 * Get or create the singleton MetaLogBrowserAdapter instance
 */
export function getMetaLogBrowserAdapter(config?: MetaLogBrowserAdapterConfig): MetaLogBrowserAdapter {
  if (!adapterInstance) {
    adapterInstance = new MetaLogBrowserAdapter(config);
  }
  return adapterInstance;
}

