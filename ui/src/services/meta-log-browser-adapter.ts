/**
 * MetaLogBrowserAdapter - Browser-compatible adapter for meta-log-db
 * 
 * Wrapper around CanvasLMetaverseBrowser for backward compatibility.
 * This adapter maintains the existing API while using the unified CanvasLMetaverseBrowser internally.
 * 
 * Provides a clean TypeScript interface for the UI package to use
 * Meta-Log database functionality directly in the browser.
 * 
 * @deprecated Consider using CanvasLMetaverseBrowser directly for new code.
 */

import { CanvasLMetaverseBrowser, type CanvasLBrowserConfig } from 'meta-log-db/browser';
import type { Fact, Canvas } from 'meta-log-db/browser';

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
  private browser: CanvasLMetaverseBrowser;
  private config: CanvasLBrowserConfig;

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
    
    // Create unified browser instance
    this.browser = new CanvasLMetaverseBrowser(this.config);
  }

  /**
   * Initialize meta-log-db with browser-native MetaLogDbBrowser
   * Uses lazy initialization - only initializes when first needed
   */
  async init(): Promise<void> {
    await this.browser.init();
  }

  /**
   * Load canvas from URL (browser-compatible)
   * @param path - File path identifier (for caching)
   * @param url - URL to CanvasL/JSONL file (optional, uses path if not provided)
   */
  async loadCanvas(path: string, url?: string): Promise<void> {
    await this.browser.loadCanvas(path, url);
  }

  /**
   * Parse JSONL canvas from URL
   */
  async parseJsonlCanvas(path: string, url?: string): Promise<Canvas> {
    return await this.browser.parseJsonlCanvas(path, url);
  }

  /**
   * Parse CanvasL file from URL
   */
  async parseCanvasL(path: string, url?: string): Promise<Canvas> {
    return await this.browser.parseCanvasL(path, url);
  }

  /**
   * Extract facts from loaded canvas
   */
  extractFacts(): Fact[] {
    return this.browser.extractFacts();
  }

  /**
   * Execute ProLog query
   */
  async prologQuery(query: string): Promise<any> {
    return await this.browser.prologQuery(query);
  }

  /**
   * Execute DataLog query
   */
  async datalogQuery(goal: string, program?: any): Promise<any> {
    return await this.browser.datalogQuery(goal, program);
  }

  /**
   * Execute SPARQL query
   */
  async sparqlQuery(query: string): Promise<any> {
    return await this.browser.sparqlQuery(query);
  }

  /**
   * Validate with SHACL
   */
  async validateShacl(shapes?: any, triples?: any[]): Promise<any> {
    return await this.browser.validateShacl(shapes, triples);
  }

  /**
   * Add ProLog rule
   */
  addPrologRule(rule: string): void {
    this.browser.addPrologRule(rule);
  }

  /**
   * Add DataLog rule
   */
  addDatalogRule(rule: string): void {
    this.browser.addDatalogRule(rule);
  }

  /**
   * Execute R5RS function
   */
  async executeR5RS(functionName: string, args: any[]): Promise<any> {
    return await this.browser.executeR5RS(functionName, args);
  }

  /**
   * Get R5RS function
   */
  async getR5RSFunction(name: string): Promise<any> {
    return await this.browser.getR5RSFunction(name);
  }

  /**
   * List R5RS functions
   */
  async listR5RSFunctions(pattern?: string): Promise<string[]> {
    return await this.browser.listR5RSFunctions(pattern);
  }

  /**
   * Invoke R5RS function (alias for executeR5RS)
   */
  async invokeR5RSFunction(name: string, args: any[], context?: any): Promise<any> {
    return await this.browser.invokeR5RSFunction(name, args, context);
  }

  /**
   * Check if adapter is initialized
   */
  isInitialized(): boolean {
    return this.browser.isInitialized();
  }

  /**
   * Get the underlying database instance (for advanced usage)
   */
  getDb() {
    return this.browser.getDb();
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

