/**
 * Browser R5RS Registry
 * 
 * Browser-specific R5RS registry using BrowserFileIO instead of Node.js fs
 */

import { BrowserFileIO } from '../io.js';
import { R5RSParser, SchemeExpression } from '../../r5rs/parser.js';
import { decryptFileContent } from '../crypto/storage-encryption.js';

export interface BrowserR5RSRegistryConfig {
  enableEncryption?: boolean;
  mnemonic?: string;
  encryptionPurpose?: 'local' | 'published' | 'contributor' | 'ephemeral';
  fileIO?: BrowserFileIO;
}

/**
 * Browser R5RS Function Registry
 */
export class BrowserR5RSRegistry {
  private functions: Map<string, Function> = new Map();
  private enginePath?: string;
  private parsedExpressions: SchemeExpression[] = [];
  private fileIO: BrowserFileIO;
  private enableEncryption: boolean;
  private mnemonic?: string;
  private encryptionPurpose: 'local' | 'published' | 'contributor' | 'ephemeral';

  constructor(config: BrowserR5RSRegistryConfig = {}) {
    this.fileIO = config.fileIO || new BrowserFileIO();
    this.enableEncryption = config.enableEncryption || false;
    this.mnemonic = config.mnemonic;
    this.encryptionPurpose = config.encryptionPurpose || 'local';
  }

  /**
   * Initialize file I/O
   */
  async init(): Promise<void> {
    await this.fileIO.init();
  }

  /**
   * Load R5RS engine from URL or IndexedDB
   */
  async load(path: string, url?: string): Promise<void> {
    this.enginePath = path;
    
    try {
      // Try to load Scheme file
      let content: string;

      try {
        content = await this.fileIO.loadFile(path, url);

        // Decrypt if encryption is enabled
        if (this.enableEncryption && this.mnemonic) {
          try {
            content = await decryptFileContent(content, this.mnemonic, this.encryptionPurpose);
          } catch (error) {
            // If decryption fails, assume content is not encrypted
            console.warn('Decryption failed, assuming unencrypted content:', error);
          }
        }
      } catch (error) {
        console.warn('Failed to load Scheme file, using builtins only:', error);
        this.registerBuiltins();
        return;
      }

      // Parse Scheme content
      this.parsedExpressions = R5RSParser.parse(content);
      
      // Extract and register functions
      const functionDefs = R5RSParser.extractFunctions(this.parsedExpressions);
      for (const [name, def] of functionDefs.entries()) {
        // Convert Scheme function definition to JavaScript function
        // This is simplified - full implementation would need an evaluator
        this.registerFromScheme(name, def);
      }
    } catch (error) {
      console.warn('Failed to parse Scheme file, using builtins only:', error);
    }
    
    // Always register builtins
    this.registerBuiltins();
  }

  /**
   * Register function from Scheme definition
   */
  private registerFromScheme(name: string, definition: SchemeExpression): void {
    // Simplified registration - full implementation would evaluate Scheme code
    // For now, we just store the definition for potential future evaluation
    console.log(`Parsed Scheme function: ${name}`);
    
    // In a full implementation, this would:
    // 1. Convert Scheme lambda to JavaScript function
    // 2. Handle closures and lexical scoping
    // 3. Support tail call optimization
    // For now, we rely on builtins
  }

  /**
   * Register built-in R5RS functions
   */
  private registerBuiltins(): void {
    // Church encoding functions
    this.register('r5rs:church-zero', () => (f: any) => (x: any) => x);
    this.register('r5rs:church-succ', (n: any) => (f: any) => (x: any) => f(n(f)(x)));
    this.register('r5rs:church-add', (m: any, n: any) => (f: any) => (x: any) => m(f)(n(f)(x)));
    this.register('r5rs:church-mult', (m: any, n: any) => (f: any) => m(n(f)));
    this.register('r5rs:church-exp', (m: any, n: any) => n(m));

    // Y-combinator
    this.register('r5rs:y-combinator', (f: any) => {
      const Y = (g: any) => g((x: any) => Y(g)(x));
      return Y(f);
    });

    // Basic list operations
    this.register('r5rs:cons', (a: any, b: any) => [a, b]);
    this.register('r5rs:car', (pair: any[]) => pair[0]);
    this.register('r5rs:cdr', (pair: any[]) => pair[1]);
    this.register('r5rs:null?', (x: any) => x === null || x === undefined || (Array.isArray(x) && x.length === 0));
  }

  /**
   * Execute an R5RS function
   */
  async execute(functionName: string, args: any[]): Promise<any> {
    const fn = this.getFunction(functionName);
    if (!fn) {
      throw new Error(`R5RS function not found: ${functionName}`);
    }

    try {
      return fn(...args);
    } catch (error) {
      throw new Error(`Error executing R5RS function ${functionName}: ${error}`);
    }
  }

  /**
   * Register a custom function
   */
  register(name: string, fn: Function): void {
    this.functions.set(name, fn);
  }

  /**
   * Get a function by name
   */
  getFunction(name: string): Function | null {
    return this.functions.get(name) || null;
  }

  /**
   * Check if a function exists
   */
  hasFunction(name: string): boolean {
    return this.functions.has(name);
  }

  /**
   * Get all registered function names
   */
  getFunctionNames(): string[] {
    return Array.from(this.functions.keys());
  }

  /**
   * Clear all functions
   */
  clear(): void {
    this.functions.clear();
  }
}

