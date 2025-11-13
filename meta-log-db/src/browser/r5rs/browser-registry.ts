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

    // Bipartite-BQF functions
    this.registerBipartiteBQFFunctions();
    
    // Polynomial operations
    this.registerPolynomialFunctions();
  }

  /**
   * Register Bipartite-BQF R5RS functions
   */
  private registerBipartiteBQFFunctions(): void {
    /**
     * Evaluate BQF at point
     * r5rs:bqf-eval(bqf, values)
     * bqf: {form: string, coefficients?: number[], variables?: string[]}
     * values: number[] - values for variables in order
     */
    this.register('r5rs:bqf-eval', (bqf: any, values: number[] = []) => {
      if (!bqf || !bqf.form) {
        throw new Error('BQF form is required');
      }

      // Parse BQF form: Q(x,y) = ax² + bxy + cy²
      // For now, use coefficients if provided, otherwise parse from form
      if (bqf.coefficients && bqf.coefficients.length >= 3) {
        const [a, b, c] = bqf.coefficients;
        
        if (values.length === 0) {
          return 0; // 0D case
        } else if (values.length === 1) {
          // 1D: Q(x) = ax²
          const x = values[0];
          return a * x * x;
        } else if (values.length === 2) {
          // 2D: Q(x,y) = ax² + bxy + cy²
          const [x, y] = values;
          return a * x * x + b * x * y + c * y * y;
        } else {
          // Higher dimensions: extend formula
          // For diagonal BQF (b=0): Q(x₁,...,xₙ) = Σᵢ aᵢxᵢ²
          let result = 0;
          for (let i = 0; i < values.length; i++) {
            const coeff = i < bqf.coefficients.length ? bqf.coefficients[i] : 0;
            result += coeff * values[i] * values[i];
          }
          return result;
        }
      }

      // Fallback: try to parse form string (simplified)
      // This is a basic implementation - full parser would be more complex
      throw new Error('BQF evaluation requires coefficients array');
    });

    /**
     * Transform BQF
     * r5rs:bqf-transform(bqf, transformation)
     * transformation: string describing transformation (e.g., "tan(Point0D)")
     */
    this.register('r5rs:bqf-transform', (bqf: any, transformation: string) => {
      if (!bqf || !bqf.form) {
        throw new Error('BQF form is required');
      }

      // For now, return transformed BQF structure
      // Full implementation would parse transformation and apply it
      return {
        ...bqf,
        transformation,
        transformed: true
      };
    });

    /**
     * Convert polynomial to BQF
     * r5rs:poly-to-bqf(polynomial)
     * polynomial: {monad: number[], functor: number[], perceptron: number[]}
     */
    this.register('r5rs:poly-to-bqf', (polynomial: any) => {
      if (!polynomial) {
        throw new Error('Polynomial is required');
      }

      // Extract dimension from polynomial (use monad as primary)
      const monad = polynomial.monad || [];
      const dimension = monad.length;

      // Generate BQF form based on dimension
      let form = '';
      let coefficients: number[] = [];
      let variables: string[] = [];

      if (dimension === 0) {
        form = 'Q() = 0';
        coefficients = [0];
        variables = [];
      } else if (dimension === 1) {
        form = 'Q(x) = x²';
        coefficients = [1, 0, 0];
        variables = ['x'];
      } else if (dimension === 2) {
        form = 'Q(x,y) = x² + y²';
        coefficients = [1, 0, 1];
        variables = ['x', 'y'];
      } else {
        // Higher dimensions
        const varNames = ['x', 'y', 'z', 't', 'w', 'u', 'v', 's'].slice(0, dimension);
        form = `Q(${varNames.join(',')}) = ${varNames.map(v => `${v}²`).join(' + ')}`;
        coefficients = Array(dimension).fill(1);
        variables = varNames;
      }

      return {
        form,
        coefficients,
        signature: 'euclidean',
        variables,
        polynomial: polynomial
      };
    });

    /**
     * Convert BQF to R5RS procedure
     * r5rs:bqf-to-procedure(bqf)
     * Returns Scheme lambda expression as string
     */
    this.register('r5rs:bqf-to-procedure', (bqf: any) => {
      if (!bqf || !bqf.form) {
        throw new Error('BQF form is required');
      }

      // If procedure already exists, return it
      if (bqf.procedure) {
        return bqf.procedure;
      }

      // Generate procedure from BQF
      const variables = bqf.variables || [];
      const coefficients = bqf.coefficients || [];

      if (variables.length === 0) {
        return "(lambda () 'vacuum)";
      }

      // Generate Scheme expression for BQF evaluation
      const varList = variables.join(' ');
      let body = '';

      if (variables.length === 1) {
        // Q(x) = ax²
        const a = coefficients[0] || 1;
        body = `(* ${a} (* ${variables[0]} ${variables[0]}))`;
      } else if (variables.length === 2) {
        // Q(x,y) = ax² + bxy + cy²
        const [a, b, c] = [coefficients[0] || 1, coefficients[1] || 0, coefficients[2] || 1];
        const x = variables[0];
        const y = variables[1];
        body = `(+ (* ${a} (* ${x} ${x})) (+ (* ${b} (* ${x} ${y})) (* ${c} (* ${y} ${y}))))`;
      } else {
        // Higher dimensions: sum of squares
        const terms = variables.map((v: string, i: number) => {
          const coeff = coefficients[i] || 1;
          return `(* ${coeff} (* ${v} ${v}))`;
        });
        body = terms.reduce((acc: string, term: string) => acc ? `(+ ${acc} ${term})` : term, '');
      }

      return `(lambda (${varList}) ${body})`;
    });
  }

  /**
   * Register polynomial operation R5RS functions
   */
  private registerPolynomialFunctions(): void {
    /**
     * Polynomial addition (component-wise)
     * r5rs:poly-add(v1, v2)
     */
    this.register('r5rs:poly-add', (v1: number[], v2: number[]) => {
      if (!Array.isArray(v1) || !Array.isArray(v2)) {
        throw new Error('Both arguments must be arrays');
      }
      const maxLen = Math.max(v1.length, v2.length);
      const result: number[] = [];
      for (let i = 0; i < maxLen; i++) {
        result[i] = (v1[i] || 0) + (v2[i] || 0);
      }
      return result;
    });

    /**
     * Polynomial multiplication
     * r5rs:poly-mult(v1, v2)
     */
    this.register('r5rs:poly-mult', (v1: number[], v2: number[]) => {
      if (!Array.isArray(v1) || !Array.isArray(v2)) {
        throw new Error('Both arguments must be arrays');
      }
      // Polynomial multiplication: convolve coefficients
      const result: number[] = Array(v1.length + v2.length - 1).fill(0);
      for (let i = 0; i < v1.length; i++) {
        for (let j = 0; j < v2.length; j++) {
          result[i + j] += v1[i] * v2[j];
        }
      }
      return result;
    });

    /**
     * Polynomial composition
     * r5rs:poly-compose(p1, p2)
     */
    this.register('r5rs:poly-compose', (p1: number[], p2: number[]) => {
      if (!Array.isArray(p1) || !Array.isArray(p2)) {
        throw new Error('Both arguments must be arrays');
      }
      // Compose polynomials: p1(p2(x))
      // This is simplified - full implementation would handle all degrees
      const polyMult = (v1: number[], v2: number[]) => {
        const result: number[] = Array(v1.length + v2.length - 1).fill(0);
        for (let i = 0; i < v1.length; i++) {
          for (let j = 0; j < v2.length; j++) {
            result[i + j] += v1[i] * v2[j];
          }
        }
        return result;
      };
      const polyAdd = (v1: number[], v2: number[]) => {
        const maxLen = Math.max(v1.length, v2.length);
        const result: number[] = [];
        for (let i = 0; i < maxLen; i++) {
          result[i] = (v1[i] || 0) + (v2[i] || 0);
        }
        return result;
      };
      
      let result: number[] = [];
      for (let i = 0; i < p1.length; i++) {
        if (p1[i] !== 0) {
          // Multiply p2 by itself i times and scale by p1[i]
          let composed = [p1[i]];
          for (let j = 0; j < i; j++) {
            composed = polyMult(composed, p2);
          }
          result = polyAdd(result, composed);
        }
      }
      return result;
    });

    /**
     * Evaluate polynomial at point
     * r5rs:poly-eval(p, x)
     */
    this.register('r5rs:poly-eval', (p: number[], x: number) => {
      if (!Array.isArray(p)) {
        throw new Error('First argument must be an array');
      }
      if (typeof x !== 'number') {
        throw new Error('Second argument must be a number');
      }
      // Horner's method for polynomial evaluation
      let result = 0;
      for (let i = p.length - 1; i >= 0; i--) {
        result = result * x + (p[i] || 0);
      }
      return result;
    });
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

