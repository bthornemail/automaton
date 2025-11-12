/**
 * CodeMirror 6 Extension for CanvasL Language
 * 
 * CanvasL is an extended JSONL canvas format with:
 * - R5RS function references
 * - Dimension references (0D-7D)
 * - Node/edge references (#id)
 * - Directives (@directive)
 * - Scheme expressions
 * 
 * Compatible with Lezer grammar and LSP/AST support
 * Compatible with: https://lezer.codemirror.net/docs/guide/index.html#writing-a-grammar
 */

import { Extension } from '@codemirror/state';
import { EditorView, ViewPlugin, Decoration, DecorationSet } from '@codemirror/view';
import { tags as t } from '@lezer/highlight';

/**
 * CanvasL Syntax Highlighting Tags
 * 
 * These tags are used for Lezer grammar-based syntax highlighting
 * Compatible with @lezer/highlight
 */
export const canvaslTags = {
  canvaslDirective: t.meta,
  canvaslReference: t.variableName,
  canvaslDimension: t.number,
  canvaslType: t.typeName,
  canvaslEdgeType: t.typeName,
  canvaslR5RSFunction: t.function(t.variableName),
  canvaslSchemeExpression: t.string,
};

/**
 * CanvasL Highlighting Plugin
 * 
 * Provides syntax highlighting for CanvasL files (.canvasl extension)
 */
const canvaslHighlightPlugin = ViewPlugin.fromClass(
  class {
    decorations: DecorationSet;

    constructor(view: EditorView) {
      this.decorations = this.buildDecorations(view);
    }

    update(update: any) {
      if (update.docChanged || update.viewportChanged) {
        this.decorations = this.buildDecorations(update.view);
      }
    }

    buildDecorations(view: EditorView): DecorationSet {
      const decorations: any[] = [];
      const doc = view.state.doc;
      const text = doc.toString();

      // Parse JSONL entries line by line
      const lines = text.split('\n');
      let currentPos = 0;

      for (const line of lines) {
        if (!line.trim()) {
          currentPos += line.length + 1;
          continue;
        }

        try {
          // Try to parse as JSON
          const entry = JSON.parse(line);
          
          // Highlight based on entry type
          if (entry.id) {
            // Highlight ID
            const idMatch = line.match(/"id"\s*:\s*"([^"]+)"/);
            if (idMatch && idMatch.index !== undefined) {
              const idStart = currentPos + idMatch.index + idMatch[0].indexOf('"') + 1;
              const idEnd = idStart + idMatch[1].length;
              decorations.push(
                Decoration.mark({
                  class: 'cm-canvasl-id',
                }).range(idStart, idEnd)
              );
            }

            // Highlight type
            if (entry.type) {
              const typeMatch = line.match(/"type"\s*:\s*"([^"]+)"/);
              if (typeMatch && typeMatch.index !== undefined) {
                const typeStart = currentPos + typeMatch.index + typeMatch[0].indexOf('"') + 1;
                const typeEnd = typeStart + typeMatch[1].length;
                let typeClass = 'cm-canvasl-type';
                
                // Special highlighting for edge types
                if (['vertical', 'horizontal', 'transition', 'self-ref', 'r5rs-call'].includes(entry.type)) {
                  typeClass = 'cm-canvasl-edge-type';
                }
                
                decorations.push(
                  Decoration.mark({
                    class: typeClass,
                  }).range(typeStart, typeEnd)
                );
              }
            }

            // Highlight R5RS function references
            if (entry.function && entry.function.startsWith('r5rs:')) {
              const funcMatch = line.match(/"function"\s*:\s*"([^"]+)"/);
              if (funcMatch && funcMatch.index !== undefined) {
                const funcStart = currentPos + funcMatch.index + funcMatch[0].indexOf('r5rs:');
                const funcEnd = funcStart + entry.function.length - 5; // -5 for "r5rs:"
                decorations.push(
                  Decoration.mark({
                    class: 'cm-canvasl-r5rs-function',
                  }).range(funcStart, funcEnd)
                );
              }
            }

            // Highlight dimension references (0D-7D)
            if (entry.id.match(/^\dD-/)) {
              const dimMatch = entry.id.match(/^(\dD)/);
              if (dimMatch) {
                const dimMatchInLine = line.match(new RegExp(`"${dimMatch[1]}`));
                if (dimMatchInLine && dimMatchInLine.index !== undefined) {
                  const dimStart = currentPos + dimMatchInLine.index + 1;
                  const dimEnd = dimStart + dimMatch[1].length;
                  decorations.push(
                    Decoration.mark({
                      class: 'cm-canvasl-dimension',
                    }).range(dimStart, dimEnd)
                  );
                }
              }
            }

            // Highlight references (#id)
            const refMatches = line.matchAll(/#([a-zA-Z0-9_-]+)/g);
            for (const match of refMatches) {
              if (match.index !== undefined) {
                const refStart = currentPos + match.index;
                const refEnd = refStart + match[0].length;
                decorations.push(
                  Decoration.mark({
                    class: 'cm-canvasl-reference',
                  }).range(refStart, refEnd)
                );
              }
            }

            // Highlight directives (@directive)
            const directiveMatches = line.matchAll(/@([a-zA-Z_][a-zA-Z0-9_-]*)/g);
            for (const match of directiveMatches) {
              if (match.index !== undefined) {
                const dirStart = currentPos + match.index;
                const dirEnd = dirStart + match[0].length;
                decorations.push(
                  Decoration.mark({
                    class: 'cm-canvasl-directive',
                  }).range(dirStart, dirEnd)
                );
              }
            }
          }
        } catch (e) {
          // Invalid JSON, skip highlighting for this line
        }

        currentPos += line.length + 1;
      }

      return Decoration.set(decorations);
    }
  },
  {
    decorations: (v) => v.decorations,
  }
);

/**
 * CanvasL Language Support
 * 
 * Creates a language support extension for .canvasl files
 * Extends JSONL canvas format with additional features
 */
export function canvaslLanguage(): Extension[] {
  return [
    // Use JSON language as base (CodeMirror's JSON parser uses Lezer)
    // We'll add CanvasL-specific highlighting on top
    canvaslHighlightPlugin,
    
    // Theme customization for CanvasL elements
    EditorView.baseTheme({
      // CanvasL ID highlighting
      '.cm-canvasl-id': {
        color: '#79b8ff',
        fontWeight: 'bold',
      },
      
      // CanvasL type highlighting
      '.cm-canvasl-type': {
        color: '#b392f0',
        fontWeight: '500',
      },
      
      // CanvasL edge type highlighting
      '.cm-canvasl-edge-type': {
        color: '#f97583',
        fontWeight: '500',
      },
      
      // R5RS function highlighting
      '.cm-canvasl-r5rs-function': {
        color: '#9ecbff',
        fontFamily: 'monospace',
        fontWeight: '500',
      },
      
      // Dimension highlighting (0D-7D)
      '.cm-canvasl-dimension': {
        color: '#ffab70',
        fontWeight: 'bold',
      },
      
      // Reference highlighting (#id)
      '.cm-canvasl-reference': {
        color: '#79b8ff',
        fontWeight: '500',
      },
      
      // Directive highlighting (@directive)
      '.cm-canvasl-directive': {
        color: '#fdaeb7',
        fontWeight: 'bold',
      },
    }),
  ];
}

/**
 * Bipartite-BQF Types
 * 
 * Types for Bipartite Binary Quadratic Polynomial Form extension
 */
export interface BQFObject {
  form: string;
  coefficients?: number[];
  signature?: string;
  variables?: string[];
  polynomial?: string;
  symbol?: string;
  procedure?: string;
}

export interface BQFTransformation {
  from: BQFObject;
  to: BQFObject;
  transformation?: string;
  polynomial?: string;
}

export interface PolynomialObject {
  monad: number[];
  functor: number[];
  perceptron: number[];
}

export interface BipartiteMetadata {
  partition?: 'topology' | 'system' | 'topology-system' | 'topology-topology' | 'system-system';
  bqf?: BQFObject | BQFTransformation;
  polynomial?: PolynomialObject;
  progression?: string;
  mapping?: string;
}

/**
 * CanvasL AST Node Types
 * 
 * These types represent the AST structure for LSP support
 */
export interface CanvasLASTNode {
  type: 'node' | 'edge' | 'directive' | 'r5rs-call' | 'reference';
  id?: string;
  line: number;
  column: number;
  length: number;
  children?: CanvasLASTNode[];
  metadata?: {
    dimension?: string;
    r5rsFunction?: string;
    fromNode?: string;
    toNode?: string;
    bipartite?: BipartiteMetadata;
  };
}

/**
 * BQF Validation
 * 
 * Validates Binary Quadratic Form objects according to specification
 */
export interface BQFValidationResult {
  valid: boolean;
  errors: string[];
}

export function validateBQF(bqf: BQFObject, dimension?: string): BQFValidationResult {
  const errors: string[] = [];

  // Validate form is present
  if (!bqf.form || typeof bqf.form !== 'string') {
    errors.push('BQF form is required and must be a string');
  }

  // Validate coefficients if present
  if (bqf.coefficients !== undefined) {
    if (!Array.isArray(bqf.coefficients)) {
      errors.push('BQF coefficients must be an array');
    } else {
      const invalidCoeffs = bqf.coefficients.filter(c => typeof c !== 'number' || isNaN(c));
      if (invalidCoeffs.length > 0) {
        errors.push('BQF coefficients must be valid numbers');
      }
    }
  }

  // Validate signature if present
  if (bqf.signature !== undefined) {
    const validSignatures = ['euclidean', 'lorentz', 'minkowski', 'riemannian', 'consensus', 'intelligence', 'quantum', 'custom'];
    if (!validSignatures.includes(bqf.signature)) {
      errors.push(`BQF signature must be one of: ${validSignatures.join(', ')}`);
    }
  }

  // Validate variables if present
  if (bqf.variables !== undefined) {
    if (!Array.isArray(bqf.variables)) {
      errors.push('BQF variables must be an array');
    } else {
      const invalidVars = bqf.variables.filter(v => typeof v !== 'string');
      if (invalidVars.length > 0) {
        errors.push('BQF variables must be strings');
      }
      
      // Validate variable count matches dimension if dimension is provided
      if (dimension) {
        const dimMatch = dimension.match(/^(\d)D$/);
        if (dimMatch) {
          const dimNum = parseInt(dimMatch[1]);
          // Expected variable counts: 0D=0, 1D=1, 2D=2, 3D=4, 4D=5, 5D+=dimNum
          const expectedVarCount = dimNum === 0 ? 0 : dimNum === 1 ? 1 : dimNum === 2 ? 2 : dimNum === 3 ? 4 : dimNum === 4 ? 5 : dimNum;
          if (bqf.variables.length !== expectedVarCount) {
            errors.push(`BQF variables count (${bqf.variables.length}) does not match dimension ${dimension} (expected ${expectedVarCount})`);
          }
        }
      }
    }
  }

  // Validate BQF form matches dimensional progression if dimension is provided
  if (dimension && bqf.form) {
    const EXPECTED_FORMS: Record<string, string> = {
      '0D': 'Q() = 0',
      '1D': 'Q(x) = x²',
      '2D': 'Q(x,y) = x² + y²',
      '3D': 'Q(x,y,z,t) = x² + y² + z² - t²',
      '4D': 'Q(w,x,y,z,t) = w² + x² + y² + z² - t²'
    };
    
    const expectedForm = EXPECTED_FORMS[dimension];
    if (expectedForm && bqf.form !== expectedForm) {
      // Allow flexible matching for higher dimensions (5D+)
      const dimNum = parseInt(dimension[0]);
      if (dimNum < 5) {
        errors.push(`BQF form for ${dimension} must be: ${expectedForm}`);
      } else if (!bqf.form.includes('Σ') && !bqf.form.includes('xᵢ')) {
        errors.push(`BQF form for ${dimension} should match pattern with Σ notation`);
      }
    }
  }

  return {
    valid: errors.length === 0,
    errors
  };
}

export function validateBQFTransformation(transformation: BQFTransformation): BQFValidationResult {
  const errors: string[] = [];

  // Validate from BQF
  if (!transformation.from) {
    errors.push('BQF transformation "from" is required');
  } else {
    const fromResult = validateBQF(transformation.from);
    if (!fromResult.valid) {
      errors.push(...fromResult.errors.map(e => `from: ${e}`));
    }
  }

  // Validate to BQF
  if (!transformation.to) {
    errors.push('BQF transformation "to" is required');
  } else {
    const toResult = validateBQF(transformation.to);
    if (!toResult.valid) {
      errors.push(...toResult.errors.map(e => `to: ${e}`));
    }
  }

  return {
    valid: errors.length === 0,
    errors
  };
}

/**
 * Polynomial Validation
 * 
 * Validates polynomial objects according to specification
 */
export interface PolynomialValidationResult {
  valid: boolean;
  errors: string[];
}

export function validatePolynomial(poly: PolynomialObject): PolynomialValidationResult {
  const errors: string[] = [];

  // Validate monad array
  if (!Array.isArray(poly.monad)) {
    errors.push('Polynomial monad must be an array');
  } else if (poly.monad.length !== 8) {
    errors.push(`Polynomial monad must have exactly 8 components, got ${poly.monad.length}`);
  } else {
    const invalidMonad = poly.monad.filter(m => typeof m !== 'number' || isNaN(m));
    if (invalidMonad.length > 0) {
      errors.push('Polynomial monad components must be valid numbers');
    }
  }

  // Validate functor array
  if (!Array.isArray(poly.functor)) {
    errors.push('Polynomial functor must be an array');
  } else if (poly.functor.length !== 8) {
    errors.push(`Polynomial functor must have exactly 8 components, got ${poly.functor.length}`);
  } else {
    const invalidFunctor = poly.functor.filter(f => typeof f !== 'number' || isNaN(f));
    if (invalidFunctor.length > 0) {
      errors.push('Polynomial functor components must be valid numbers');
    }
  }

  // Validate perceptron array
  if (!Array.isArray(poly.perceptron)) {
    errors.push('Polynomial perceptron must be an array');
  } else if (poly.perceptron.length !== 8) {
    errors.push(`Polynomial perceptron must have exactly 8 components, got ${poly.perceptron.length}`);
  } else {
    const invalidPerceptron = poly.perceptron.filter(p => typeof p !== 'number' || isNaN(p));
    if (invalidPerceptron.length > 0) {
      errors.push('Polynomial perceptron components must be valid numbers');
    }
  }

  return {
    valid: errors.length === 0,
    errors
  };
}

/**
 * Bipartite Validation
 * 
 * Validates bipartite metadata according to specification
 */
export interface BipartiteValidationResult {
  valid: boolean;
  errors: string[];
}

export function validateBipartite(
  bipartite: BipartiteMetadata,
  nodeType: 'node' | 'edge',
  fromNode?: string,
  toNode?: string
): BipartiteValidationResult {
  const errors: string[] = [];

  // Validate partition
  if (bipartite.partition !== undefined) {
    const validPartitions = ['topology', 'system', 'topology-system', 'topology-topology', 'system-system'];
    if (!validPartitions.includes(bipartite.partition)) {
      errors.push(`Partition must be one of: ${validPartitions.join(', ')}`);
    }

    // Validate edge partition consistency
    if (nodeType === 'edge') {
      if (bipartite.partition === 'topology-system' || bipartite.partition === 'system-topology') {
        // Horizontal edge: topology ↔ system
        // This is valid
      } else if (bipartite.partition === 'topology-topology' || bipartite.partition === 'system-system') {
        // Vertical edge: same partition
        // This is valid
      } else {
        errors.push(`Edge partition "${bipartite.partition}" is not valid for edges`);
      }
    }
  }

  // Validate BQF
  if (bipartite.bqf !== undefined) {
    if ('from' in bipartite.bqf && 'to' in bipartite.bqf) {
      // BQF Transformation (for edges)
      const result = validateBQFTransformation(bipartite.bqf as BQFTransformation);
      if (!result.valid) {
        errors.push(...result.errors);
      }
    } else {
      // BQF Object (for nodes)
      const result = validateBQF(bipartite.bqf as BQFObject, bipartite.partition?.includes('topology') ? undefined : undefined);
      if (!result.valid) {
        errors.push(...result.errors);
      }
    }
  }

  // Validate polynomial
  if (bipartite.polynomial !== undefined) {
    const result = validatePolynomial(bipartite.polynomial);
    if (!result.valid) {
      errors.push(...result.errors);
    }
  }

  return {
    valid: errors.length === 0,
    errors
  };
}

/**
 * Parse bipartite metadata from entry
 */
function parseBipartiteMetadata(entry: any): BipartiteMetadata | undefined {
  if (!entry.bipartite || typeof entry.bipartite !== 'object') {
    return undefined;
  }

  const bipartite: BipartiteMetadata = {};

  // Parse partition
  if (entry.bipartite.partition) {
    const validPartitions = ['topology', 'system', 'topology-system', 'topology-topology', 'system-system'];
    if (validPartitions.includes(entry.bipartite.partition)) {
      bipartite.partition = entry.bipartite.partition as BipartiteMetadata['partition'];
    }
  }

  // Parse BQF
  if (entry.bipartite.bqf) {
    if (entry.bipartite.bqf.from && entry.bipartite.bqf.to) {
      // BQF Transformation
      bipartite.bqf = {
        from: entry.bipartite.bqf.from as BQFObject,
        to: entry.bipartite.bqf.to as BQFObject,
        transformation: entry.bipartite.bqf.transformation,
        polynomial: entry.bipartite.bqf.polynomial
      } as BQFTransformation;
    } else {
      // BQF Object
      bipartite.bqf = entry.bipartite.bqf as BQFObject;
    }
  }

  // Parse polynomial
  if (entry.bipartite.polynomial) {
    bipartite.polynomial = entry.bipartite.polynomial as PolynomialObject;
  }

  // Parse progression
  if (entry.bipartite.progression) {
    bipartite.progression = entry.bipartite.progression;
  }

  // Parse mapping
  if (entry.bipartite.mapping) {
    bipartite.mapping = entry.bipartite.mapping;
  }

  return Object.keys(bipartite).length > 0 ? bipartite : undefined;
}

/**
 * Parse CanvasL file into AST
 * 
 * This function creates an AST structure suitable for LSP integration
 */
export function parseCanvasLAST(content: string): CanvasLASTNode[] {
  const ast: CanvasLASTNode[] = [];
  const lines = content.split('\n');

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    if (!line.trim()) continue;

    try {
      const entry = JSON.parse(line);
      
      if (entry.id) {
        const node: CanvasLASTNode = {
          type: entry.type === 'vertical' || entry.type === 'horizontal' || entry.type === 'transition' 
            ? 'edge' 
            : entry.function && entry.function.startsWith('r5rs:')
            ? 'r5rs-call'
            : 'node',
          id: entry.id,
          line: i + 1,
          column: 0,
          length: line.length,
          metadata: {},
        };

        // Extract dimension from ID
        const dimMatch = entry.id.match(/^(\dD)/);
        if (dimMatch) {
          node.metadata!.dimension = dimMatch[1];
        }

        // Extract R5RS function
        if (entry.function && entry.function.startsWith('r5rs:')) {
          node.metadata!.r5rsFunction = entry.function;
        }

        // Extract edge information
        if (node.type === 'edge') {
          node.metadata!.fromNode = entry.from || entry.fromNode;
          node.metadata!.toNode = entry.to || entry.toNode;
        }

        // Extract and parse bipartite metadata
        const bipartite = parseBipartiteMetadata(entry);
        if (bipartite) {
          node.metadata!.bipartite = bipartite;
          
          // Validate bipartite metadata
          const validationResult = validateBipartite(
            bipartite,
            node.type,
            node.metadata!.fromNode,
            node.metadata!.toNode
          );
          
          if (!validationResult.valid) {
            // Store validation errors in metadata for LSP error reporting
            if (!node.metadata!.bipartite) {
              node.metadata!.bipartite = bipartite;
            }
            // Note: Validation errors can be accessed via separate validation function
            // For now, we parse but don't block on validation errors
            console.warn(`Bipartite validation errors for ${entry.id} at line ${i + 1}:`, validationResult.errors);
          }
        }

        ast.push(node);
      }
    } catch (e) {
      // Skip invalid JSON lines
    }
  }

  return ast;
}

/**
 * Get AST node at position (for LSP hover/definition)
 */
export function getASTNodeAtPosition(ast: CanvasLASTNode[], line: number, column: number): CanvasLASTNode | null {
  return ast.find(node => 
    node.line === line && 
    column >= node.column && 
    column <= node.column + node.length
  ) || null;
}

/**
 * Find references to a node ID (for LSP references)
 */
export function findReferences(ast: CanvasLASTNode[], nodeId: string): CanvasLASTNode[] {
  return ast.filter(node => 
    node.metadata?.fromNode === nodeId || 
    node.metadata?.toNode === nodeId ||
    node.id === nodeId
  );
}

/**
 * Validation Error for LSP
 */
export interface ValidationError {
  line: number;
  column: number;
  message: string;
  severity: 'error' | 'warning';
}

/**
 * Validate entire AST and return errors
 */
export function validateAST(ast: CanvasLASTNode[]): ValidationError[] {
  const errors: ValidationError[] = [];

  for (const node of ast) {
    if (node.metadata?.bipartite) {
      const validationResult = validateBipartite(
        node.metadata.bipartite,
        node.type,
        node.metadata.fromNode,
        node.metadata.toNode
      );

      if (!validationResult.valid) {
        for (const errorMsg of validationResult.errors) {
          errors.push({
            line: node.line,
            column: node.column,
            message: `Bipartite validation: ${errorMsg}`,
            severity: 'error'
          });
        }
      }
    }
  }

  return errors;
}

/**
 * Get validation errors for a specific node
 */
export function getNodeValidationErrors(node: CanvasLASTNode): ValidationError[] {
  const errors: ValidationError[] = [];

  if (node.metadata?.bipartite) {
    const validationResult = validateBipartite(
      node.metadata.bipartite,
      node.type,
      node.metadata.fromNode,
      node.metadata.toNode
    );

    if (!validationResult.valid) {
      for (const errorMsg of validationResult.errors) {
        errors.push({
          line: node.line,
          column: node.column,
          message: `Bipartite validation: ${errorMsg}`,
          severity: 'error'
        });
      }
    }
  }

  return errors;
}
