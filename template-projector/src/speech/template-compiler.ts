/**
 * CANVASL Template Compiler and Validator
 * Compiles templates to executable voice apps with homological validation
 */

import type {
  CANVASLTemplate,
  ChainComplex,
  SpeechHandlers,
  MacroExecutor,
  ValidationResult,
  CompiledVoiceApp,
  Keyword,
  Edge,
  InterfaceTriple
} from './types.js';
import { SpeechRecognitionHandler, SpeechSynthesisHandler } from './speech-handlers.js';
import { createMacro } from './web-api-macros.js';

// ========================================
// TEMPLATE COMPILER
// ========================================

export class TemplateCompiler {
  /**
   * Compile a CANVASL template into an executable voice app
   */
  compile(template: CANVASLTemplate): CompiledVoiceApp {
    console.log(`[TemplateCompiler] Compiling template: ${template.id}`);

    // Extract chain complex from template
    const complex = this.buildComplex(template);

    // Generate speech handlers
    const speechHandlers = this.compileSpeechHandlers(template);

    // Generate macro executors
    const macroExecutors = this.compileMacros(template);

    // Validate topology
    const validation = this.validate(template, complex);

    if (!validation.valid) {
      console.warn(`[TemplateCompiler] Validation warnings:`, validation.errors);
    }

    return {
      complex,
      speechHandlers,
      macroExecutors,
      validation
    };
  }

  /**
   * Build chain complex from template
   */
  private buildComplex(template: CANVASLTemplate): ChainComplex {
    const complex: ChainComplex = {
      C0: [], // Keywords from speech.input.keywords + macro keywords
      C1: [], // Edges: speech_api connections
      C2: [], // Template itself
      C3: [], // Interface triples from macros
      C4: [], // Evolution contexts
      ∂1: new Map(),
      ∂2: new Map(),
      ∂3: new Map(),
      ∂4: new Map()
    };

    // C0: Keywords
    const keywordSet = new Set<string>();

    if (template.frontmatter.speech?.input?.keywords) {
      for (const kw of template.frontmatter.speech.input.keywords) {
        keywordSet.add(kw);
      }
    }

    if (template.frontmatter.macros) {
      for (const macro of template.frontmatter.macros) {
        keywordSet.add(macro.keyword);
      }
    }

    for (const kw of keywordSet) {
      complex.C0.push({ id: kw, name: kw, dimension: 0 });
    }

    // C1: Edges from keywords to APIs
    if (template.frontmatter.macros) {
      for (const macro of template.frontmatter.macros) {
        const edge: Edge = {
          id: `${macro.keyword}->${macro.api}`,
          type: macro.type,
          dimension: 1
        };
        complex.C1.push(edge);

        // ∂₁(edge) = [keyword, api]
        complex.∂1.set(edge.id, [macro.keyword, macro.api]);
      }
    }

    // C2: Template document
    complex.C2.push(template);

    // ∂₂ from frontmatter adjacency
    if (template.frontmatter.adjacency) {
      complex.∂2.set(template.id, {
        edges: template.frontmatter.adjacency.edges,
        signs: template.frontmatter.adjacency.orientation
      });
    }

    // C3: Interface triples for each macro
    if (template.frontmatter.macros) {
      for (const macro of template.frontmatter.macros) {
        const triple: InterfaceTriple = {
          id: `iface-${macro.keyword}`,
          triple: [macro.keyword, macro.api, macro.method],
          dimension: 3
        };
        complex.C3.push(triple);
      }
    }

    console.log(`[TemplateCompiler] Built complex: C0=${complex.C0.length}, C1=${complex.C1.length}, C2=${complex.C2.length}, C3=${complex.C3.length}`);

    return complex;
  }

  /**
   * Compile speech handlers
   */
  private compileSpeechHandlers(template: CANVASLTemplate): SpeechHandlers {
    // Placeholder callback - will be replaced by voice app
    const placeholderCallback = (keyword: string, transcript: string) => {
      console.log(`[SpeechHandler] Keyword detected: ${keyword} in "${transcript}"`);
    };

    return {
      recognition: new SpeechRecognitionHandler(
        template.frontmatter.speech.input,
        placeholderCallback
      ),
      synthesis: new SpeechSynthesisHandler(
        template.frontmatter.speech.output
      )
    };
  }

  /**
   * Compile macros into executors
   */
  private compileMacros(template: CANVASLTemplate): Map<string, MacroExecutor> {
    const executors = new Map<string, MacroExecutor>();

    if (!template.frontmatter.macros) {
      return executors;
    }

    for (const macroConfig of template.frontmatter.macros) {
      const macro = createMacro(macroConfig);

      if (macro) {
        executors.set(macro.keyword, {
          keyword: macro.keyword,
          execute: async (params?: any) => {
            return await macro.execute();
          }
        });
      }
    }

    console.log(`[TemplateCompiler] Compiled ${executors.size} macro executors`);

    return executors;
  }

  /**
   * Validate template with homological checks
   */
  private validate(
    template: CANVASLTemplate,
    complex: ChainComplex
  ): ValidationResult {
    const errors: string[] = [];

    // Check required fields
    if (!template.frontmatter.speech) {
      errors.push("Missing speech configuration");
    }

    if (!template.frontmatter.speech?.input?.keywords || template.frontmatter.speech.input.keywords.length === 0) {
      errors.push("No keywords defined for speech input");
    }

    if (!template.frontmatter.macros || template.frontmatter.macros.length === 0) {
      errors.push("No macros defined");
    }

    // Homological validation
    if (template.frontmatter.validates?.homology) {
      const homologyErrors = this.validateHomology(complex);
      errors.push(...homologyErrors);
    }

    // Accessibility validation
    if (template.frontmatter.validates?.accessibility) {
      const accessibilityErrors = this.validateAccessibility(template);
      errors.push(...accessibilityErrors);
    }

    return {
      valid: errors.length === 0,
      errors
    };
  }

  /**
   * Validate homological consistency
   */
  private validateHomology(complex: ChainComplex): string[] {
    const errors: string[] = [];

    // Check ∂ ∘ ∂ = 0 for n=1
    // For each edge e, verify ∂₁(e) gives valid vertices
    for (const edge of complex.C1) {
      const boundary = complex.∂1.get(edge.id);
      if (!boundary) {
        errors.push(`Edge ${edge.id} has no boundary`);
        continue;
      }

      const [v0, v1] = boundary;
      const hasV0 = complex.C0.some(v => v.id === v0);
      const hasV1 = complex.C0.some(v => v.id === v1);

      if (!hasV0) {
        errors.push(`Edge ${edge.id} references non-existent vertex ${v0}`);
      }
      if (!hasV1) {
        errors.push(`Edge ${edge.id} references non-existent vertex ${v1}`);
      }
    }

    // Check ∂₂ references valid edges
    for (const doc of complex.C2) {
      const boundary = complex.∂2.get(doc.id);
      if (!boundary) continue;

      for (const edgeId of boundary.edges) {
        if (!complex.C1.some(e => e.id === edgeId)) {
          errors.push(`Document ${doc.id} references non-existent edge ${edgeId}`);
        }
      }

      if (boundary.edges.length !== boundary.signs.length) {
        errors.push(`Document ${doc.id} has mismatched edges and orientations`);
      }
    }

    return errors;
  }

  /**
   * Validate WCAG accessibility compliance
   */
  private validateAccessibility(template: CANVASLTemplate): string[] {
    const errors: string[] = [];

    if (!template.frontmatter.speech?.input?.lang) {
      errors.push("Missing language specification for accessibility");
    }

    if (!template.frontmatter.speech?.output?.voice) {
      errors.push("Missing voice specification for speech output");
    }

    // Check speech rate is reasonable (0.5 to 2.0)
    const rate = template.frontmatter.speech?.output?.rate;
    if (rate && (rate < 0.5 || rate > 2.0)) {
      errors.push(`Speech rate ${rate} outside recommended range (0.5-2.0)`);
    }

    return errors;
  }
}

// ========================================
// HOMOLOGY COMPUTATION
// ========================================

export class HomologyComputer {
  /**
   * Compute boundary matrix for ∂ₙ: Cₙ → Cₙ₋₁
   */
  computeBoundaryMatrix(
    complex: ChainComplex,
    n: number
  ): number[][] {
    let domain: any[];
    let codomain: any[];
    let boundaryMap: Map<string, any>;

    switch (n) {
      case 1:
        domain = complex.C1;
        codomain = complex.C0;
        boundaryMap = complex.∂1;
        break;
      case 2:
        domain = complex.C2;
        codomain = complex.C1;
        boundaryMap = complex.∂2;
        break;
      case 3:
        domain = complex.C3;
        codomain = complex.C2;
        boundaryMap = complex.∂3;
        break;
      case 4:
        domain = complex.C4;
        codomain = complex.C3;
        boundaryMap = complex.∂4;
        break;
      default:
        return [];
    }

    if (domain.length === 0 || codomain.length === 0) {
      return [];
    }

    // Build matrix: rows = codomain basis, cols = domain basis
    const matrix: number[][] = [];
    for (let i = 0; i < codomain.length; i++) {
      matrix.push(new Array(domain.length).fill(0));
    }

    // Fill matrix entries from boundary map
    for (let j = 0; j < domain.length; j++) {
      const cell = domain[j];
      const boundary = boundaryMap.get(cell.id);

      if (!boundary) continue;

      if (n === 1) {
        // ∂₁(edge) = [v₁, v₂]
        const [v0, v1] = boundary as [string, string];
        const i0 = codomain.findIndex((v: any) => v.id === v0);
        const i1 = codomain.findIndex((v: any) => v.id === v1);
        if (i0 >= 0) matrix[i0][j] = -1;
        if (i1 >= 0) matrix[i1][j] = +1;
      } else {
        // Higher dimensions: oriented sums
        const { edges, signs } = boundary as { edges: string[], signs: number[] };
        for (let k = 0; k < edges.length; k++) {
          const i = codomain.findIndex((c: any) => c.id === edges[k]);
          if (i >= 0) matrix[i][j] = signs[k];
        }
      }
    }

    return matrix;
  }

  /**
   * Compute homology groups Hₙ = ker(∂ₙ) / im(∂ₙ₊₁)
   */
  computeHomology(complex: ChainComplex, n: number): {
    betti: number;
    generators: any[];
  } {
    // Simplified implementation - returns Betti number
    const ∂n = this.computeBoundaryMatrix(complex, n);

    // For now, just count independent cycles (ker dimension)
    // Full implementation would compute quotient ker/im
    const rank = this.matrixRank(∂n);
    const betti = Math.max(0, (n === 0 ? complex.C0.length : complex.C1.length) - rank);

    return {
      betti,
      generators: []
    };
  }

  /**
   * Compute matrix rank (simplified)
   */
  private matrixRank(matrix: number[][]): number {
    if (matrix.length === 0 || matrix[0].length === 0) return 0;

    const reduced = this.rowReduce(matrix);
    let rank = 0;

    for (const row of reduced) {
      if (row.some(x => x !== 0)) {
        rank++;
      }
    }

    return rank;
  }

  /**
   * Row reduction (Gaussian elimination mod 2)
   */
  private rowReduce(matrix: number[][]): number[][] {
    const result = matrix.map(row => [...row]);
    const rows = result.length;
    const cols = result[0]?.length || 0;

    let pivotRow = 0;
    for (let col = 0; col < cols && pivotRow < rows; col++) {
      // Find pivot
      let pivot = -1;
      for (let row = pivotRow; row < rows; row++) {
        if (result[row][col] !== 0) {
          pivot = row;
          break;
        }
      }

      if (pivot === -1) continue;

      // Swap rows
      [result[pivotRow], result[pivot]] = [result[pivot], result[pivotRow]];

      // Eliminate
      for (let row = 0; row < rows; row++) {
        if (row !== pivotRow && result[row][col] !== 0) {
          for (let c = 0; c < cols; c++) {
            result[row][c] = (result[row][c] - result[pivotRow][c]) % 2;
          }
        }
      }

      pivotRow++;
    }

    return result;
  }
}

/**
 * Compute all Betti numbers β₀ through β₄
 */
export function computeAllBetti(complex: ChainComplex): number[] {
  const computer = new HomologyComputer();
  const betti: number[] = [];

  for (let n = 0; n <= 4; n++) {
    const { betti: bn } = computer.computeHomology(complex, n);
    betti.push(bn);
  }

  return betti;
}

/**
 * Compute Euler characteristic: χ = Σ (-1)ⁿ βₙ
 */
export function eulerCharacteristic(betti: number[]): number {
  return betti.reduce((sum, bn, n) => sum + Math.pow(-1, n) * bn, 0);
}
