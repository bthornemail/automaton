/**
 * BQF Transformation Service
 * 
 * Implements Binary Quadratic Form transformations for duality operations.
 * BQF = [a, b, c] where:
 *   a = affine points (binary, discrete, values/facts)
 *   b = interaction lines (shared bipartitely, ports/hashes)
 *   c = projective planes (float, continuous, functions/rules)
 * 
 * Source: docs/31-Understanding-Computational-Geometries/02-Reflections/05-grok-bqf.md
 */

/**
 * Binary Quadratic Form type
 * [affine, interaction, projective]
 */
export type BQF = [number, number, number];

/**
 * BQF transformation result
 */
export interface BQFTransformationResult {
  bqf: BQF;
  operation: 'apply' | 'abstract' | 'dual-swap' | 'compose';
  timestamp: number;
}

/**
 * BQF Transformation Service
 * 
 * Provides operations for transforming BQF forms:
 * - Apply: Forward transformation (exponential, affine → projective)
 * - Abstract: Backward transformation (linear, projective → affine)
 * - Dual Swap: Geometric inversion (swap affine ↔ projective)
 * - Compose: Sequential composition of transformations
 */
export class BQFTransformationService {
  /**
   * Apply BQF transformation (forward, exponential)
   * [a, b, c] → [a, b, c-1]
   * 
   * Direction: Affine → Projective
   * Effect: Forward propagation, exponential growth
   * 
   * @param bqf - Binary Quadratic Form [a, b, c]
   * @returns Transformed BQF
   * @throws Error if c=0 (cannot apply without projective component)
   */
  apply(bqf: BQF): BQF {
    const [a, b, c] = bqf;
    
    if (c <= 0) {
      throw new Error('Cannot apply: c=0 (no projective component)');
    }
    
    return [a, b, c - 1];
  }

  /**
   * Abstract BQF transformation (backward, linear)
   * [a, b, c] → [a, b, c+1]
   * 
   * Direction: Projective → Affine
   * Effect: Backward propagation, linear collapse
   * 
   * @param bqf - Binary Quadratic Form [a, b, c]
   * @returns Transformed BQF
   */
  abstract(bqf: BQF): BQF {
    const [a, b, c] = bqf;
    return [a, b, c + 1];
  }

  /**
   * Dual swap BQF transformation
   * [a, b, c] → [c, b, a]
   * 
   * Effect: Swap affine and projective components (geometric inversion)
   * Example: Cube [8,12,6] ↔ Octahedron [6,12,8]
   * 
   * @param bqf - Binary Quadratic Form [a, b, c]
   * @returns Dual-swapped BQF [c, b, a]
   */
  dualSwap(bqf: BQF): BQF {
    const [a, b, c] = bqf;
    return [c, b, a];
  }

  /**
   * Compose two BQF transformations
   * Q₁ ∘ Q₂: Compose transformations sequentially
   * 
   * Matrix multiplication for composition:
   * - x² coefficient: a₁a₂ + b₁a₂
   * - xy coefficient: a₁b₂ + b₁b₂ + c₁a₂
   * - y² coefficient: b₁c₂ + c₁c₂
   * 
   * @param q1 - First BQF transformation
   * @param q2 - Second BQF transformation
   * @returns Composed BQF
   */
  compose(q1: BQF, q2: BQF): BQF {
    const [a1, b1, c1] = q1;
    const [a2, b2, c2] = q2;
    
    return [
      a1 * a2 + b1 * a2,           // x² coefficient
      a1 * b2 + b1 * b2 + c1 * a2, // xy coefficient
      b1 * c2 + c1 * c2             // y² coefficient
    ];
  }

  /**
   * Check if BQF can be applied (has projective component)
   * 
   * @param bqf - Binary Quadratic Form [a, b, c]
   * @returns True if c > 0, false otherwise
   */
  canApply(bqf: BQF): boolean {
    return bqf[2] > 0;
  }

  /**
   * Check if BQF is self-dual (a === c)
   * 
   * @param bqf - Binary Quadratic Form [a, b, c]
   * @returns True if a === c (self-dual), false otherwise
   */
  isSelfDual(bqf: BQF): boolean {
    return bqf[0] === bqf[2];
  }

  /**
   * Create transformation result with metadata
   * 
   * @param bqf - Transformed BQF
   * @param operation - Operation performed
   * @returns Transformation result with timestamp
   */
  createResult(bqf: BQF, operation: 'apply' | 'abstract' | 'dual-swap' | 'compose'): BQFTransformationResult {
    return {
      bqf,
      operation,
      timestamp: Date.now()
    };
  }

  /**
   * Validate BQF format
   * 
   * @param bqf - Binary Quadratic Form to validate
   * @returns True if valid BQF [number, number, number], false otherwise
   */
  isValid(bqf: any): bqf is BQF {
    return (
      Array.isArray(bqf) &&
      bqf.length === 3 &&
      typeof bqf[0] === 'number' &&
      typeof bqf[1] === 'number' &&
      typeof bqf[2] === 'number'
    );
  }

  /**
   * String representation of BQF
   * 
   * @param bqf - Binary Quadratic Form [a, b, c]
   * @returns String representation: "[a, b, c]"
   */
  toString(bqf: BQF): string {
    return `[${bqf[0]}, ${bqf[1]}, ${bqf[2]}]`;
  }

  /**
   * Parse BQF from string or array
   * 
   * @param input - String "[a,b,c]" or array [a, b, c]
   * @returns Parsed BQF
   * @throws Error if invalid format
   */
  parse(input: string | number[]): BQF {
    if (Array.isArray(input)) {
      if (this.isValid(input)) {
        return input;
      }
      throw new Error(`Invalid BQF array: expected [number, number, number], got ${JSON.stringify(input)}`);
    }

    // Parse string format "[a, b, c]"
    const match = input.match(/\[(\d+),\s*(\d+),\s*(\d+)\]/);
    if (!match) {
      throw new Error(`Invalid BQF string format: expected "[a, b, c]", got "${input}"`);
    }

    const [a, b, c] = match.slice(1).map(Number);
    return [a, b, c];
  }
}

// Singleton instance
export const bqfTransformationService = new BQFTransformationService();

