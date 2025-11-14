/**
 * Regular Polyhedra Geometry Operations Tool
 * 
 * Implements regular polyhedra geometry operations following docs/32-Regulay-Polyhedra-Geometry:
 * - BQF encoding for polyhedra
 * - Polyhedra transformations (dual swap, BQF apply/abstract)
 * - R5RS type to polyhedra mapping
 * - Geometric structure validation
 * 
 * Related Documentation:
 * - docs/32-Regulay-Polyhedra-Geometry/README.md
 * - docs/32-Regulay-Polyhedra-Geometry/04-COMPUTATIONAL-MAPPING.md
 * - docs/32-Regulay-Polyhedra-Geometry/05-BQF-ENCODING.md
 */

export interface BQFEncoding {
  coefficients: [number, number, number]; // [a, b, c]
  form: string; // e.g., "x² + xy + y²"
  signature: string; // e.g., "identity", "successor", "pairing"
}

export interface PolyhedraBQF {
  polyhedron: string; // e.g., "tetrahedron", "cube", "octahedron"
  bqf: BQFEncoding;
  vertices: number; // a
  edges: number; // b
  faces: number; // c
}

export interface BQFEncodeRequest {
  polyhedron: 'tetrahedron' | 'cube' | 'octahedron' | 'icosahedron' | 'dodecahedron';
  includeForm?: boolean;
  includeSignature?: boolean;
}

export interface BQFEncodeResponse {
  success: boolean;
  result?: PolyhedraBQF;
  error?: {
    code: string;
    message: string;
    details?: any;
  };
}

export interface PolyhedraTransformRequest {
  operation: 'dual-swap' | 'apply-bqf' | 'abstract-bqf';
  input: BQFEncoding | string; // BQF or polyhedron name
  options?: {
    preserveStructure?: boolean;
  };
}

export interface PolyhedraTransformResponse {
  success: boolean;
  result?: {
    output: BQFEncoding | string;
    transformation: string;
  };
  error?: {
    code: string;
    message: string;
    details?: any;
  };
}

export interface ComputeMappingRequest {
  r5rsType: 'boolean' | 'pair' | 'symbol' | 'number' | 'char' | 'string' | 'vector' | 'procedure';
  includeDimension?: boolean;
  includeBQF?: boolean;
}

export interface ComputeMappingResponse {
  success: boolean;
  result?: {
    r5rsType: string;
    dimension: string;
    polyhedron: string;
    bqf: BQFEncoding;
  };
  error?: {
    code: string;
    message: string;
    details?: any;
  };
}

export interface GeometricValidateRequest {
  structure: {
    type: 'polyhedron' | 'bqf' | 'mapping';
    data: any;
  };
  constraints?: {
    validateBQF?: boolean;
    validateDimensional?: boolean;
    validateBipartite?: boolean;
  };
}

export interface GeometricValidateResponse {
  success: boolean;
  result?: {
    valid: boolean;
    errors: string[];
    warnings: string[];
  };
  error?: {
    code: string;
    message: string;
    details?: any;
  };
}

/**
 * Polyhedra BQF definitions from docs/32-Regulay-Polyhedra-Geometry
 */
const POLYHEDRA_BQF: Record<string, PolyhedraBQF> = {
  tetrahedron: {
    polyhedron: 'tetrahedron',
    bqf: {
      coefficients: [4, 6, 4],
      form: '4x² + 6xy + 4y²',
      signature: 'pairing'
    },
    vertices: 4,
    edges: 6,
    faces: 4
  },
  cube: {
    polyhedron: 'cube',
    bqf: {
      coefficients: [8, 12, 6],
      form: '8x² + 12xy + 6y²',
      signature: 'algebra'
    },
    vertices: 8,
    edges: 12,
    faces: 6
  },
  octahedron: {
    polyhedron: 'octahedron',
    bqf: {
      coefficients: [6, 12, 8],
      form: '6x² + 12xy + 8y²',
      signature: 'network'
    },
    vertices: 6,
    edges: 12,
    faces: 8
  },
  icosahedron: {
    polyhedron: 'icosahedron',
    bqf: {
      coefficients: [12, 30, 20],
      form: '12x² + 30xy + 20y²',
      signature: 'intelligence'
    },
    vertices: 12,
    edges: 30,
    faces: 20
  },
  dodecahedron: {
    polyhedron: 'dodecahedron',
    bqf: {
      coefficients: [20, 30, 12],
      form: '20x² + 30xy + 12y²',
      signature: 'quantum'
    },
    vertices: 20,
    edges: 30,
    faces: 12
  }
};

/**
 * R5RS Type to Polyhedra Mapping from docs/32-Regulay-Polyhedra-Geometry/04-COMPUTATIONAL-MAPPING.md
 */
const R5RS_TYPE_MAPPING: Record<string, { dimension: string; polyhedron: string; bqf: BQFEncoding }> = {
  boolean: {
    dimension: '0D',
    polyhedron: 'point',
    bqf: { coefficients: [1, 0, 0], form: 'x²', signature: 'identity' }
  },
  char: {
    dimension: '1D',
    polyhedron: 'line',
    bqf: { coefficients: [2, 1, 0], form: '2x² + xy', signature: 'successor' }
  },
  number: {
    dimension: '2D',
    polyhedron: 'plane',
    bqf: { coefficients: [4, 4, 1], form: '4x² + 4xy + y²', signature: 'pairing' }
  },
  pair: {
    dimension: '3D',
    polyhedron: 'tetrahedron',
    bqf: { coefficients: [4, 6, 4], form: '4x² + 6xy + 4y²', signature: 'pairing' }
  },
  string: {
    dimension: '4D',
    polyhedron: 'cube',
    bqf: { coefficients: [8, 12, 6], form: '8x² + 12xy + 6y²', signature: 'algebra' }
  },
  vector: {
    dimension: '5D',
    polyhedron: 'octahedron',
    bqf: { coefficients: [6, 12, 8], form: '6x² + 12xy + 8y²', signature: 'network' }
  },
  procedure: {
    dimension: '6D',
    polyhedron: 'icosahedron',
    bqf: { coefficients: [12, 30, 20], form: '12x² + 30xy + 20y²', signature: 'intelligence' }
  },
  symbol: {
    dimension: '7D',
    polyhedron: 'dodecahedron',
    bqf: { coefficients: [20, 30, 12], form: '20x² + 30xy + 12y²', signature: 'quantum' }
  }
};

/**
 * Encode polyhedron as BQF
 */
export function bqfEncode(request: BQFEncodeRequest): BQFEncodeResponse {
  try {
    const polyhedra = POLYHEDRA_BQF[request.polyhedron];
    if (!polyhedra) {
      return {
        success: false,
        error: {
          code: 'BQF_ENCODE_POLYHEDRON_NOT_FOUND',
          message: `Polyhedron not found: ${request.polyhedron}`,
          details: { available: Object.keys(POLYHEDRA_BQF) }
        }
      };
    }

    const result: PolyhedraBQF = {
      ...polyhedra,
      bqf: {
        ...polyhedra.bqf,
        form: request.includeForm !== false ? polyhedra.bqf.form : '',
        signature: request.includeSignature !== false ? polyhedra.bqf.signature : ''
      }
    };

    return {
      success: true,
      result
    };
  } catch (error) {
    return {
      success: false,
      error: {
        code: 'BQF_ENCODE_ERROR',
        message: error instanceof Error ? error.message : String(error),
        details: { error }
      }
    };
  }
}

/**
 * Transform polyhedra using BQF operations
 */
export function polyhedraTransform(request: PolyhedraTransformRequest): PolyhedraTransformResponse {
  try {
    let inputBQF: BQFEncoding;

    // Convert input to BQF if needed
    if (typeof request.input === 'string') {
      const polyhedra = POLYHEDRA_BQF[request.input.toLowerCase()];
      if (!polyhedra) {
        return {
          success: false,
          error: {
            code: 'POLYHEDRA_TRANSFORM_INVALID_INPUT',
            message: `Invalid polyhedron name: ${request.input}`,
            details: { available: Object.keys(POLYHEDRA_BQF) }
          }
        };
      }
      inputBQF = polyhedra.bqf;
    } else {
      inputBQF = request.input;
    }

    let outputBQF: BQFEncoding;
    let transformation: string;

    switch (request.operation) {
      case 'dual-swap':
        // Dual swap: swap vertices and faces, keep edges
        outputBQF = {
          coefficients: [inputBQF.coefficients[2], inputBQF.coefficients[1], inputBQF.coefficients[0]],
          form: `${inputBQF.coefficients[2]}x² + ${inputBQF.coefficients[1]}xy + ${inputBQF.coefficients[0]}y²`,
          signature: inputBQF.signature
        };
        transformation = `Dual swap: swapped vertices (${inputBQF.coefficients[0]}) and faces (${inputBQF.coefficients[2]})`;
        break;

      case 'apply-bqf':
        // Apply BQF (forward, exponential): [a, b, c] → [a, b, c-1]
        const newC = Math.max(0, inputBQF.coefficients[2] - 1);
        outputBQF = {
          coefficients: [
            inputBQF.coefficients[0],
            inputBQF.coefficients[1],
            newC
          ],
          form: `${inputBQF.coefficients[0]}x² + ${inputBQF.coefficients[1]}xy + ${newC}y²`,
          signature: inputBQF.signature
        };
        transformation = `Apply BQF: reduced faces from ${inputBQF.coefficients[2]} to ${newC}`;
        break;

      case 'abstract-bqf':
        // Abstract BQF (backward, linear): [a, b, c] → [a, b, c+1]
        const newC2 = inputBQF.coefficients[2] + 1;
        outputBQF = {
          coefficients: [
            inputBQF.coefficients[0],
            inputBQF.coefficients[1],
            newC2
          ],
          form: `${inputBQF.coefficients[0]}x² + ${inputBQF.coefficients[1]}xy + ${newC2}y²`,
          signature: inputBQF.signature
        };
        transformation = `Abstract BQF: increased faces from ${inputBQF.coefficients[2]} to ${newC2}`;
        break;

      default:
        return {
          success: false,
          error: {
            code: 'POLYHEDRA_TRANSFORM_INVALID_OPERATION',
            message: `Invalid operation: ${request.operation}`,
            details: { available: ['dual-swap', 'apply-bqf', 'abstract-bqf'] }
          }
        };
    }

    return {
      success: true,
      result: {
        output: outputBQF,
        transformation
      }
    };
  } catch (error) {
    return {
      success: false,
      error: {
        code: 'POLYHEDRA_TRANSFORM_ERROR',
        message: error instanceof Error ? error.message : String(error),
        details: { error }
      }
    };
  }
}

/**
 * Compute R5RS type to polyhedra mapping
 */
export function computeMapping(request: ComputeMappingRequest): ComputeMappingResponse {
  try {
    const mapping = R5RS_TYPE_MAPPING[request.r5rsType];
    if (!mapping) {
      return {
        success: false,
        error: {
          code: 'COMPUTE_MAPPING_INVALID_TYPE',
          message: `Invalid R5RS type: ${request.r5rsType}`,
          details: { available: Object.keys(R5RS_TYPE_MAPPING) }
        }
      };
    }

    const result: any = {
      r5rsType: request.r5rsType
    };

    if (request.includeDimension !== false) {
      result.dimension = mapping.dimension;
    }

    if (request.includeBQF !== false) {
      result.polyhedron = mapping.polyhedron;
      result.bqf = mapping.bqf;
    }

    return {
      success: true,
      result
    };
  } catch (error) {
    return {
      success: false,
      error: {
        code: 'COMPUTE_MAPPING_ERROR',
        message: error instanceof Error ? error.message : String(error),
        details: { error }
      }
    };
  }
}

/**
 * Validate geometric structure
 */
export function geometricValidate(request: GeometricValidateRequest): GeometricValidateResponse {
  const errors: string[] = [];
  const warnings: string[] = [];

  try {
    const { structure, constraints } = request;

    switch (structure.type) {
      case 'polyhedron':
        const polyName = structure.data.polyhedron || structure.data;
        if (typeof polyName === 'string' && !POLYHEDRA_BQF[polyName.toLowerCase()]) {
          errors.push(`Invalid polyhedron: ${polyName}`);
        }
        break;

      case 'bqf':
        const bqf = structure.data.bqf || structure.data;
        if (!bqf.coefficients || !Array.isArray(bqf.coefficients) || bqf.coefficients.length !== 3) {
          errors.push('Invalid BQF: coefficients must be array of 3 numbers');
        } else {
          const [a, b, c] = bqf.coefficients;
          if (a < 0 || b < 0 || c < 0) {
            errors.push('Invalid BQF: coefficients must be non-negative');
          }
        }

        if (constraints?.validateBQF) {
          // Additional BQF validation
          const [a, b, c] = bqf.coefficients || [0, 0, 0];
          // Euler's formula check: V - E + F = 2 (for convex polyhedra)
          const euler = a - b + c;
          if (Math.abs(euler - 2) > 0.1) {
            warnings.push(`Euler characteristic deviation: V - E + F = ${euler} (expected 2)`);
          }
        }
        break;

      case 'mapping':
        const r5rsType = structure.data.r5rsType || structure.data.type;
        if (!R5RS_TYPE_MAPPING[r5rsType]) {
          errors.push(`Invalid R5RS type in mapping: ${r5rsType}`);
        }
        break;

      default:
        errors.push(`Unknown structure type: ${structure.type}`);
    }

    // Validate dimensional progression if requested
    if (constraints?.validateDimensional && structure.type === 'mapping') {
      const mapping = R5RS_TYPE_MAPPING[structure.data.r5rsType || structure.data.type];
      if (mapping) {
        const dimension = parseInt(mapping.dimension);
        if (dimension < 0 || dimension > 7) {
          errors.push(`Invalid dimension: ${mapping.dimension} (must be 0D-7D)`);
        }
      }
    }

    // Validate bipartite structure if requested
    if (constraints?.validateBipartite && structure.type === 'bqf') {
      const bqf = structure.data.bqf || structure.data;
      const [a, b, c] = bqf.coefficients || [0, 0, 0];
      // Bipartite check: edges (b) should connect vertices (a) and faces (c)
      if (b < Math.min(a, c)) {
        warnings.push('Bipartite structure: edges may be insufficient to connect vertices and faces');
      }
    }

    return {
      success: true,
      result: {
        valid: errors.length === 0,
        errors,
        warnings
      }
    };
  } catch (error) {
    return {
      success: false,
      error: {
        code: 'GEOMETRIC_VALIDATE_ERROR',
        message: error instanceof Error ? error.message : String(error),
        details: { error }
      }
    };
  }
}

/**
 * Get all available polyhedra
 */
export function getAvailablePolyhedra(): string[] {
  return Object.keys(POLYHEDRA_BQF);
}

/**
 * Get all available R5RS types
 */
export function getAvailableR5RSTypes(): string[] {
  return Object.keys(R5RS_TYPE_MAPPING);
}

