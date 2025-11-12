/**
 * Computational Manifold Service
 * 
 * Provides 3D spatial encoding of polynomial types and evaluation traces.
 */

import { ProvenanceNode } from './provenance-slide-service';

export interface TypeSpaceCoordinates {
  position: [number, number, number];
  rotation: [number, number, number];
  scale: number;
  opacity: number;
}

export interface EvaluationKeyframe {
  time: number;
  duration: number;
  transform: TypeSpaceCoordinates;
  effect: {
    type: 'particle-burst' | 'energy-flow' | 'branch-glow' | 'pulse';
    color: string;
    intensity?: number;
  };
}

export interface EvaluationStep {
  type: 'β-reduction' | 'primitive-application' | 'if-reduction' | 'other';
  before?: any;
  after?: any;
  operator?: string;
  args?: any[];
  condition?: any;
}

export class ComputationalManifoldService {
  /**
   * Map 8-type polynomial vector to 3D coordinates
   */
  typeSpaceCoordinates(typeVector: number[]): TypeSpaceCoordinates {
    if (typeVector.length < 8) {
      // Pad with zeros if incomplete
      typeVector = [...typeVector, ...Array(8 - typeVector.length).fill(0)];
    }
    
    const [b, p, s, n, c, str, v, proc] = typeVector;
    
    return {
      position: [b / 10.0, p / 10.0, s / 10.0],
      rotation: [n * 0.1, c * 0.1, str * 0.1],
      scale: 1.0 + (v * 0.5),
      opacity: Math.max(0, Math.min(1, proc / 10.0))
    };
  }

  /**
   * Extract type vector from provenance node
   */
  extractTypeVector(node: ProvenanceNode): number[] {
    // Default type vector: [boolean, pair, symbol, number, char, string, vector, procedure]
    const defaultVector = [0, 0, 0, 0, 0, 0, 0, 0];
    
    // Try to extract from metadata
    if (node.metadata.churchEncoding) {
      // Parse Church encoding to extract type information
      // This is a simplified extraction - actual implementation would parse the encoding
      const encoding = node.metadata.churchEncoding;
      if (typeof encoding === 'string') {
        // Simple heuristic: count occurrences of type keywords
        defaultVector[0] = (encoding.match(/boolean|bool/gi) || []).length;
        defaultVector[1] = (encoding.match(/pair|cons/gi) || []).length;
        defaultVector[2] = (encoding.match(/symbol/gi) || []).length;
        defaultVector[3] = (encoding.match(/number|num/gi) || []).length;
        defaultVector[4] = (encoding.match(/char/gi) || []).length;
        defaultVector[5] = (encoding.match(/string|str/gi) || []).length;
        defaultVector[6] = (encoding.match(/vector|vec/gi) || []).length;
        defaultVector[7] = (encoding.match(/procedure|proc|lambda|λ/gi) || []).length;
      }
    }
    
    // Normalize to reasonable range
    return defaultVector.map(v => Math.min(10, Math.max(0, v)));
  }

  /**
   * Map provenance node to type-space coordinates
   */
  mapNodeToTypeSpace(node: ProvenanceNode): TypeSpaceCoordinates {
    const typeVector = this.extractTypeVector(node);
    return this.typeSpaceCoordinates(typeVector);
  }

  /**
   * Convert evaluation trace to animation keyframes
   */
  evalTraceToAnimation(trace: EvaluationStep[]): EvaluationKeyframe[] {
    const keyframes: EvaluationKeyframe[] = [];
    let time = 0.0;
    
    for (const step of trace) {
      const duration = this.getStepDuration(step);
      const transform = this.evaluationStepToTransform(step);
      const effect = this.evaluationEffect(step);
      
      keyframes.push({
        time,
        duration,
        transform,
        effect
      });
      
      time += duration;
    }
    
    return keyframes;
  }

  /**
   * Get duration for evaluation step
   */
  private getStepDuration(step: EvaluationStep): number {
    switch (step.type) {
      case 'β-reduction':
        return 1.0;
      case 'primitive-application':
        return 0.5;
      case 'if-reduction':
        return 0.7;
      default:
        return 0.3;
    }
  }

  /**
   * Convert evaluation step to transform
   */
  private evaluationStepToTransform(step: EvaluationStep): TypeSpaceCoordinates {
    // Default transform based on step type
    const baseTransform: TypeSpaceCoordinates = {
      position: [0, 0, 0],
      rotation: [0, 0, 0],
      scale: 1.0,
      opacity: 1.0
    };
    
    switch (step.type) {
      case 'β-reduction':
        return {
          ...baseTransform,
          position: [0.5, 0, 0],
          scale: 1.2
        };
      case 'primitive-application':
        return {
          ...baseTransform,
          position: [0, 0.5, 0],
          rotation: [0, Math.PI / 4, 0]
        };
      case 'if-reduction':
        return {
          ...baseTransform,
          position: [0, 0, 0.5],
          rotation: [Math.PI / 4, 0, 0]
        };
      default:
        return baseTransform;
    }
  }

  /**
   * Get visual effect for evaluation step
   */
  private evaluationEffect(step: EvaluationStep): EvaluationKeyframe['effect'] {
    switch (step.type) {
      case 'β-reduction':
        return {
          type: 'particle-burst',
          color: '#ff6b6b',
          intensity: 0.8
        };
      case 'primitive-application':
        return {
          type: 'energy-flow',
          color: '#4ecdc4',
          intensity: 0.6
        };
      case 'if-reduction':
        return {
          type: 'branch-glow',
          color: '#45b7d1',
          intensity: 0.7
        };
      default:
        return {
          type: 'pulse',
          color: '#96ceb4',
          intensity: 0.5
        };
    }
  }

  /**
   * Calculate perceptron network transition curve
   */
  perceptronTransitionToCurve(
    from: TypeSpaceCoordinates,
    to: TypeSpaceCoordinates
  ): { controlPoints: [number, number, number][]; tangents: [number, number, number][] } {
    // Bezier curve control points
    const mid1: [number, number, number] = [
      from.position[0] + (to.position[0] - from.position[0]) * 0.3,
      from.position[1] + (to.position[1] - from.position[1]) * 0.3,
      from.position[2] + (to.position[2] - from.position[2]) * 0.3
    ];
    
    const mid2: [number, number, number] = [
      from.position[0] + (to.position[0] - from.position[0]) * 0.7,
      from.position[1] + (to.position[1] - from.position[1]) * 0.7,
      from.position[2] + (to.position[2] - from.position[2]) * 0.7
    ];
    
    const controlPoints: [number, number, number][] = [
      from.position,
      mid1,
      mid2,
      to.position
    ];
    
    const tangents: [number, number, number][] = [
      [
        mid1[0] - from.position[0],
        mid1[1] - from.position[1],
        mid1[2] - from.position[2]
      ],
      [
        to.position[0] - mid2[0],
        to.position[1] - mid2[1],
        to.position[2] - mid2[2]
      ]
    ];
    
    return { controlPoints, tangents };
  }
}

export const computationalManifoldService = new ComputationalManifoldService();

