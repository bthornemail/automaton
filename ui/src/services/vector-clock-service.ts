/**
 * Vector Clock Service
 * 
 * Implements vector clock operations for causal ordering in distributed systems.
 * Vector clocks track causal history: [file, line, timestamp, pattern, ...extra]
 * 
 * Source: docs/31-Understanding-Computational-Geometries/02-Reflections/03-grok-automaton-recursion.md
 */

import type {
  VectorClock,
  VectorClockComponent,
  VectorClockComparison,
  VectorClockMergeResult,
  VectorClockIncrementOptions,
  VectorClockValidation
} from '../types/vector-clock';

/**
 * Vector Clock Service
 * 
 * Provides operations for:
 * - Causal ordering (happens-before comparison)
 * - Clock merging (component-wise maximum)
 * - Clock incrementing (advancing logical time)
 * - Conflict detection (concurrent events)
 */
export class VectorClockService {
  /**
   * Default vector clock indices
   */
  private static readonly DEFAULT_INDICES = {
    FILE: 0,
    LINE: 1,
    TIMESTAMP: 2,
    PATTERN: 3
  };

  /**
   * Compare two vector clocks to determine causal ordering
   * 
   * v1 happens before v2 if:
   * - All components of v1 are <= corresponding components of v2
   * - At least one component of v1 is < corresponding component of v2
   * 
   * @param v1 - First vector clock
   * @param v2 - Second vector clock
   * @returns Comparison result with happens-before, concurrent, equal, happens-after flags
   */
  compare(v1: VectorClock, v2: VectorClock): VectorClockComparison {
    const maxLength = Math.max(v1.length, v2.length);
    const components: VectorClockComparison['components'] = [];
    
    let allLessOrEqual = true;
    let atLeastOneLess = false;
    let allEqual = true;
    let atLeastOneGreater = false;

    for (let i = 0; i < maxLength; i++) {
      const c1 = i < v1.length ? v1[i] : this.getDefaultComponent(i);
      const c2 = i < v2.length ? v2[i] : this.getDefaultComponent(i);
      
      const relation = this.compareComponents(c1, c2);
      components.push({ index: i, v1: c1, v2: c2, relation });

      if (relation === '<') {
        atLeastOneLess = true;
        allEqual = false;
      } else if (relation === '>') {
        allLessOrEqual = false;
        atLeastOneGreater = true;
        allEqual = false;
      } else if (relation === '=') {
        // Continue checking
      } else {
        // '?' - incomparable (different types)
        allLessOrEqual = false;
        allEqual = false;
      }
    }

    const happensBefore = allLessOrEqual && atLeastOneLess;
    const equal = allEqual && !atLeastOneLess && !atLeastOneGreater;
    const happensAfter = !happensBefore && !equal && atLeastOneGreater;
    const concurrent = !happensBefore && !happensAfter && !equal;

    return {
      happensBefore,
      concurrent,
      equal,
      happensAfter,
      components
    };
  }

  /**
   * Check if v1 happens before v2
   * 
   * @param v1 - First vector clock
   * @param v2 - Second vector clock
   * @returns True if v1 happens before v2
   */
  happensBefore(v1: VectorClock, v2: VectorClock): boolean {
    return this.compare(v1, v2).happensBefore;
  }

  /**
   * Merge two vector clocks (component-wise maximum)
   * 
   * Merged clock = max(v1[i], v2[i]) for each component i
   * Used to combine causal histories from different processes
   * 
   * @param v1 - First vector clock
   * @param v2 - Second vector clock
   * @returns Merged vector clock with conflict indicators
   */
  merge(v1: VectorClock, v2: VectorClock): VectorClockMergeResult {
    const maxLength = Math.max(v1.length, v2.length);
    const merged: VectorClock = [];
    const updated: number[] = [];
    const conflicts: number[] = [];

    for (let i = 0; i < maxLength; i++) {
      const c1 = i < v1.length ? v1[i] : this.getDefaultComponent(i);
      const c2 = i < v2.length ? v2[i] : this.getDefaultComponent(i);
      
      const max = this.maxComponent(c1, c2);
      merged.push(max);

      if (c1 !== c2) {
        conflicts.push(i);
        if (max === c2) {
          updated.push(i);
        }
      }
    }

    return {
      merged,
      updated,
      conflicts
    };
  }

  /**
   * Increment a vector clock component
   * 
   * @param vc - Vector clock to increment
   * @param options - Increment options (component index, value, or name)
   * @returns New vector clock with incremented component
   */
  increment(
    vc: VectorClock,
    options: VectorClockIncrementOptions = {}
  ): VectorClock {
    const {
      componentIndex = VectorClockService.DEFAULT_INDICES.TIMESTAMP,
      value = 1,
      componentName
    } = options;

    // If componentName is provided, find index by name
    let index = componentIndex;
    if (componentName) {
      index = this.findComponentIndex(vc, componentName);
      if (index === -1) {
        // Add new named component
        const newVc = [...vc, componentName];
        index = newVc.length - 1;
        const result = [...newVc];
        result[index] = (typeof result[index] === 'number' ? result[index] : 0) + value;
        return result;
      }
    }

    // Ensure vector clock is long enough
    const result = [...vc];
    while (result.length <= index) {
      result.push(this.getDefaultComponent(result.length));
    }

    // Increment the component
    const current = result[index];
    if (typeof current === 'number') {
      result[index] = current + value;
    } else {
      // If not a number, convert to number or use value
      result[index] = value;
    }

    return result;
  }

  /**
   * Create initial vector clock from provenance metadata
   * 
   * @param file - Source file identifier
   * @param line - Line number (default: 0)
   * @param timestamp - Logical timestamp (default: Date.now())
   * @param pattern - Pattern identifier (default: 'init')
   * @param extra - Additional components
   * @returns Initial vector clock
   */
  create(
    file: string,
    line: number = 0,
    timestamp: number = Date.now(),
    pattern: string = 'init',
    ...extra: VectorClockComponent[]
  ): VectorClock {
    return [file, line, timestamp, pattern, ...extra];
  }

  /**
   * Validate vector clock format
   * 
   * @param vc - Vector clock to validate
   * @param minLength - Minimum required length (default: 4)
   * @returns Validation result
   */
  validate(vc: VectorClock, minLength: number = 4): VectorClockValidation {
    const errors: string[] = [];
    
    if (!Array.isArray(vc)) {
      errors.push('Vector clock must be an array');
      return {
        valid: false,
        errors,
        minLength,
        actualLength: 0
      };
    }

    if (vc.length < minLength) {
      errors.push(`Vector clock must have at least ${minLength} components, got ${vc.length}`);
    }

    // Validate component types
    if (vc.length > 0 && typeof vc[0] !== 'string') {
      errors.push('Component 0 (file) must be a string');
    }
    if (vc.length > 1 && typeof vc[1] !== 'number') {
      errors.push('Component 1 (line) must be a number');
    }
    if (vc.length > 2 && typeof vc[2] !== 'number') {
      errors.push('Component 2 (timestamp) must be a number');
    }
    if (vc.length > 3 && typeof vc[3] !== 'string') {
      errors.push('Component 3 (pattern) must be a string');
    }

    return {
      valid: errors.length === 0,
      errors,
      minLength,
      actualLength: vc.length
    };
  }

  /**
   * Compare two components
   * 
   * @param c1 - First component
   * @param c2 - Second component
   * @returns '<' if c1 < c2, '>' if c1 > c2, '=' if equal, '?' if incomparable
   */
  private compareComponents(
    c1: VectorClockComponent,
    c2: VectorClockComponent
  ): '<' | '=' | '>' | '?' {
    if (typeof c1 === 'number' && typeof c2 === 'number') {
      if (c1 < c2) return '<';
      if (c1 > c2) return '>';
      return '=';
    }

    if (typeof c1 === 'string' && typeof c2 === 'string') {
      if (c1 < c2) return '<';
      if (c1 > c2) return '>';
      return '=';
    }

    // Different types - incomparable
    return '?';
  }

  /**
   * Get maximum of two components
   * 
   * @param c1 - First component
   * @param c2 - Second component
   * @returns Maximum component
   */
  private maxComponent(
    c1: VectorClockComponent,
    c2: VectorClockComponent
  ): VectorClockComponent {
    const comparison = this.compareComponents(c1, c2);
    if (comparison === '>' || comparison === '=') {
      return c1;
    }
    if (comparison === '<') {
      return c2;
    }
    // Incomparable - return c1 as default
    return c1;
  }

  /**
   * Get default component value for index
   * 
   * @param index - Component index
   * @returns Default value (0 for numbers, '' for strings)
   */
  private getDefaultComponent(index: number): VectorClockComponent {
    if (index === VectorClockService.DEFAULT_INDICES.FILE) {
      return '';
    }
    if (index === VectorClockService.DEFAULT_INDICES.LINE) {
      return 0;
    }
    if (index === VectorClockService.DEFAULT_INDICES.TIMESTAMP) {
      return 0;
    }
    if (index === VectorClockService.DEFAULT_INDICES.PATTERN) {
      return '';
    }
    // Default to 0 for extra components
    return 0;
  }

  /**
   * Find component index by name
   * 
   * @param vc - Vector clock
   * @param name - Component name to find
   * @returns Component index, or -1 if not found
   */
  private findComponentIndex(vc: VectorClock, name: string): number {
    // For named components, we'd need a mapping structure
    // For now, search for string matches
    for (let i = 0; i < vc.length; i++) {
      if (vc[i] === name) {
        return i;
      }
    }
    return -1;
  }

  /**
   * Extract provenance metadata from vector clock
   * 
   * @param vc - Vector clock
   * @returns Provenance metadata object
   */
  extractProvenance(vc: VectorClock): {
    file: string;
    line: number;
    timestamp: number;
    pattern: string;
    extra: VectorClockComponent[];
  } {
    return {
      file: (vc[0] as string) || '',
      line: (vc[1] as number) || 0,
      timestamp: (vc[2] as number) || 0,
      pattern: (vc[3] as string) || '',
      extra: vc.slice(4)
    };
  }

  /**
   * Create vector clock from provenance metadata
   * 
   * @param provenance - Provenance metadata
   * @returns Vector clock
   */
  fromProvenance(provenance: {
    file: string;
    line: number;
    timestamp: number;
    pattern: string;
    extra?: VectorClockComponent[];
  }): VectorClock {
    return [
      provenance.file,
      provenance.line,
      provenance.timestamp,
      provenance.pattern,
      ...(provenance.extra || [])
    ];
  }
}

// Singleton instance
export const vectorClockService = new VectorClockService();

