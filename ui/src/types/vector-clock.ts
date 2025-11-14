/**
 * Vector Clock Types
 * 
 * Vector clocks for causal ordering in distributed systems.
 * Based on provenance metadata: [file, line, timestamp, pattern, ...extra]
 * 
 * Source: docs/31-Understanding-Computational-Geometries/02-Reflections/03-grok-automaton-recursion.md
 */

/**
 * Vector Clock component types
 */
export type VectorClockComponent = string | number;

/**
 * Vector Clock
 * 
 * Tracks causal history across distributed processes.
 * Format: [file: string, line: number, timestamp: number, pattern: string, ...extra: any[]]
 * 
 * Each component represents a different dimension of causality:
 * - file: Source file identifier
 * - line: Line number in file
 * - timestamp: Logical time
 * - pattern: Pattern identifier
 * - extra: Additional components for extended causality
 */
export type VectorClock = VectorClockComponent[];

/**
 * Vector Clock Comparison Result
 */
export interface VectorClockComparison {
  /**
   * True if v1 happens before v2 (v1 < v2)
   */
  happensBefore: boolean;

  /**
   * True if v1 and v2 are concurrent (neither happens before the other)
   */
  concurrent: boolean;

  /**
   * True if v1 and v2 are equal (v1 === v2)
   */
  equal: boolean;

  /**
   * True if v1 happens after v2 (v1 > v2)
   */
  happensAfter: boolean;

  /**
   * Component-wise comparison results
   */
  components: {
    index: number;
    v1: VectorClockComponent;
    v2: VectorClockComponent;
    relation: '<' | '=' | '>' | '?';
  }[];
}

/**
 * Vector Clock Merge Result
 */
export interface VectorClockMergeResult {
  /**
   * Merged vector clock (component-wise maximum)
   */
  merged: VectorClock;

  /**
   * Components that were updated during merge
   */
  updated: number[];

  /**
   * Conflict indicators (where components differ)
   */
  conflicts: number[];
}

/**
 * Vector Clock Increment Options
 */
export interface VectorClockIncrementOptions {
  /**
   * Component index to increment (default: timestamp index, usually 2)
   */
  componentIndex?: number;

  /**
   * Increment value (default: 1)
   */
  value?: number;

  /**
   * Component name for named increments (e.g., "datalog", "prolog")
   */
  componentName?: string;
}

/**
 * Vector Clock Validation Result
 */
export interface VectorClockValidation {
  /**
   * True if vector clock is valid
   */
  valid: boolean;

  /**
   * Validation errors
   */
  errors: string[];

  /**
   * Minimum required length
   */
  minLength: number;

  /**
   * Actual length
   */
  actualLength: number;
}

