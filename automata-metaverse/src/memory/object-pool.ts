/**
 * Object Pool for Memory Optimization
 * 
 * Reuses objects to reduce allocation overhead and GC pressure
 */

/**
 * Generic object pool implementation
 */
export class ObjectPool<T> {
  private pool: T[] = [];
  private createFn: () => T;
  private resetFn: (obj: T) => void;
  private maxSize: number;

  /**
   * Create a new object pool
   * 
   * @param createFn - Function to create new objects
   * @param resetFn - Function to reset objects for reuse
   * @param maxSize - Maximum pool size (default: 100)
   */
  constructor(createFn: () => T, resetFn: (obj: T) => void, maxSize: number = 100) {
    this.createFn = createFn;
    this.resetFn = resetFn;
    this.maxSize = maxSize;
  }

  /**
   * Acquire an object from the pool
   * 
   * @returns {T} Object from pool or newly created
   */
  acquire(): T {
    if (this.pool.length > 0) {
      return this.pool.pop()!;
    }
    return this.createFn();
  }

  /**
   * Release an object back to the pool
   * 
   * @param obj - Object to release
   */
  release(obj: T): void {
    if (this.pool.length < this.maxSize) {
      this.resetFn(obj);
      this.pool.push(obj);
    }
  }

  /**
   * Clear the pool
   */
  clear(): void {
    this.pool = [];
  }

  /**
   * Get current pool size
   */
  get size(): number {
    return this.pool.length;
  }
}

