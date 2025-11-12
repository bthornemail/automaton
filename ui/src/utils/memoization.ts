/**
 * Memoization Utilities
 * 
 * Utilities for memoizing expensive computations
 */

/**
 * Create a memoized function with cache
 */
export function memoize<T extends (...args: any[]) => any>(
  fn: T,
  keyGenerator?: (...args: Parameters<T>) => string
): T {
  const cache = new Map<string, ReturnType<T>>();

  const memoized = ((...args: Parameters<T>): ReturnType<T> => {
    const key = keyGenerator 
      ? keyGenerator(...args)
      : JSON.stringify(args);

    if (cache.has(key)) {
      return cache.get(key)!;
    }

    const result = fn(...args);
    cache.set(key, result);
    return result;
  }) as T;

  // Add cache control methods
  (memoized as any).clear = () => cache.clear();
  (memoized as any).delete = (key: string) => cache.delete(key);
  (memoized as any).has = (key: string) => cache.has(key);
  (memoized as any).size = cache.size;

  return memoized;
}

/**
 * Generate hash from multiple values
 */
export function generateHash(...values: any[]): string {
  return values.map(v => {
    if (typeof v === 'string') return v;
    if (typeof v === 'number') return v.toString();
    if (Array.isArray(v)) return `[${v.map(generateHash).join(',')}]`;
    if (v && typeof v === 'object') {
      try {
        return JSON.stringify(v);
      } catch {
        return String(v);
      }
    }
    return String(v);
  }).join(':');
}

/**
 * Memoize slide content generation
 */
export function memoizeSlideContent(
  dimension: string,
  nodes: any[],
  patterns?: string[]
): string {
  const nodeCount = nodes.length;
  const patternHash = patterns ? generateHash(...patterns.sort()) : '';
  const cacheKey = `${dimension}:${nodeCount}:${patternHash}`;
  
  // This would be used with a cache store
  return cacheKey;
}

