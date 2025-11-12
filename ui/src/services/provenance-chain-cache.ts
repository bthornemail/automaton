/**
 * Provenance Chain Cache
 * 
 * LRU cache for frequently accessed provenance chains
 */

import { ProvenanceChain } from './provenance-slide-service';

export interface CacheEntry {
  chain: ProvenanceChain;
  timestamp: number;
  accessCount: number;
  lastAccessed: number;
}

export interface CacheStats {
  size: number;
  maxSize: number;
  hitCount: number;
  missCount: number;
  evictionCount: number;
  hitRate: number;
}

export class ProvenanceChainCache {
  private cache: Map<string, CacheEntry>;
  private maxSize: number;
  private hitCount: number;
  private missCount: number;
  private evictionCount: number;

  constructor(maxSize: number = 50) {
    this.cache = new Map();
    this.maxSize = maxSize;
    this.hitCount = 0;
    this.missCount = 0;
    this.evictionCount = 0;
  }

  /**
   * Get cached chain by key
   */
  get(key: string): ProvenanceChain | null {
    const entry = this.cache.get(key);
    
    if (!entry) {
      this.missCount++;
      return null;
    }

    // Update access statistics
    entry.accessCount++;
    entry.lastAccessed = Date.now();
    
    // Move to end (most recently used)
    this.cache.delete(key);
    this.cache.set(key, entry);
    
    this.hitCount++;
    return entry.chain;
  }

  /**
   * Set cached chain by key
   */
  set(key: string, chain: ProvenanceChain): void {
    // If key already exists, update it
    if (this.cache.has(key)) {
      const existing = this.cache.get(key)!;
      existing.chain = chain;
      existing.timestamp = Date.now();
      existing.lastAccessed = Date.now();
      existing.accessCount++;
      
      // Move to end
      this.cache.delete(key);
      this.cache.set(key, existing);
      return;
    }

    // If cache is full, evict least recently used
    if (this.cache.size >= this.maxSize) {
      this.evictLRU();
    }

    // Add new entry
    this.cache.set(key, {
      chain,
      timestamp: Date.now(),
      accessCount: 1,
      lastAccessed: Date.now()
    });
  }

  /**
   * Evict least recently used entry
   */
  private evictLRU(): void {
    if (this.cache.size === 0) return;

    // Find entry with oldest lastAccessed time
    let lruKey: string | null = null;
    let oldestTime = Infinity;

    for (const [key, entry] of this.cache.entries()) {
      if (entry.lastAccessed < oldestTime) {
        oldestTime = entry.lastAccessed;
        lruKey = key;
      }
    }

    if (lruKey) {
      this.cache.delete(lruKey);
      this.evictionCount++;
    }
  }

  /**
   * Clear all cached chains
   */
  clear(): void {
    this.cache.clear();
    this.hitCount = 0;
    this.missCount = 0;
    this.evictionCount = 0;
  }

  /**
   * Invalidate cache entries matching pattern
   */
  invalidate(pattern: string | RegExp): void {
    const regex = typeof pattern === 'string' ? new RegExp(pattern) : pattern;
    
    for (const key of this.cache.keys()) {
      if (regex.test(key)) {
        this.cache.delete(key);
      }
    }
  }

  /**
   * Get cache statistics
   */
  getStats(): CacheStats {
    const totalRequests = this.hitCount + this.missCount;
    const hitRate = totalRequests > 0 ? this.hitCount / totalRequests : 0;

    return {
      size: this.cache.size,
      maxSize: this.maxSize,
      hitCount: this.hitCount,
      missCount: this.missCount,
      evictionCount: this.evictionCount,
      hitRate
    };
  }

  /**
   * Generate cache key from evolution path and dimension
   */
  static generateKey(evolutionPath: string, dimension?: string): string {
    return dimension ? `${evolutionPath}:${dimension}` : evolutionPath;
  }
}

// Export singleton instance
export const provenanceChainCache = new ProvenanceChainCache(50);

