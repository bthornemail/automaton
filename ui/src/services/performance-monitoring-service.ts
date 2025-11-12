/**
 * Performance Monitoring Service
 * 
 * Tracks FPS, memory usage, worker message latency, and performance warnings
 */

export interface MemoryUsage {
  total: number;
  used: number;
  available: number;
  breakdown: {
    nodes: number;
    edges: number;
    threeObjects: number;
    workerBuffers: number;
    cache: number;
  };
}

export interface PerformanceWarning {
  type: 'low-fps' | 'high-memory' | 'high-latency' | 'large-node-count' | 'large-edge-count';
  message: string;
  severity: 'warning' | 'error';
  timestamp: number;
  value: number;
  threshold: number;
}

export interface PerformanceMetrics {
  fps: number;
  averageFps: number;
  frameTime: number;
  memoryUsage: MemoryUsage;
  messageLatency: Record<string, number>;
  warnings: PerformanceWarning[];
  nodeCount: number;
  edgeCount: number;
}

interface MessageLatencyEntry {
  type: string;
  startTime: number;
}

export class PerformanceMonitoringService {
  private isMonitoring: boolean = false;
  private frameCount: number = 0;
  private lastFrameTime: number = 0;
  private fpsHistory: number[] = [];
  private maxFpsHistory: number = 60;
  private currentFps: number = 0;
  private averageFps: number = 0;
  private frameTime: number = 0;
  
  private messageLatency: Map<string, number[]> = new Map();
  private pendingMessages: Map<string, MessageLatencyEntry> = new Map();
  
  private warnings: PerformanceWarning[] = [];
  private maxWarnings: number = 100;
  
  private nodeCount: number = 0;
  private edgeCount: number = 0;
  
  private animationFrameId: number | null = null;

  /**
   * Start performance monitoring.
   * 
   * Begins continuous performance monitoring, tracking FPS, memory usage, and
   * message latencies. Monitoring runs in the background using requestAnimationFrame.
   * 
   * @example
   * ```typescript
   * performanceMonitoringService.startMonitoring();
   * // Performance metrics are now being tracked
   * ```
   */
  startMonitoring(): void {
    if (this.isMonitoring) return;
    
    this.isMonitoring = true;
    this.lastFrameTime = performance.now();
    this.measureFrame();
  }

  /**
   * Stop performance monitoring.
   * 
   * Stops the continuous performance monitoring loop. Metrics collected up to
   * this point are retained and can still be queried.
   * 
   * @example
   * ```typescript
   * performanceMonitoringService.stopMonitoring();
   * // Monitoring is stopped, but metrics are still available
   * ```
   */
  stopMonitoring(): void {
    if (!this.isMonitoring) return;
    
    this.isMonitoring = false;
    if (this.animationFrameId !== null) {
      cancelAnimationFrame(this.animationFrameId);
      this.animationFrameId = null;
    }
  }

  /**
   * Measure frame performance
   */
  private measureFrame = (): void => {
    if (!this.isMonitoring) return;

    const currentTime = performance.now();
    const deltaTime = currentTime - this.lastFrameTime;
    this.lastFrameTime = currentTime;

    // Calculate FPS
    this.frameTime = deltaTime;
    this.currentFps = deltaTime > 0 ? 1000 / deltaTime : 0;
    
    // Update FPS history
    this.fpsHistory.push(this.currentFps);
    if (this.fpsHistory.length > this.maxFpsHistory) {
      this.fpsHistory.shift();
    }
    
    // Calculate average FPS
    if (this.fpsHistory.length > 0) {
      this.averageFps = this.fpsHistory.reduce((sum, fps) => sum + fps, 0) / this.fpsHistory.length;
    }

    // Check for low FPS warning
    if (this.currentFps < 30 && this.currentFps > 0) {
      this.logWarning({
        type: 'low-fps',
        message: `Low FPS detected: ${this.currentFps.toFixed(1)} FPS`,
        severity: 'warning',
        timestamp: Date.now(),
        value: this.currentFps,
        threshold: 30
      });
    }

    // Continue monitoring
    this.animationFrameId = requestAnimationFrame(this.measureFrame);
  };

  /**
   * Track message latency start.
   * 
   * Records the start time of a message operation for latency tracking.
   * Should be called before sending a message to a worker or making an async call.
   * 
   * @param {string} type - Message type (e.g., 'init', 'load', 'render')
   * @param {string} [messageId] - Optional unique message ID (auto-generated if not provided)
   * 
   * @example
   * ```typescript
   * const messageId = performanceMonitoringService.trackMessageStart('load');
   * await workerService.loadProvenanceChain(chain);
   * performanceMonitoringService.trackMessageEnd(messageId);
   * ```
   */
  trackMessageStart(type: string, messageId?: string): void {
    const id = messageId || `${type}-${Date.now()}`;
    this.pendingMessages.set(id, {
      type,
      startTime: performance.now()
    });
  }

  /**
   * Track message latency end.
   * 
   * Records the end time of a message operation and calculates latency.
   * Should be called after receiving a response or completing an async operation.
   * 
   * @param {string} messageId - Message ID returned from trackMessageStart
   * 
   * @example
   * ```typescript
   * const messageId = performanceMonitoringService.trackMessageStart('load');
   * await workerService.loadProvenanceChain(chain);
   * performanceMonitoringService.trackMessageEnd(messageId);
   * const latency = performanceMonitoringService.getMessageLatency('load');
   * ```
   */
  trackMessageEnd(messageId: string): void {
    const entry = this.pendingMessages.get(messageId);
    if (!entry) return;

    const latency = performance.now() - entry.startTime;
    
    // Store latency for this message type
    if (!this.messageLatency.has(entry.type)) {
      this.messageLatency.set(entry.type, []);
    }
    
    const latencies = this.messageLatency.get(entry.type)!;
    latencies.push(latency);
    
    // Keep only last 100 measurements per type
    if (latencies.length > 100) {
      latencies.shift();
    }
    
    this.pendingMessages.delete(messageId);

    // Check for high latency warning
    if (latency > 100) {
      this.logWarning({
        type: 'high-latency',
        message: `High latency for ${entry.type}: ${latency.toFixed(1)}ms`,
        severity: 'warning',
        timestamp: Date.now(),
        value: latency,
        threshold: 100
      });
    }
  }

  /**
   * Update node and edge counts.
   * 
   * Updates the current node and edge counts for the provenance chain being
   * rendered. This is used to generate performance warnings for large graphs.
   * 
   * @param {number} nodeCount - Current number of nodes
   * @param {number} edgeCount - Current number of edges
   * 
   * @example
   * ```typescript
   * performanceMonitoringService.updateNodeEdgeCounts(chain.nodes.length, chain.edges.length);
   * // Warnings will be generated if counts exceed thresholds
   * ```
   */
  updateNodeEdgeCounts(nodeCount: number, edgeCount: number): void {
    this.nodeCount = nodeCount;
    this.edgeCount = edgeCount;

    // Check for large node count warning
    if (nodeCount > 1000) {
      this.logWarning({
        type: 'large-node-count',
        message: `Large node count: ${nodeCount} nodes`,
        severity: 'warning',
        timestamp: Date.now(),
        value: nodeCount,
        threshold: 1000
      });
    }

    // Check for large edge count warning
    if (edgeCount > 5000) {
      this.logWarning({
        type: 'large-edge-count',
        message: `Large edge count: ${edgeCount} edges`,
        severity: 'warning',
        timestamp: Date.now(),
        value: edgeCount,
        threshold: 5000
      });
    }
  }

  /**
   * Get current FPS.
   * 
   * Returns the current frames per second, calculated from the most recent
   * frame time measurement.
   * 
   * @returns {number} Current FPS (0 if monitoring is not active)
   * 
   * @example
   * ```typescript
   * const fps = performanceMonitoringService.getFPS();
   * console.log(`Current FPS: ${fps.toFixed(1)}`);
   * ```
   */
  getFPS(): number {
    return this.currentFps;
  }

  /**
   * Get average FPS.
   * 
   * Returns the average FPS over the last 60 frames, providing a more stable
   * performance metric than the current FPS.
   * 
   * @returns {number} Average FPS over last 60 frames
   * 
   * @example
   * ```typescript
   * const avgFps = performanceMonitoringService.getAverageFPS();
   * console.log(`Average FPS: ${avgFps.toFixed(1)}`);
   * ```
   */
  getAverageFPS(): number {
    return this.averageFps;
  }

  /**
   * Get memory usage.
   * 
   * Returns current memory usage statistics if available in the browser.
   * Includes used, total, and estimated memory breakdown by component.
   * 
   * @returns {MemoryUsage} Memory usage statistics
   * 
   * @example
   * ```typescript
   * const memory = performanceMonitoringService.getMemoryUsage();
   * console.log(`Memory used: ${(memory.used / 1024 / 1024).toFixed(2)} MB`);
   * ```
   */
  getMemoryUsage(): MemoryUsage {
    // Try to use performance.memory API (Chrome)
    let total = 0;
    let used = 0;
    let available = 0;

    if ('memory' in performance && (performance as any).memory) {
      const memory = (performance as any).memory;
      total = memory.totalJSHeapSize || 0;
      used = memory.usedJSHeapSize || 0;
      available = memory.jsHeapSizeLimit || 0;
    } else {
      // Estimate from object counts
      used = this.estimateMemoryUsage();
      total = used;
      available = 0;
    }

    // Check for high memory warning
    if (used > 100 * 1024 * 1024) { // 100MB
      this.logWarning({
        type: 'high-memory',
        message: `High memory usage: ${(used / 1024 / 1024).toFixed(1)}MB`,
        severity: 'warning',
        timestamp: Date.now(),
        value: used,
        threshold: 100 * 1024 * 1024
      });
    }

    return {
      total,
      used,
      available,
      breakdown: {
        nodes: this.estimateNodeMemory(),
        edges: this.estimateEdgeMemory(),
        threeObjects: 0, // Would need Three.js renderer info
        workerBuffers: 0, // Would need worker info
        cache: 0 // Would need cache info
      }
    };
  }

  /**
   * Estimate memory usage from object counts
   */
  private estimateMemoryUsage(): number {
    // Rough estimate: ~1KB per node, ~0.5KB per edge
    return (this.nodeCount * 1024) + (this.edgeCount * 512);
  }

  /**
   * Estimate node memory
   */
  private estimateNodeMemory(): number {
    return this.nodeCount * 1024; // ~1KB per node
  }

  /**
   * Estimate edge memory
   */
  private estimateEdgeMemory(): number {
    return this.edgeCount * 512; // ~0.5KB per edge
  }

  /**
   * Get message latency for a specific type
   */
  getMessageLatency(type: string): number {
    const latencies = this.messageLatency.get(type);
    if (!latencies || latencies.length === 0) return 0;
    
    return latencies.reduce((sum, lat) => sum + lat, 0) / latencies.length;
  }

  /**
   * Get all message latencies
   */
  getAllMessageLatencies(): Record<string, number> {
    const result: Record<string, number> = {};
    
    for (const [type] of this.messageLatency.entries()) {
      result[type] = this.getMessageLatency(type);
    }
    
    return result;
  }

  /**
   * Get performance warnings
   */
  getWarnings(): PerformanceWarning[] {
    return [...this.warnings];
  }

  /**
   * Log performance warning
   */
  logWarning(warning: PerformanceWarning): void {
    // Avoid duplicate warnings (same type within 5 seconds)
    const recentWarning = this.warnings.find(
      w => w.type === warning.type && 
      Date.now() - w.timestamp < 5000
    );
    
    if (recentWarning) return;

    this.warnings.push(warning);
    
    // Keep only recent warnings
    if (this.warnings.length > this.maxWarnings) {
      this.warnings.shift();
    }

    // Log to console in development
    if (import.meta.env.DEV || import.meta.env.MODE === 'development') {
      console.warn(`[Performance] ${warning.message}`, warning);
    }
  }

  /**
   * Get all performance metrics.
   * 
   * Returns a comprehensive object containing all performance metrics:
   * FPS, memory usage, message latencies, node/edge counts, and warnings.
   * 
   * @returns {PerformanceMetrics} Complete performance metrics object
   * 
   * @example
   * ```typescript
   * const metrics = performanceMonitoringService.getMetrics();
   * console.log('Performance Metrics:', metrics);
   * ```
   */
  getMetrics(): PerformanceMetrics {
    return {
      fps: this.currentFps,
      averageFps: this.averageFps,
      frameTime: this.frameTime,
      memoryUsage: this.getMemoryUsage(),
      messageLatency: this.getAllMessageLatencies(),
      warnings: this.getWarnings(),
      nodeCount: this.nodeCount,
      edgeCount: this.edgeCount
    };
  }

  /**
   * Clear all metrics
   */
  clear(): void {
    this.warnings = [];
    this.messageLatency.clear();
    this.pendingMessages.clear();
    this.fpsHistory = [];
    this.nodeCount = 0;
    this.edgeCount = 0;
  }
}

// Export singleton instance
export const performanceMonitoringService = new PerformanceMonitoringService();

