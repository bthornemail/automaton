/**
 * Example 6: Performance Optimization Patterns
 * 
 * This example demonstrates performance monitoring and optimization
 * strategies for large-scale provenance chains.
 */

import { performanceMonitoringService } from '@/services/performance-monitoring-service';
import { provenanceSlideService } from '@/services/provenance-slide-service';
import { provenanceChainCache } from '@/services/provenance-chain-cache';
import { avatarLoaderService } from '@/services/avatar-loader-service';

async function optimizePerformance() {
  // Step 1: Start performance monitoring
  performanceMonitoringService.startMonitoring();
  console.log('Performance monitoring started');

  // Step 2: Monitor metrics during operation
  const evolutionPath = '/evolutions/advanced-automaton';

  // Track chain building
  const chainStartId = performanceMonitoringService.trackMessageStart('buildChain');
  const chain = await provenanceSlideService.buildProvenanceChain(evolutionPath);
  performanceMonitoringService.trackMessageEnd(chainStartId);

  // Update node/edge counts
  performanceMonitoringService.updateNodeEdgeCounts(
    chain.nodes.length,
    chain.edges.length
  );

  // Step 3: Get performance metrics
  const metrics = performanceMonitoringService.getMetrics();
  console.log('\nPerformance Metrics:');
  console.log(`  FPS: ${metrics.fps.toFixed(1)}`);
  console.log(`  Average FPS: ${metrics.averageFps.toFixed(1)}`);
  console.log(`  Memory: ${(metrics.memoryUsage.used / 1024 / 1024).toFixed(2)} MB`);
  console.log(`  Node Count: ${metrics.nodeCount}`);
  console.log(`  Edge Count: ${metrics.edgeCount}`);

  // Step 4: Check for warnings
  const warnings = performanceMonitoringService.getWarnings();
  if (warnings.length > 0) {
    console.log('\nPerformance Warnings:');
    warnings.forEach(warning => {
      console.warn(`  ${warning.type}: ${warning.message}`);
    });
  }

  // Step 5: Optimize based on metrics
  if (metrics.fps < 30) {
    console.log('\nOptimizing for low FPS...');
    // Enable LOD, reduce quality, etc.
  }

  if (metrics.memoryUsage.used > 100 * 1024 * 1024) {
    console.log('\nOptimizing for high memory...');
    // Clear caches
    provenanceChainCache.clear();
    avatarLoaderService.clearCache();
    console.log('Caches cleared');
  }

  if (metrics.nodeCount > 1000) {
    console.log('\nOptimizing for large node count...');
    // Use pagination, instancing, LOD
    console.log('Consider using pagination for large chains');
  }

  // Step 6: Check message latencies
  const latencies = performanceMonitoringService.getAllMessageLatencies();
  console.log('\nMessage Latencies:');
  Object.entries(latencies).forEach(([type, latency]) => {
    console.log(`  ${type}: ${latency.toFixed(2)}ms`);
    if (latency > 100) {
      console.warn(`    High latency detected for ${type}`);
    }
  });

  // Step 7: Get cache statistics
  const cacheStats = provenanceChainCache.getStats();
  console.log('\nCache Statistics:');
  console.log(`  Size: ${cacheStats.size}/${cacheStats.maxSize}`);
  console.log(`  Hit Rate: ${(cacheStats.hitRate * 100).toFixed(1)}%`);
  console.log(`  Hits: ${cacheStats.hitCount}`);
  console.log(`  Misses: ${cacheStats.missCount}`);

  // Step 8: Stop monitoring (optional)
  // performanceMonitoringService.stopMonitoring();

  return metrics;
}

// Run the example
optimizePerformance()
  .then(metrics => {
    console.log('\nPerformance optimization completed');
    console.log('Final FPS:', metrics.fps.toFixed(1));
  })
  .catch(error => {
    console.error('Performance optimization failed:', error);
  });

