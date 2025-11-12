---
id: troubleshooting-guide-federated-provenance-canvas
title: "Troubleshooting Guide: Federated Provenance Canvas Integration"
level: intermediate
type: guide
tags: [troubleshooting, debugging, common-issues, error-handling, performance-issues]
keywords: [troubleshooting, debugging, common-issues, error-handling, performance-issues, worker-errors, rendering-problems]
prerequisites: [implementation-details-federated-provenance-canvas, api-reference-federated-provenance-canvas]
enables: [workflow-walkthroughs-federated-provenance-canvas]
related: [federated-provenance-canvas-integration-docs, developer-guide-federated-provenance-canvas, implementation-details-federated-provenance-canvas]
readingTime: 30
difficulty: 3
version: "1.0.0"
blackboard:
  status: active
  assignedAgent: "6D-Intelligence-Agent"
  lastUpdate: "2025-01-07"
  dependencies: [implementation-details-federated-provenance-canvas, api-reference-federated-provenance-canvas]
  watchers: ["4D-Network-Agent", "Query-Interface-Agent"]
---

# Troubleshooting Guide: Federated Provenance Canvas Integration

**Date**: 2025-01-07  
**Version**: 1.0.0

## Overview

This guide provides solutions to common issues encountered when using the Federated Provenance Canvas Integration. It covers error messages, debugging tips, and performance optimization strategies.

## Table of Contents

1. [Common Issues](#common-issues)
2. [Worker Initialization Failures](#worker-initialization-failures)
3. [Provenance Chain Building Errors](#provenance-chain-building-errors)
4. [Performance Issues](#performance-issues)
5. [Rendering Problems](#rendering-problems)
6. [Debugging Tips](#debugging-tips)
7. [Error Message Reference](#error-message-reference)

## Common Issues

### Issue: Service Not Initialized

**Symptoms**:
- Error: "Worker not initialized"
- Methods fail with initialization errors

**Solution**:
```typescript
// Always initialize services before use
await provenanceSlideService.init();

// Check initialization status
if (workerService.isInitialized()) {
  workerService.loadProvenanceChain(chain);
} else {
  console.error('Worker not initialized');
}
```

**Prevention**: Always call `init()` before using service methods.

### Issue: OffscreenCanvas Not Supported

**Symptoms**:
- Error: "OffscreenCanvas not supported in this browser"
- Fallback mode activated

**Solution**:
```typescript
// Check support before initialization
if (ProvenanceCanvasWorkerService.isSupported()) {
  await workerService.init(canvas, options);
} else {
  // Use 2D fallback rendering
  console.warn('OffscreenCanvas not supported, using 2D fallback');
}
```

**Prevention**: Check browser compatibility before attempting worker initialization.

### Issue: Cache Misses

**Symptoms**:
- Slow performance on repeated operations
- Cache statistics show low hit rate

**Solution**:
```typescript
// Check cache statistics
const stats = provenanceChainCache.getStats();
console.log(`Hit rate: ${(stats.hitRate * 100).toFixed(1)}%`);

// Clear and rebuild cache if needed
if (stats.hitRate < 0.5) {
  provenanceChainCache.clear();
  // Rebuild cache with fresh data
}
```

**Prevention**: Use consistent cache keys and ensure cache is properly configured.

## Worker Initialization Failures

### Issue: Worker Creation Fails

**Symptoms**:
- Error: "Failed to initialize worker"
- Worker falls back to 2D-only mode

**Causes**:
1. Worker file not found
2. Module import errors
3. Browser compatibility issues

**Solution**:
```typescript
try {
  await workerService.init(canvas, options);
} catch (error) {
  if (error instanceof WorkerError) {
    console.error('Worker initialization failed:', error.message);
    console.log('Fallback mode:', error.fallbackMode);
    
    // Use fallback rendering
    if (error.fallbackMode === '2d-only') {
      // Implement 2D rendering fallback
    }
  }
}
```

**Debugging**:
1. Check browser console for module import errors
2. Verify worker file path is correct
3. Check browser compatibility (Chrome 69+, Firefox 105+, Safari 16.4+)

### Issue: Worker Crashes

**Symptoms**:
- Worker stops responding
- Rendering freezes
- Error messages in console

**Solution**:
```typescript
// Worker has automatic recovery
// Monitor recovery attempts
workerService.onMessage('recovered', (data) => {
  console.log('Worker recovered after', data.attempts, 'attempts');
});

// Manual recovery if needed
if (!workerService.isInitialized()) {
  // Worker will attempt recovery automatically
  // Or manually reinitialize
  await workerService.init(canvas, options);
}
```

**Prevention**:
- Monitor worker errors
- Implement proper error boundaries
- Use performance monitoring to detect issues early

## Provenance Chain Building Errors

### Issue: Evolution Files Not Found

**Symptoms**:
- Error: "Failed to load evolution files"
- Empty provenance chain

**Solution**:
```typescript
try {
  const chain = await provenanceSlideService.buildProvenanceChain(evolutionPath);
} catch (error) {
  // Check if path is correct
  console.error('Evolution path:', evolutionPath);
  
  // Try alternative loading methods
  const files = await provenanceSlideService.loadEvolutionFilesPaginated(evolutionPath, {
    page: 0,
    pageSize: 100
  });
  
  if (files.files.length === 0) {
    console.error('No evolution files found at path');
  }
}
```

**Debugging**:
1. Verify evolution path is correct
2. Check file permissions
3. Verify files contain valid JSONL/CanvasL format
4. Check database service is properly configured

### Issue: Pattern Extraction Fails

**Symptoms**:
- Error: "Failed to extract patterns"
- Missing nodes in provenance chain

**Solution**:
```typescript
// Check file format
const files = await loadEvolutionFiles(evolutionPath);
files.forEach(file => {
  if (!file.selfReference && !file.metadata?.selfReference) {
    console.warn('File missing selfReference:', file);
  }
});

// Validate pattern extraction
const patterns = await extractSelfExecutionPatternsWithProvenance(files);
console.log(`Extracted ${patterns.length} patterns`);
```

**Prevention**:
- Ensure all evolution files have `selfReference` metadata
- Validate JSONL/CanvasL format before processing
- Check federated provenance queries are working

### Issue: Federated Provenance Queries Fail

**Symptoms**:
- Error: "Failed to query federated provenance"
- Missing provenance history

**Solution**:
```typescript
// Check agent provenance query service
try {
  const query: FederatedProvenanceQuery = {
    files: [filePath],
    query: 'SELECT ?provenance WHERE { ?entry prov:wasDerivedFrom ?provenance }',
    queryType: QueryType.SPARQL
  };
  
  const results = await agentProvenanceQueryService.queryFederatedProvenance(query);
  console.log('Provenance results:', results);
} catch (error) {
  console.error('Federated provenance query failed:', error);
  // Use fallback: selfReference as provenance
}
```

**Debugging**:
1. Verify SPARQL query syntax
2. Check database service is accessible
3. Verify file paths in queries are correct
4. Check network connectivity for remote queries

## Performance Issues

### Issue: Low FPS

**Symptoms**:
- FPS drops below 30
- Rendering is choppy
- Performance warnings appear

**Solution**:
```typescript
// Monitor FPS
performanceMonitoringService.startMonitoring();

// Check FPS
const fps = performanceMonitoringService.getFPS();
const avgFps = performanceMonitoringService.getAverageFPS();

if (fps < 30) {
  console.warn('Low FPS detected:', fps);
  
  // Check node/edge counts
  const metrics = performanceMonitoringService.getMetrics();
  console.log('Node count:', metrics.nodeCount);
  console.log('Edge count:', metrics.edgeCount);
  
  // Optimize rendering
  if (metrics.nodeCount > 1000) {
    // Enable LOD (Level of Detail)
    // Enable frustum culling
    // Reduce rendering quality
  }
}
```

**Optimization Strategies**:
1. Enable LOD for large graphs
2. Use instancing for similar nodes
3. Enable frustum culling
4. Reduce rendering quality for distant objects
5. Paginate large evolution directories

### Issue: High Memory Usage

**Symptoms**:
- Memory warnings
- Browser becomes slow
- Out of memory errors

**Solution**:
```typescript
// Monitor memory
const memory = performanceMonitoringService.getMemoryUsage();
console.log('Memory used:', (memory.used / 1024 / 1024).toFixed(2), 'MB');

if (memory.used > 100 * 1024 * 1024) {
  // Clear caches
  provenanceChainCache.clear();
  avatarLoaderService.clearCache();
  thoughtCardService.clearAll();
  
  // Force garbage collection (if available)
  if (global.gc) {
    global.gc();
  }
}
```

**Prevention**:
- Implement proper cache eviction
- Clear unused resources
- Use pagination for large datasets
- Monitor memory usage regularly

### Issue: Slow Message Latency

**Symptoms**:
- High latency warnings
- Slow worker communication
- Delayed rendering updates

**Solution**:
```typescript
// Track message latency
const messageId = performanceMonitoringService.trackMessageStart('load');
await workerService.loadProvenanceChain(chain);
performanceMonitoringService.trackMessageEnd(messageId);

// Check latency
const latency = performanceMonitoringService.getMessageLatency('load');
if (latency > 100) {
  console.warn('High message latency:', latency, 'ms');
  
  // Optimize message size
  // Reduce message frequency
  // Batch operations
}
```

**Optimization**:
1. Reduce message payload size
2. Batch multiple operations
3. Use transferable objects for large data
4. Optimize worker processing

## Rendering Problems

### Issue: Avatars Not Loading

**Symptoms**:
- Avatars don't appear
- GLTF loading errors
- Fallback models used

**Solution**:
```typescript
// Check avatar loader
try {
  const model = await avatarLoaderService.loadAvatar({
    gltfModel: '/evolutions/angelica.glb',
    scale: [0.5, 0.5, 0.5],
    type: 'human'
  });
} catch (error) {
  console.error('Avatar loading failed:', error);
  
  // Check file path
  // Verify GLTF file is valid
  // Check CORS settings
  // Verify file is in public directory
}

// Check cache
const stats = avatarLoaderService.getCacheStats();
console.log('Avatar cache:', stats);
```

**Debugging**:
1. Verify GLTF file paths are correct
2. Check files are in `public/evolutions/` directory
3. Verify CORS settings for external files
4. Check browser console for loading errors
5. Validate GLTF file format

### Issue: Thought Cards Not Displaying

**Symptoms**:
- Thought cards don't appear
- Cards appear but are empty
- Texture generation fails

**Solution**:
```typescript
// Check thought card creation
const card = thoughtCardService.createThoughtCardFromNode(node);
if (!card) {
  console.warn('Failed to create thought card for node:', node.id);
  return;
}

// Check texture generation
const texture = thoughtCardService.getCardTexture(card.id);
if (!texture) {
  console.warn('Texture not found for card:', card.id);
  // Regenerate texture
  thoughtCardService.createCardTexture(card);
}
```

**Debugging**:
1. Verify node has avatar configuration
2. Check card content is not empty
3. Verify canvas context is available
4. Check texture cache

### Issue: Knowledge Graphs Not Rendering

**Symptoms**:
- SVG not generated
- Graph appears empty
- Rendering errors

**Solution**:
```typescript
// Check knowledge graph creation
const card = knowledgeGraphCardService.buildKnowledgeGraph(slide, agentId);
console.log('Knowledge graph nodes:', card.nodes.length);
console.log('Knowledge graph edges:', card.edges.length);

// Check SVG rendering
const svg = knowledgeGraphCardService.renderKnowledgeGraph(card);
if (!svg || svg.children.length === 0) {
  console.error('SVG rendering failed');
  // Check node/edge data
}
```

**Debugging**:
1. Verify slide has provenance chain
2. Check agent ID exists in chain
3. Verify node/edge data is valid
4. Check SVG element creation

## Debugging Tips

### 1. Enable Debug Logging

```typescript
// Enable verbose logging in development
if (import.meta.env.DEV) {
  console.log('Provenance chain:', chain);
  console.log('Slides:', slides);
  console.log('Performance metrics:', performanceMonitoringService.getMetrics());
}
```

### 2. Use Performance Monitoring

```typescript
// Start monitoring
performanceMonitoringService.startMonitoring();

// Get comprehensive metrics
const metrics = performanceMonitoringService.getMetrics();
console.log('Performance Metrics:', {
  fps: metrics.fps,
  averageFps: metrics.averageFps,
  memory: metrics.memoryUsage,
  warnings: metrics.warnings
});
```

### 3. Check Service State

```typescript
// Check service initialization
console.log('Slide service initialized:', provenanceSlideService !== null);
console.log('Worker initialized:', workerService.isInitialized());
console.log('Worker fallback mode:', workerService.getFallbackMode());

// Check cache state
const cacheStats = provenanceChainCache.getStats();
console.log('Cache stats:', cacheStats);
```

### 4. Validate Data Structures

```typescript
// Validate provenance chain
function validateChain(chain: ProvenanceChain): boolean {
  if (!chain.nodes || !Array.isArray(chain.nodes)) {
    console.error('Invalid chain: missing nodes array');
    return false;
  }
  
  if (!chain.edges || !Array.isArray(chain.edges)) {
    console.error('Invalid chain: missing edges array');
    return false;
  }
  
  // Check node IDs are unique
  const nodeIds = new Set(chain.nodes.map(n => n.id));
  if (nodeIds.size !== chain.nodes.length) {
    console.error('Invalid chain: duplicate node IDs');
    return false;
  }
  
  return true;
}
```

## Error Message Reference

### Worker Errors

| Error | Cause | Solution |
|-------|-------|----------|
| `Worker not initialized` | Service not initialized | Call `init()` before use |
| `OffscreenCanvas not supported` | Browser compatibility | Use 2D fallback or update browser |
| `Worker error occurred` | Worker crash | Check worker code, enable recovery |
| `Timeout waiting for message` | Worker not responding | Increase timeout or check worker |

### Provenance Chain Errors

| Error | Cause | Solution |
|-------|-------|----------|
| `Failed to load evolution files` | Path incorrect or files missing | Verify path and file existence |
| `Failed to extract patterns` | Invalid file format | Validate JSONL/CanvasL format |
| `Federated provenance query failed` | Database/network issue | Check database service and network |
| `Invalid provenance chain` | Data structure issue | Validate chain structure |

### Performance Warnings

| Warning | Cause | Solution |
|---------|-------|----------|
| `Low FPS detected` | Too many objects or complex rendering | Enable LOD, reduce quality |
| `High memory usage` | Too much data cached | Clear caches, use pagination |
| `High message latency` | Large messages or slow worker | Optimize message size, batch operations |
| `Large node count` | Too many nodes | Enable instancing, use LOD |

## Related Documentation

- [Implementation Details](./03-IMPLEMENTATION-DETAILS.md) - Technical architecture
- [API Reference](./04-API-REFERENCE.md) - Complete API documentation
- [Developer Guide](./05-DEVELOPER-GUIDE.md) - Extension patterns
- [Workflow Walkthroughs](./07-WORKFLOW-WALKTHROUGHS.md) - Step-by-step workflows

---

**Last Updated**: 2025-01-07  
**Version**: 1.0.0

