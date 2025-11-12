---
id: workflow-walkthroughs-federated-provenance-canvas
title: "Workflow Walkthroughs: Federated Provenance Canvas Integration"
level: intermediate
type: guide
tags: [workflow-walkthroughs, step-by-step, tutorials, video-scripts, user-guides]
keywords: [workflow-walkthroughs, step-by-step, tutorials, video-scripts, user-guides, provenance-chain, slide-generation, worker-setup]
prerequisites: [federated-provenance-canvas-integration-docs, api-reference-federated-provenance-canvas]
enables: []
related: [federated-provenance-canvas-integration-docs, developer-guide-federated-provenance-canvas, troubleshooting-guide-federated-provenance-canvas]
readingTime: 60
difficulty: 2
version: "1.0.0"
blackboard:
  status: active
  assignedAgent: "6D-Intelligence-Agent"
  lastUpdate: "2025-01-07"
  dependencies: [federated-provenance-canvas-integration-docs, api-reference-federated-provenance-canvas]
  watchers: ["4D-Network-Agent", "Query-Interface-Agent"]
---

# Workflow Walkthroughs: Federated Provenance Canvas Integration

**Date**: 2025-01-07  
**Version**: 1.0.0

## Overview

This document provides detailed, step-by-step walkthroughs for common workflows in the Federated Provenance Canvas Integration. These walkthroughs can be used as scripts for video tutorials or as text-based guides.

## Table of Contents

1. [Building a Provenance Chain from Evolution Directory](#workflow-1-building-a-provenance-chain)
2. [Generating Slides and Cards](#workflow-2-generating-slides-and-cards)
3. [Setting Up Offscreen Canvas Worker](#workflow-3-setting-up-offscreen-canvas-worker)
4. [Integrating 3D Avatars with Thought Cards](#workflow-4-integrating-3d-avatars)
5. [Creating Knowledge Graph Cards](#workflow-5-creating-knowledge-graph-cards)
6. [Performance Optimization Workflow](#workflow-6-performance-optimization)
7. [Debugging a Failing Provenance Chain](#workflow-7-debugging-a-failing-chain)

## Workflow 1: Building a Provenance Chain from Evolution Directory

### Overview

This workflow demonstrates how to build a provenance chain from an evolution directory containing automaton files.

### Step-by-Step Instructions

**Step 1: Initialize the Service**

```typescript
import { provenanceSlideService } from '@/services/provenance-slide-service';

// Initialize the service
await provenanceSlideService.init();
console.log('Service initialized');
```

**Expected Outcome**: Service is ready to use.

**Step 2: Specify Evolution Directory**

```typescript
const evolutionPath = '/evolutions/advanced-automaton';
console.log('Evolution path:', evolutionPath);
```

**Expected Outcome**: Path is defined and ready.

**Step 3: Build Provenance Chain**

```typescript
const chain = await provenanceSlideService.buildProvenanceChain(evolutionPath);
console.log(`Chain built: ${chain.nodes.length} nodes, ${chain.edges.length} edges`);
```

**Expected Outcome**: Provenance chain with nodes and edges.

**Step 4: Verify Chain Structure**

```typescript
// Check nodes
chain.nodes.forEach(node => {
  console.log(`Node: ${node.id}, Dimension: ${node.metadata.dimension}`);
});

// Check edges
chain.edges.forEach(edge => {
  console.log(`Edge: ${edge.from} -> ${edge.to}, Type: ${edge.type}`);
});
```

**Expected Outcome**: Chain structure is validated.

### Common Pitfalls

- **Pitfall**: Evolution path doesn't exist
  - **Solution**: Verify path is correct and files are accessible
- **Pitfall**: Files missing selfReference metadata
  - **Solution**: Ensure all files have proper selfReference structure
- **Pitfall**: Timeout errors
  - **Solution**: Use pagination for large directories

### Video Script Notes

- Show file structure in evolution directory
- Demonstrate chain visualization
- Highlight node and edge relationships
- Show dimensional progression (0D→7D→0D)

## Workflow 2: Generating Slides and Cards

### Overview

This workflow demonstrates how to generate slides and cards from a provenance chain.

### Step-by-Step Instructions

**Step 1: Generate Slides**

```typescript
const slides = await provenanceSlideService.generateSlidesFromEvolution(evolutionPath);
console.log(`Generated ${slides.length} slides`);
```

**Expected Outcome**: Array of slides, one per dimension.

**Step 2: Inspect Slide Structure**

```typescript
slides.forEach(slide => {
  console.log(`Slide: ${slide.dimension}`);
  console.log(`  Nodes: ${slide.provenanceChain?.nodes.length || 0}`);
  console.log(`  Cards: ${slide.cards.length}`);
});
```

**Expected Outcome**: Slide information displayed.

**Step 3: Access Cards**

```typescript
slides.forEach(slide => {
  slide.cards.forEach(card => {
    console.log(`Card: ${card.pattern}, Lines: ${card.jsonlLines.length}`);
  });
});
```

**Expected Outcome**: Card information displayed.

**Step 4: Use Slide Content**

```typescript
slides.forEach(slide => {
  // Display slide content (markdown)
  console.log(slide.content);
  
  // Use provenance chain for rendering
  const chain = slide.provenanceChain;
  // Render chain in 3D
});
```

**Expected Outcome**: Slides ready for display.

### Common Pitfalls

- **Pitfall**: Empty slides
  - **Solution**: Verify provenance chain has nodes
- **Pitfall**: Missing cards
  - **Solution**: Check pattern extraction is working
- **Pitfall**: Invalid slide content
  - **Solution**: Verify Church encoding and BQF calculations

### Video Script Notes

- Show slide generation process
- Demonstrate dimensional progression
- Display card grouping by pattern
- Show slide content rendering

## Workflow 3: Setting Up Offscreen Canvas Worker

### Overview

This workflow demonstrates how to set up and use the offscreen canvas worker for 3D rendering.

### Step-by-Step Instructions

**Step 1: Check Browser Support**

```typescript
import { ProvenanceCanvasWorkerService } from '@/services/provenance-canvas-worker-service';

if (!ProvenanceCanvasWorkerService.isSupported()) {
  console.warn('OffscreenCanvas not supported, using 2D fallback');
  // Use 2D rendering
}
```

**Expected Outcome**: Support status determined.

**Step 2: Get Canvas Element**

```typescript
const canvas = document.getElementById('provenance-canvas') as HTMLCanvasElement;
const offscreenCanvas = canvas.transferControlToOffscreen();
```

**Expected Outcome**: Offscreen canvas ready.

**Step 3: Initialize Worker**

```typescript
const workerService = new ProvenanceCanvasWorkerService();

await workerService.init(offscreenCanvas, {
  width: canvas.width,
  height: canvas.height,
  antialias: true
});

console.log('Worker initialized');
```

**Expected Outcome**: Worker is ready.

**Step 4: Load Provenance Chain**

```typescript
const chain = await provenanceSlideService.buildProvenanceChain(evolutionPath);
workerService.loadProvenanceChain(chain);
console.log('Chain loaded into worker');
```

**Expected Outcome**: Chain is being rendered.

**Step 5: Handle Interactions**

```typescript
canvas.addEventListener('click', async (event) => {
  const node = await workerService.handleInteraction(
    event.clientX,
    event.clientY,
    canvas.width,
    canvas.height,
    'click'
  );
  
  if (node) {
    console.log('Selected node:', node.id);
  }
});
```

**Expected Outcome**: Interactions are handled.

### Common Pitfalls

- **Pitfall**: Worker initialization fails
  - **Solution**: Check browser compatibility, verify worker file path
- **Pitfall**: Canvas not rendering
  - **Solution**: Verify canvas dimensions, check worker messages
- **Pitfall**: Interactions not working
  - **Solution**: Verify coordinate normalization, check message handlers

### Video Script Notes

- Show browser compatibility check
- Demonstrate worker initialization
- Display 3D rendering
- Show interaction handling

## Workflow 4: Integrating 3D Avatars with Thought Cards

### Overview

This workflow demonstrates how to integrate 3D avatars with thought cards for agent visualization.

### Step-by-Step Instructions

**Step 1: Preload Avatar Templates**

```typescript
import { avatarLoaderService } from '@/services/avatar-loader-service';

await avatarLoaderService.preloadTemplates();
console.log('Avatars preloaded');
```

**Expected Outcome**: Avatars are cached and ready.

**Step 2: Build Provenance Chain with Avatars**

```typescript
const chain = await provenanceSlideService.buildProvenanceChain(evolutionPath);
// Avatars are automatically assigned to agent nodes
```

**Expected Outcome**: Chain nodes have avatar configurations.

**Step 3: Create Thought Cards**

```typescript
import { thoughtCardService } from '@/services/thought-card-service';

const thoughtCards = chain.nodes
  .filter(n => n.avatar)
  .map(node => thoughtCardService.createThoughtCardFromNode(node))
  .filter(Boolean);

console.log(`Created ${thoughtCards.length} thought cards`);
```

**Expected Outcome**: Thought cards created for agent nodes.

**Step 4: Render Avatars and Cards**

```typescript
// In your React component
import { ProvenanceAvatar } from '@/components/UnifiedProvenanceCanvas/ProvenanceAvatar';
import { ThoughtCard3D } from '@/components/UnifiedProvenanceCanvas/ThoughtCard3D';

chain.nodes.forEach(node => {
  if (node.avatar) {
    // Render avatar
    <ProvenanceAvatar
      node={node}
      avatarConfig={node.avatar}
    />
    
    // Render thought card
    const card = thoughtCardService.getThoughtCardsForAvatar(node.id)[0];
    if (card) {
      <ThoughtCard3D
        card={card}
        avatarPosition={node.position}
      />
    }
  }
});
```

**Expected Outcome**: Avatars and thought cards are rendered.

### Common Pitfalls

- **Pitfall**: Avatars not loading
  - **Solution**: Verify GLTF file paths, check CORS settings
- **Pitfall**: Thought cards not appearing
  - **Solution**: Verify card creation, check texture generation
- **Pitfall**: Cards positioned incorrectly
  - **Solution**: Verify offset calculations, check camera position

### Video Script Notes

- Show avatar loading process
- Demonstrate thought card creation
- Display 3D scene with avatars and cards
- Show card positioning relative to avatars

## Workflow 5: Creating Knowledge Graph Cards

### Overview

This workflow demonstrates how to create and render 2D knowledge graph cards for agent thought processes.

### Step-by-Step Instructions

**Step 1: Generate Slides**

```typescript
const slides = await provenanceSlideService.generateSlidesFromEvolution(evolutionPath);
```

**Expected Outcome**: Slides with provenance chains.

**Step 2: Extract Agent IDs**

```typescript
const agentIds = new Set<string>();
slides.forEach(slide => {
  slide.provenanceChain?.nodes.forEach(node => {
    if (node.metadata.agentId) {
      agentIds.add(node.metadata.agentId);
    }
  });
});

console.log('Agent IDs:', Array.from(agentIds));
```

**Expected Outcome**: List of unique agent IDs.

**Step 3: Build Knowledge Graphs**

```typescript
import { knowledgeGraphCardService } from '@/services/knowledge-graph-card-service';

const knowledgeGraphs = slides.flatMap(slide =>
  Array.from(agentIds).map(agentId =>
    knowledgeGraphCardService.buildKnowledgeGraph(slide, agentId)
  )
);

console.log(`Created ${knowledgeGraphs.length} knowledge graphs`);
```

**Expected Outcome**: Knowledge graph cards for all agents.

**Step 4: Render as SVG**

```typescript
knowledgeGraphs.forEach(card => {
  const svg = knowledgeGraphCardService.renderKnowledgeGraph(card);
  
  // Display SVG in DOM
  document.body.appendChild(svg);
  
  // Or use in component
  // <KnowledgeGraphCard2D card={card} />
});
```

**Expected Outcome**: SVG knowledge graphs displayed.

### Common Pitfalls

- **Pitfall**: Empty knowledge graphs
  - **Solution**: Verify agent nodes exist in slides
- **Pitfall**: SVG not rendering
  - **Solution**: Check SVG element creation, verify node/edge data
- **Pitfall**: Layout issues
  - **Solution**: Verify node positions, check SVG viewBox

### Video Script Notes

- Show knowledge graph extraction
- Demonstrate graph building process
- Display SVG rendering
- Show graph layout and relationships

## Workflow 6: Performance Optimization Workflow

### Overview

This workflow demonstrates how to monitor and optimize performance.

### Step-by-Step Instructions

**Step 1: Start Performance Monitoring**

```typescript
import { performanceMonitoringService } from '@/services/performance-monitoring-service';

performanceMonitoringService.startMonitoring();
console.log('Performance monitoring started');
```

**Expected Outcome**: Monitoring is active.

**Step 2: Monitor Metrics**

```typescript
// Get current metrics
const metrics = performanceMonitoringService.getMetrics();

console.log('FPS:', metrics.fps);
console.log('Average FPS:', metrics.averageFps);
console.log('Memory:', (metrics.memoryUsage.used / 1024 / 1024).toFixed(2), 'MB');
console.log('Warnings:', metrics.warnings.length);
```

**Expected Outcome**: Performance metrics displayed.

**Step 3: Check Warnings**

```typescript
const warnings = performanceMonitoringService.getWarnings();
warnings.forEach(warning => {
  console.warn(`${warning.type}: ${warning.message}`);
});
```

**Expected Outcome**: Warnings identified.

**Step 4: Optimize Based on Metrics**

```typescript
// If FPS is low
if (metrics.fps < 30) {
  // Enable LOD
  // Reduce rendering quality
  // Enable frustum culling
}

// If memory is high
if (metrics.memoryUsage.used > 100 * 1024 * 1024) {
  // Clear caches
  provenanceChainCache.clear();
  avatarLoaderService.clearCache();
}

// If node count is high
if (metrics.nodeCount > 1000) {
  // Use instancing
  // Enable LOD
  // Paginate data
}
```

**Expected Outcome**: Performance optimized.

### Common Pitfalls

- **Pitfall**: Not monitoring performance
  - **Solution**: Always start monitoring, check metrics regularly
- **Pitfall**: Ignoring warnings
  - **Solution**: Address warnings promptly, implement optimizations
- **Pitfall**: Over-optimization
  - **Solution**: Balance performance with functionality

### Video Script Notes

- Show performance monitoring setup
- Demonstrate metric collection
- Display optimization strategies
- Show before/after performance comparison

## Workflow 7: Debugging a Failing Provenance Chain

### Overview

This workflow demonstrates how to debug issues when building a provenance chain fails.

### Step-by-Step Instructions

**Step 1: Check Error Message**

```typescript
try {
  const chain = await provenanceSlideService.buildProvenanceChain(evolutionPath);
} catch (error) {
  console.error('Error:', error.message);
  console.error('Stack:', error.stack);
}
```

**Expected Outcome**: Error information captured.

**Step 2: Verify Evolution Path**

```typescript
// Check if path exists
const fileCount = await provenanceSlideService.getEvolutionFileCount(evolutionPath);
console.log('File count:', fileCount);

if (fileCount === 0) {
  console.error('No files found at path:', evolutionPath);
}
```

**Expected Outcome**: Path validity confirmed.

**Step 3: Test File Loading**

```typescript
// Try loading files with pagination
const page1 = await provenanceSlideService.loadEvolutionFilesPaginated(evolutionPath, {
  page: 0,
  pageSize: 10
});

console.log('Files loaded:', page1.files.length);
console.log('Total files:', page1.total);

if (page1.files.length === 0) {
  console.error('No files could be loaded');
}
```

**Expected Outcome**: File loading verified.

**Step 4: Validate File Format**

```typescript
page1.files.forEach(file => {
  // Check for selfReference
  if (!file.selfReference && !file.metadata?.selfReference) {
    console.warn('File missing selfReference:', file);
  }
  
  // Validate JSON structure
  try {
    JSON.parse(JSON.stringify(file));
  } catch (e) {
    console.error('Invalid JSON in file:', file);
  }
});
```

**Expected Outcome**: File format validated.

**Step 5: Test Pattern Extraction**

```typescript
// Manually test pattern extraction
const patterns = await provenanceSlideService.extractSelfExecutionPatternsWithProvenance(
  page1.files
);

console.log('Patterns extracted:', patterns.length);
patterns.forEach(pattern => {
  console.log(`Pattern: ${pattern.pattern}, Dimension: ${pattern.dimension}`);
});
```

**Expected Outcome**: Pattern extraction verified.

**Step 6: Check Cache**

```typescript
// Check cache state
const cacheStats = provenanceChainCache.getStats();
console.log('Cache stats:', cacheStats);

// Try clearing cache
provenanceChainCache.clear();

// Retry chain building
const chain = await provenanceSlideService.buildProvenanceChain(evolutionPath);
```

**Expected Outcome**: Issue resolved or further debugging needed.

### Common Pitfalls

- **Pitfall**: Not checking error messages
  - **Solution**: Always log error details
- **Pitfall**: Assuming path is correct
  - **Solution**: Verify path and file existence
- **Pitfall**: Not validating file format
  - **Solution**: Check file structure before processing

### Video Script Notes

- Show error identification
- Demonstrate debugging steps
- Display validation checks
- Show resolution process

## Related Documentation

- [Developer Guide](./05-DEVELOPER-GUIDE.md) - Extension patterns
- [Troubleshooting Guide](./06-TROUBLESHOOTING-GUIDE.md) - Common issues
- [API Reference](./04-API-REFERENCE.md) - Complete API documentation
- [Implementation Details](./03-IMPLEMENTATION-DETAILS.md) - Technical architecture

---

**Last Updated**: 2025-01-07  
**Version**: 1.0.0

