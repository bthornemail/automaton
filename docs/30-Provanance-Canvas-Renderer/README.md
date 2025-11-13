---
id: provenance-canvas-renderer-docs-readme
title: "Provenance Canvas Renderer Documentation"
level: foundational
type: documentation
tags: [provenance-canvas-renderer, performance-optimization, rendering, worker, documentation, webgl, gltf, svg, avatars, computational-manifold, a-frame, slide-editing, search-filtering, export, testing]
keywords: [provenance-canvas-renderer, performance-optimization, pagination, caching, memoization, virtual-scrolling, worker-rendering, lod, frustum-culling, performance-monitoring, webgl, gltf-avatars, svg-textures, computational-manifold, a-frame, networked-aframe, slide-editing, card-details, search-filtering, export-formats, testing]
prerequisites: [federated-provenance-canvas-integration-docs, canvasl-rfc2119-spec, federated-provenance-meta-log-spec, webgl-glft-svg-avatars-analysis]
enables: [provenance-canvas-renderer-meta-specification-rfc2119, provenance-canvas-renderer-rfc2119-spec, provenance-canvas-renderer-protocol-specification-rfc2119, rendering-evolution-documentation]
related: [federated-provenance-canvas-integration-docs, canvasl-rfc2119-spec, federated-provenance-meta-log-spec, bipartite-bqf-extension-rfc2119-spec, webgl-glft-svg-avatars-analysis, webgl-computational-manifold-architecture]
readingTime: 15
difficulty: 3
version: "1.0.0"
blackboard:
  status: completed
  assignedAgent: "6D-Intelligence-Agent"
  lastUpdate: "2025-01-07"
  dependencies: [federated-provenance-canvas-integration-docs, canvasl-rfc2119-spec, federated-provenance-meta-log-spec]
  watchers: ["4D-Network-Agent", "Visualization-Agent"]
---

# Provenance Canvas Renderer Documentation

**Status**: ✅ **COMPLETED**  
**Date**: 2025-01-07  
**Version**: 1.1.0

## Overview

The Provenance Canvas Renderer provides high-performance rendering of federated provenance chains with comprehensive performance optimizations. The system enables efficient visualization of large-scale evolution directories with real-time performance metrics, supporting pagination, caching, memoization, virtual scrolling, worker rendering optimizations, and performance monitoring. The system also includes interactive slide editing, comprehensive card detail views, powerful search and filtering capabilities, and multi-format export functionality.

## Documentation Structure

### 1. [Meta-Specification](./00-META-SPECIFICATION-RFC2119.md)
Coordinates all specifications, defines versioning strategy, and establishes immutability policies.

### 2. [Renderer Specification](./01-PROVENANCE-CANVAS-RENDERER-RFC2119-SPEC.md)
Complete RFC2119 specification covering architecture, performance optimizations, API requirements, and implementation requirements.

### 3. [Protocol Specification](./02-PROTOCOL-SPECIFICATION-RFC2119.md)
Protocol for renderer operations, message formats, performance monitoring, and error handling.

### 4. [Rendering Evolution](./03-RENDERING-EVOLUTION.md)
Evolution documentation tracking the progression from basic rendering to full WebGL/GLTF/SVG system.

### 5. [Testing Documentation](./04-TESTING.md)
Comprehensive testing documentation covering unit tests, integration tests, and test coverage.

## Key Features

### ✅ Performance Optimizations

#### Provenance Chain Building
- **Pagination**: Support for large evolution directories with configurable page size
- **Caching**: LRU cache with maximum 50 chains, automatic eviction
- **Pattern Extraction**: Optimized with Map-based grouping and batched queries
- **Lazy Loading**: On-demand provenance history loading

#### Slide/Card Generation
- **Memoization**: Cached slide content generation with cache key generation
- **Card Aggregation**: Single-pass Map-based aggregation with early exit
- **Virtual Scrolling**: Efficient rendering for large card lists (>50 cards)
- **Debouncing**: 300ms delay for dimension filter changes

#### Worker Rendering
- **Instancing**: THREE.InstancedMesh for nodes >100
- **Edge Optimization**: Shared geometry, batched updates, distance culling
- **Level-of-Detail (LOD)**: 3 detail levels (high/medium/low) based on camera distance
- **Frustum Culling**: Efficient visibility culling for large scenes

### ✅ Performance Monitoring

- **FPS Tracking**: Current and average FPS over 60 frames
- **Memory Usage**: Total and breakdown by component (nodes, edges, textures, geometries, materials)
- **Worker Message Latency**: Per-message-type latency tracking
- **Performance Warnings**: Automatic warnings for low FPS, high memory, high latency, large scenes

### ✅ 3D Thought Cards for Avatars

The system supports 3D thought cards that appear near avatars in the 3D scene, rendered using offscreen canvas:

- **Billboard Planes**: Thought cards are rendered as 3D billboard planes that always face the camera
- **Dynamic Content**: Cards display agent thought processes extracted from node metadata
- **Positioning**: Cards can be positioned above, left, right, or behind avatars
- **Animations**: Gentle floating animations and fade effects
- **Offscreen Canvas**: Rendered using offscreen canvas for optimal performance

**Usage**:
```typescript
import { thoughtCardService } from '@/services/thought-card-service';

// Create thought card for avatar
const card = thoughtCardService.createThoughtCardFromNode(agentNode);
```

### ✅ 2D CanvasL Knowledge Graph Cards

The system supports 2D CanvasL knowledge graph cards that visualize agent thought processes:

- **Knowledge Graph Extraction**: Extracts thought processes from agent nodes
- **CanvasL Format**: Renders knowledge graphs in CanvasL format with nodes and edges
- **SVG Rendering**: Knowledge graphs are rendered as SVG for crisp 2D visualization
- **Interactive Exploration**: Cards support interactive exploration of thought processes
- **Multi-Agent Support**: Generates knowledge graphs for all agents in a slide

**Usage**:
```typescript
import { knowledgeGraphCardService } from '@/services/knowledge-graph-card-service';

// Build knowledge graph for agent
const kgCard = knowledgeGraphCardService.buildKnowledgeGraph(slide, agentId);

// Render as SVG
const svg = knowledgeGraphCardService.renderKnowledgeGraph(kgCard);
```

### ✅ Interactive Slide Editing

The system supports interactive editing of slides with full persistence:

- **Inline Editing**: Edit slide title, description, and content inline
- **Card Management**: Add and remove cards from slides
- **Slide Reordering**: Reorder slides with drag-and-drop interface
- **Save to Evolution**: Save edited slides back to evolution directory
- **Edit History**: Track all modifications with undo capability

**Usage**:
```typescript
import { slideEditingService } from '@/services/slide-editing-service';

// Initialize slides for editing
slideEditingService.initializeSlides(slides);

// Edit slide
const updated = slideEditingService.editSlide('slide-1', {
  title: 'Updated Title',
  description: 'Updated description'
});

// Add card
const newCard: Card = { id: 'card-1', pattern: 'identity', jsonlLines: [], metadata: {} };
slideEditingService.addCardToSlide('slide-1', newCard);

// Save to evolution directory
await slideEditingService.saveSlidesToEvolution('/evolutions/advanced-automaton');
```

### ✅ Card Detail Views

The system provides comprehensive card detail views with expandable sections:

- **Expandable Details**: Collapsible sections for card details, JSONL lines, provenance history, and pattern visualization
- **JSONL Line Viewer**: Formatted JSON display with syntax highlighting and line numbers
- **Provenance History Timeline**: Chronological timeline of provenance entries with file, line, pattern, and agent information
- **Pattern Visualization**: Visual representation of patterns with Church encoding and BQF forms

**Usage**:
```typescript
// Card detail view opens automatically when clicking a card
// No code needed - UI component handles display
```

### ✅ Provenance Chain Search and Filtering

The system provides powerful search and filtering capabilities:

- **Multi-Criteria Search**: Search by pattern, dimension, agent ID, node type, edge type, and file
- **Advanced Query Builder**: Combine multiple queries with AND/OR logic
- **Filter Presets**: Save, load, share, and delete filter presets
- **Real-time Filtering**: Apply filters in real-time to provenance chains
- **Results Summary**: Display filtered vs. total node/edge counts

**Usage**:
```typescript
import { provenanceSearchService } from '@/services/provenance-search-service';

// Search chain
const results = provenanceSearchService.searchChain(chain, {
  pattern: 'identity',
  dimension: '0D',
  agentId: '0D-Topology-Agent'
});

// Save filter preset
const presetId = provenanceSearchService.savePreset('0D Identity Nodes', {
  pattern: 'identity',
  dimension: '0D'
});

// Load preset
const preset = provenanceSearchService.getPreset(presetId);
const filtered = provenanceSearchService.searchChain(chain, preset.query);
```

### ✅ Export Provenance Chains

The system supports exporting provenance chains in multiple formats:

- **JSON**: Structured JSON format with nodes, edges, and metadata
- **JSONL**: JSON Lines format (one object per line)
- **GraphML**: XML format for graph visualization tools (yEd, Gephi)
- **DOT**: Graphviz DOT format for graph rendering
- **PNG**: Raster image export with customizable dimensions and background
- **SVG**: Vector image export with customizable dimensions and background

**Usage**:
```typescript
import { provenanceExportService } from '@/services/provenance-export-service';

// Export to JSON
await provenanceExportService.exportChain(chain, {
  format: 'json',
  filename: 'provenance-chain.json',
  includeMetadata: true
});

// Export to PNG
await provenanceExportService.exportChain(chain, {
  format: 'png',
  imageOptions: {
    width: 1920,
    height: 1080,
    backgroundColor: '#1f2937'
  }
});
```

### ✅ 3D Rendering Capabilities

#### GLTF Avatar Support
- **Human Avatars**: GLTF models loaded via GLTFLoader (e.g., DamagedHelmet.glb)
- **AI Agent Avatars**: Distinct GLTF models (e.g., Fox.glb scaled to 0.003)
- **Avatar Templates**: Networked templates for multiplayer synchronization
- **Dynamic Loading**: Avatars loaded from CDN or user uploads
- **Text Labels**: Avatar names displayed above avatars
- **Visual Identification**: Distinct colors for AI agents (green #00ff88)

#### SVG Dynamic Textures
- **SVG to Texture**: Convert SVG to base64 data URLs for WebGL texture loading
- **Real-time Updates**: SVG animated and updated every frame
- **Procedural UI**: SVG for scalable 2D/3D overlays
- **Infinite Worlds**: Support for infinite world generation (no pixelation)
- **Dynamic Topology**: Dynamic topology diagrams in the manifold

#### WebGL Computational Manifold
- **3D Spatial Encoding**: Map 8-type polynomial vectors to 3D coordinates
- **Evaluation Traces**: Render evaluation traces as 3D animations with particle effects
- **GLSL Shaders**: Polynomial visualization shaders (polynomial rings, combinator fields)
- **Perceptron Networks**: 3D perceptron network transitions as Bezier curves
- **Interactive Controls**: Interactive 3D evaluation controls
- **Multi-Strategy Comparison**: Side-by-side 3D viewports

#### A-Frame Integration
- **A-Frame Scene**: Declarative HTML-like syntax for VR/AR metaverse
- **Networked-A-Frame**: Multiplayer avatar synchronization
- **WebRTC Voice Chat**: Voice chat integration
- **WebXR Support**: VR/AR headset support

## Quick Start

### 1. Initialize Services

```typescript
import { provenanceSlideService } from '@/services/provenance-slide-service';
import { provenanceCanvasWorkerService } from '@/services/provenance-canvas-worker-service';
import { performanceMonitoringService } from '@/services/performance-monitoring-service';

// Initialize services
await provenanceSlideService.init();
await performanceMonitoringService.startMonitoring();
```

### 2. Build Provenance Chain with Pagination

```typescript
// Build chain with pagination for large directories
const chain = await provenanceSlideService.buildProvenanceChainPaginated(
  '/evolutions/advanced-automaton',
  { page: 1, pageSize: 100 }
);
```

### 3. Generate Slides with Memoization

```typescript
// Slides are automatically memoized
const slides = await provenanceSlideService.generateSlidesFromEvolution(
  '/evolutions/advanced-automaton'
);
```

### 4. Initialize Worker with Performance Monitoring

```typescript
// Initialize worker
const canvas = document.querySelector('canvas').transferControlToOffscreen();
await provenanceCanvasWorkerService.init(canvas, {
  width: 800,
  height: 600,
  antialias: true
});

// Load chain with optimizations
await provenanceCanvasWorkerService.loadProvenanceChain(chain, {
  useInstancing: true,
  useLOD: true,
  useFrustumCulling: true
});

// Monitor performance
performanceMonitoringService.onMetrics((metrics) => {
  console.log(`FPS: ${metrics.fps.current}`);
  console.log(`Memory: ${(metrics.memory.total / 1024 / 1024).toFixed(1)}MB`);
  
  if (metrics.warnings.length > 0) {
    console.warn('Performance warnings:', metrics.warnings);
  }
});
```

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│         UnifiedProvenanceCanvas Component              │
│  (MetaverseCanvas3D + DimensionalCanvas + Worker)      │
└─────────────────────────────────────────────────────────┘
                         │
        ┌────────────────┼────────────────┐
        │                │                │
┌───────▼──────┐  ┌──────▼──────┐  ┌──────▼──────┐
│ Provenance   │  │ Provenance  │  │ Performance │
│ Slide        │  │ Canvas      │  │ Monitoring  │
│ Service      │  │ Worker      │  │ Service     │
│              │  │ Service      │  │             │
└───────┬──────┘  └──────┬──────┘  └──────┬──────┘
        │                │                │
        └────────────────┼────────────────┘
                         │
        ┌────────────────┼────────────────┐
        │                │                │
┌───────▼──────┐  ┌──────▼──────┐  ┌──────▼──────┐
│ Provenance   │  │ Cache       │  │ Memoization │
│ Chain        │  │ Management  │  │ Utilities   │
│ Cache        │  │             │  │             │
└──────────────┘  └─────────────┘  └─────────────┘
```

## File Structure

```
ui/src/
├── services/
│   ├── provenance-slide-service.ts          ✅ Optimized with pagination, caching, memoization
│   ├── provenance-canvas-worker-service.ts  ✅ Integrated with performance monitoring
│   ├── performance-monitoring-service.ts   ✅ Performance monitoring
│   ├── provenance-chain-cache.ts            ✅ LRU cache for chains
│   ├── slide-editing-service.ts             ✅ NEW: Interactive slide editing
│   ├── provenance-search-service.ts         ✅ NEW: Search and filtering
│   └── provenance-export-service.ts        ✅ NEW: Export functionality
├── hooks/
│   └── useDebounce.ts                       ✅ Debounce hook
├── utils/
│   └── memoization.ts                       ✅ Memoization utilities
└── components/
    ├── shared/
    │   └── VirtualizedCardList.tsx          ✅ Virtual scrolling component
    └── UnifiedProvenanceCanvas/
        ├── SlideEditor.tsx                  ✅ NEW: Slide editing UI
        ├── CardManager.tsx                  ✅ NEW: Card management UI
        ├── SlideReorderer.tsx               ✅ NEW: Slide reordering UI
        ├── CardDetailView.tsx               ✅ NEW: Card detail view
        ├── ProvenanceSearchFilter.tsx       ✅ NEW: Search/filter UI
        └── ExportDialog.tsx                 ✅ NEW: Export dialog
```

## Performance Optimizations Summary

### Provenance Chain Building
- ✅ **Pagination**: `loadEvolutionFilesPaginated()` and `getEvolutionFileCount()`
- ✅ **Caching**: `ProvenanceChainCache` class with LRU eviction (max 50 chains)
- ✅ **Pattern Extraction**: Map-based grouping, Set for duplicates, batched queries
- ✅ **Lazy Loading**: `loadProvenanceHistory()` for on-demand loading

### Slide/Card Generation
- ✅ **Memoization**: `generateSlideContentMemoized()` with cache key generation
- ✅ **Card Aggregation**: Single-pass Map-based aggregation with early exit
- ✅ **Virtual Scrolling**: `VirtualizedCardList` component for >50 cards
- ✅ **Debouncing**: `useDebounce` hook with 300ms delay

### Worker Rendering
- ✅ **Instancing**: `renderInstancedNodes()` using THREE.InstancedMesh for >100 nodes
- ✅ **Edge Optimization**: `renderOptimizedEdges()` with shared geometry, batched updates, distance culling
- ✅ **LOD**: 3 detail levels (high/medium/low) based on camera distance
- ✅ **Frustum Culling**: Distance checks and visibility updates

### Performance Monitoring
- ✅ **FPS Tracking**: Current and average over 60 frames
- ✅ **Memory Usage**: Total and breakdown by component
- ✅ **Worker Message Latency**: Per-message-type tracking
- ✅ **Performance Warnings**: Low FPS, high memory, high latency, large scenes

## Dependencies

### External
- **three**: Three.js for 3D rendering
- **@react-three/fiber**: React Three.js integration
- **@react-three/drei**: Three.js helpers

### Internal
- **docs/29-Bipartite-BQF-Federated-Offscreen-Workers/**: Related implementation documentation
- **docs/13-Federated-Provenance-Meta-Log/**: Federated provenance specification
- **docs/04-CanvasL/**: CanvasL specification

## Success Criteria

All features from the plan are implemented:

1. ✅ Provenance chain building optimizations (pagination, caching, pattern extraction, lazy loading)
2. ✅ Slide/card generation optimizations (memoization, card aggregation, virtual scrolling, debouncing)
3. ✅ Worker rendering optimizations (instancing, edge optimization, LOD, frustum culling)
4. ✅ Performance monitoring (FPS tracking, memory usage, worker message latency, performance warnings)
5. ✅ Interactive slide editing (edit content, add/remove cards, reorder slides, save to evolution)
6. ✅ Card detail views (expandable details, JSONL viewer, provenance timeline, pattern visualization)
7. ✅ Provenance chain search and filtering (multi-criteria search, advanced queries, filter presets)
8. ✅ Export functionality (JSON, JSONL, GraphML, DOT, PNG, SVG formats)

## Related Documentation

- **`docs/29-Bipartite-BQF-Federated-Offscreen-Workers/`**: Federated Provenance Canvas Integration Documentation
- **`docs/13-Federated-Provenance-Meta-Log/`**: Federated Provenance Meta-Log specification
- **`docs/04-CanvasL/CANVASL-RFC2119-SPEC.md`**: Base CanvasL specification
- **`docs/28-Canvasl-Frontmatter-Knowledge-Model/`**: Bipartite-BQF specification
- **`docs/18-Metaverse-Portal-Interface/WEBGL_GLTF_SVG_AVATARS_ANALYSIS.md`**: WebGL GLTF SVG Avatars Analysis
- **`docs/01-R5RS-Expressions/WebGL Computational Manifold Architecture.md`**: WebGL Computational Manifold Architecture

## Support

For questions or issues, please refer to:
- Renderer specification: [01-PROVENANCE-CANVAS-RENDERER-RFC2119-SPEC.md](./01-PROVENANCE-CANVAS-RENDERER-RFC2119-SPEC.md)
- Protocol specification: [02-PROTOCOL-SPECIFICATION-RFC2119.md](./02-PROTOCOL-SPECIFICATION-RFC2119.md)
- Meta-specification: [00-META-SPECIFICATION-RFC2119.md](./00-META-SPECIFICATION-RFC2119.md)
- Rendering evolution: [03-RENDERING-EVOLUTION.md](./03-RENDERING-EVOLUTION.md)

---

**End of Documentation**

