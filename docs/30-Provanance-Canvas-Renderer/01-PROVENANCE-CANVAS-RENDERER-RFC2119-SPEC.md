---
id: provenance-canvas-renderer-rfc2119-spec
title: "Provenance Canvas Renderer Specification (RFC 2119)"
level: foundational
type: specification
tags: [provenance-canvas-renderer, rfc2119, specification, performance-optimization, rendering, worker, webgl, gltf, svg, avatars, computational-manifold, a-frame]
keywords: [provenance-canvas-renderer, performance-optimization, pagination, caching, memoization, virtual-scrolling, worker-rendering, lod, frustum-culling, performance-monitoring, webgl, gltf-avatars, svg-textures, computational-manifold, a-frame, networked-aframe]
prerequisites: [provenance-canvas-renderer-meta-specification-rfc2119, federated-provenance-canvas-integration-docs, canvasl-rfc2119-spec, webgl-glft-svg-avatars-analysis]
enables: [provenance-canvas-renderer-protocol-specification-rfc2119, provenance-canvas-renderer-implementation, rendering-evolution-documentation]
related: [federated-provenance-canvas-integration-docs, canvasl-rfc2119-spec, federated-provenance-meta-log-spec, webgl-glft-svg-avatars-analysis, webgl-computational-manifold-architecture]
readingTime: 90
difficulty: 5
version: "1.0.0"
gitTag: "v1.0.0"
immutableTag: "v1.0.0-immutable"
versionDirectory: "versions/v1.0.0/"
blackboard:
  status: active
  assignedAgent: "6D-Intelligence-Agent"
  lastUpdate: "2025-01-07"
  dependencies: [provenance-canvas-renderer-meta-specification-rfc2119, federated-provenance-canvas-integration-docs, canvasl-rfc2119-spec]
  watchers: ["4D-Network-Agent", "Visualization-Agent"]
  r5rsEngine: "r5rs-canvas-engine.scm"
  versionConjoining:
    package: "@automaton/provenance-canvas-renderer-spec@1.0.0"
    metaSpec: "00-META-SPECIFICATION-RFC2119.md@1.0.0"
    protocolSpec: "02-PROTOCOL-SPECIFICATION-RFC2119.md@1.0.0"
    immutableSnapshot: "versions/v1.0.0/"
---

# Provenance Canvas Renderer Specification (RFC 2119)

**Status**: Draft  
**Version**: 1.0.0  
**Date**: 2025-01-07  
**Authors**: Automaton System

## Abstract

This specification defines the Provenance Canvas Renderer system for high-performance rendering of federated provenance chains. The renderer provides comprehensive performance optimizations including pagination, caching, memoization, virtual scrolling, worker rendering optimizations (instancing, level-of-detail, frustum culling), and performance monitoring. The system enables efficient visualization of large-scale evolution directories with real-time performance metrics.

## Table of Contents

1. [Introduction](#1-introduction)
2. [Architecture Overview](#2-architecture-overview)
3. [Performance Optimizations](#3-performance-optimizations)
4. [Performance Monitoring](#4-performance-monitoring)
5. [API Requirements](#5-api-requirements)
6. [Implementation Requirements](#6-implementation-requirements)
7. [Validation Requirements](#7-validation-requirements)
8. [References](#8-references)

---

## 1. Introduction

### 1.1 Purpose

This specification defines:

- Provenance Canvas Renderer architecture and components
- Performance optimization requirements
- Performance monitoring requirements
- API interfaces and contracts
- Implementation requirements
- Validation requirements

### 1.2 Scope

This specification covers:

- Provenance chain building optimizations
- Slide/card generation optimizations
- Worker rendering optimizations
- Performance monitoring system
- API requirements
- Implementation requirements

### 1.3 RFC 2119 Keywords

The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT", "SHOULD", "SHOULD NOT", "RECOMMENDED", "MAY", and "OPTIONAL" in this document are to be interpreted as described in RFC 2119.

### 1.4 Related Documentation (v1.0.0)

- **`00-META-SPECIFICATION-RFC2119.md@1.0.0`** (`versions/v1.0.0/00-META-SPECIFICATION-RFC2119.md`): Meta-specification coordinating all specs
- **`02-PROTOCOL-SPECIFICATION-RFC2119.md@1.0.0`** (`versions/v1.0.0/02-PROTOCOL-SPECIFICATION-RFC2119.md`): Protocol specification

**Package**: `@automaton/provenance-canvas-renderer-spec@1.0.0` | **Git Tags**: `v1.0.0`, `v1.0.0-immutable`

- **`docs/29-Bipartite-BQF-Federated-Offscreen-Workers/`**: Related implementation documentation
- **`docs/13-Federated-Provenance-Meta-Log/`**: Federated provenance specification
- **`docs/18-Metaverse-Portal-Interface/WEBGL_GLTF_SVG_AVATARS_ANALYSIS.md`**: WebGL GLTF SVG Avatars Analysis
- **`docs/01-R5RS-Expressions/WebGL Computational Manifold Architecture.md`**: WebGL Computational Manifold Architecture

---

## 2. Architecture Overview

### 2.1 Component Architecture

The Provenance Canvas Renderer consists of the following components:

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

### 2.2 Service Layer Structure

#### 2.2.1 Provenance Slide Service

**File**: `ui/src/services/provenance-slide-service.ts`

**Responsibilities**:
- Build provenance chains from evolution directories
- Generate slides (one per recursion level 0D→7D→0D)
- Generate cards (grouped by pattern)
- Extract self-execution patterns
- Track federated provenance

**Performance Requirements**:
- MUST support pagination for large directories
- MUST implement caching with LRU eviction
- MUST optimize pattern extraction
- SHOULD support lazy loading

#### 2.2.2 Provenance Canvas Worker Service

**File**: `ui/src/services/provenance-canvas-worker-service.ts`

**Responsibilities**:
- Manage offscreen canvas worker lifecycle
- Handle worker communication
- Load provenance chains into worker
- Handle camera and interaction events

**Performance Requirements**:
- MUST support instancing for >100 nodes
- MUST optimize edge rendering
- MUST implement level-of-detail (LOD)
- MUST implement frustum culling

#### 2.2.3 Performance Monitoring Service

**File**: `ui/src/services/performance-monitoring-service.ts`

**Responsibilities**:
- Track FPS (current and average)
- Track memory usage (with breakdown)
- Track worker message latency
- Provide performance warnings

**Performance Requirements**:
- MUST track FPS
- MUST track memory usage
- MUST track worker message latency
- MUST provide performance warnings

### 2.3 Worker Architecture

**File**: `ui/src/workers/provenance-canvas-worker.ts`

**Responsibilities**:
- Render provenance chains in 3D using Three.js
- Handle interactions via raycasting
- Manage scene, camera, and renderer
- Implement performance optimizations

**Performance Requirements**:
- MUST use instancing for large node counts (>100)
- MUST optimize edge rendering with shared geometry
- MUST implement LOD system with 3 detail levels
- MUST implement frustum culling

### 2.4 3D Rendering Architecture

The Provenance Canvas Renderer extends beyond basic Three.js rendering to support advanced 3D visualization including GLTF avatars, SVG dynamic textures, WebGL Computational Manifold, and A-Frame integration.

#### 2.4.1 GLTF Avatar Support (MUST)

Implementations MUST support GLTF avatar rendering for human and AI agent visualization in the multiverse.

**Requirements**:
- MUST support GLTF/GLB model loading via GLTFLoader from Three.js
- MUST support human avatars (e.g., `DamagedHelmet.glb` from Khronos glTF Sample Models)
- MUST support AI agent avatars (e.g., `Fox.glb` scaled to 0.003 for smaller size)
- MUST support avatar templates for multiplayer synchronization
- MUST support dynamic avatar loading from CDN or user uploads
- MUST support text labels above avatars for identification
- MUST support distinct visual identification for AI agents (e.g., green color #00ff88)

**Implementation**:
```typescript
interface AvatarConfig {
  gltfModel: string;
  scale: [number, number, number];
  position: [number, number, number];
  label?: string;
  color?: string;
  type: 'human' | 'ai-agent';
}

async function loadGLTFAvatar(config: AvatarConfig): Promise<THREE.Group> {
  const loader = new GLTFLoader();
  const gltf = await loader.loadAsync(config.gltfModel);
  const model = gltf.scene;
  
  // Apply scale
  model.scale.set(...config.scale);
  
  // Apply position
  model.position.set(...config.position);
  
  // Apply color for AI agents
  if (config.type === 'ai-agent' && config.color) {
    model.traverse((child) => {
      if (child instanceof THREE.Mesh) {
        child.material.color.setHex(parseInt(config.color!.replace('#', '0x')));
      }
    });
  }
  
  return model;
}
```

**A-Frame Integration**:
```html
<a-entity id="player" networked="template:#avatar-template">
  <a-camera wasd-controls="enabled: true" look-controls="enabled: true"></a-camera>
</a-entity>

<template id="avatar-template">
  <a-entity class="avatar">
    <a-gltf-model 
      gltf-model="https://raw.githubusercontent.com/KhronosGroup/glTF-Sample-Models/master/2.0/DamagedHelmet/glTF-Binary/DamagedHelmet.glb"
      scale="0.5 0.5 0.5">
    </a-gltf-model>
    <a-text value="Player" position="0 1.2 0" align="center" color="white"></a-text>
  </a-entity>
</template>
```

#### 2.4.2 SVG Dynamic Textures (SHOULD)

Implementations SHOULD support SVG dynamic textures for procedural UI overlays and infinite world generation.

**Requirements**:
- SHOULD convert SVG to base64 data URLs for WebGL texture loading
- SHOULD apply SVG as textures to WebGL planes
- SHOULD support real-time SVG updates (every frame)
- SHOULD support procedural UI generation
- SHOULD support infinite world generation (no pixelation)
- SHOULD support dynamic topology diagrams in the manifold

**Implementation**:
```typescript
function svgToTexture(svg: SVGElement): THREE.Texture {
  const serializer = new XMLSerializer();
  const svgString = serializer.serializeToString(svg);
  const base64 = btoa(svgString);
  const dataUrl = `data:image/svg+xml;base64,${base64}`;
  
  const loader = new THREE.TextureLoader();
  const texture = loader.load(dataUrl);
  texture.needsUpdate = true;
  return texture;
}

function updateSVGTexture(plane: THREE.Mesh, svg: SVGElement): void {
  const texture = svgToTexture(svg);
  if (plane.material instanceof THREE.MeshBasicMaterial) {
    plane.material.map = texture;
    plane.material.needsUpdate = true;
  }
}

// Real-time SVG updates
function animateSVG(svg: SVGElement, plane: THREE.Mesh, time: number): void {
  const hue = (time % 360);
  svg.querySelector('rect')?.setAttribute('fill', `hsl(${hue}, 70%, 50%)`);
  svg.querySelector('text')!.textContent = `Time: ${Math.floor(time / 1000)}s`;
  updateSVGTexture(plane, svg);
  requestAnimationFrame((t) => animateSVG(svg, plane, t));
}
```

**A-Frame Integration**:
```javascript
const svg = document.getElementById('svg-texture');
const plane = document.querySelector('a-plane');

function updateSVG(time) {
  const hue = (time % 360);
  svg.querySelector('rect').setAttribute('fill', `hsl(${hue}, 70%, 50%)`);
  svg.querySelector('text').textContent = `Time: ${Math.floor(time / 1000)}s`;
  
  // Update texture
  plane.setAttribute('material', 'src', 
    'data:image/svg+xml;base64,' + btoa(new XMLSerializer().serializeToString(svg)));
  requestAnimationFrame(updateSVG);
}
updateSVG(0);
```

#### 2.4.3 WebGL Computational Manifold (SHOULD)

Implementations SHOULD support 3D spatial encoding of polynomial types and evaluation traces as part of the Computational Manifold Architecture.

**Requirements**:
- SHOULD map 8-type polynomial vectors to 3D coordinates (position, rotation, scale, opacity)
- SHOULD render evaluation traces as 3D animations with particle effects
- SHOULD support GLSL shaders for polynomial visualization (polynomial rings, combinator fields)
- SHOULD support 3D perceptron network transitions as Bezier curves
- SHOULD provide interactive 3D evaluation controls
- SHOULD support multi-strategy comparison in 3D viewports

**Type-Space Coordinate Mapping**:
```typescript
interface TypeSpaceCoordinates {
  position: [number, number, number];
  rotation: [number, number, number];
  scale: number;
  opacity: number;
}

function typeSpaceCoordinates(typeVector: number[]): TypeSpaceCoordinates {
  const [b, p, s, n, c, str, v, proc] = typeVector;
  return {
    position: [b / 10.0, p / 10.0, s / 10.0],
    rotation: [n * 0.1, c * 0.1, str * 0.1],
    scale: 1.0 + (v * 0.5),
    opacity: proc / 10.0
  };
}
```

**Evaluation Trace Animation**:
```typescript
interface EvaluationKeyframe {
  time: number;
  duration: number;
  transform: TypeSpaceCoordinates;
  effect: {
    type: 'particle-burst' | 'energy-flow' | 'branch-glow' | 'pulse';
    color: string;
    intensity?: number;
  };
}

function evalTraceToAnimation(trace: EvaluationStep[]): EvaluationKeyframe[] {
  const keyframes: EvaluationKeyframe[] = [];
  let time = 0.0;
  
  for (const step of trace) {
    const duration = getStepDuration(step);
    const transform = evaluationStepToTransform(step);
    const effect = evaluationEffect(step);
    
    keyframes.push({
      time,
      duration,
      transform,
      effect
    });
    
    time += duration;
  }
  
  return keyframes;
}
```

**GLSL Shader Support**:
```glsl
// polynomial-fragment-shader.glsl
uniform vec3 monadCoords;
uniform vec3 functorCoords; 
uniform vec3 perceptronCoords;
uniform float evaluationTime;
uniform vec3 yCombinator;
uniform vec3 zCombinator;

varying vec3 vPosition;
varying vec3 vNormal;

void main() {
  // Polynomial rings as concentric spheres
  float monadRing = sin(length(vPosition - monadCoords) * 10.0 - evaluationTime);
  float functorRing = sin(length(vPosition - functorCoords) * 15.0 - evaluationTime * 1.5);
  float perceptronRing = sin(length(vPosition - perceptronCoords) * 20.0 - evaluationTime * 2.0);
  
  // Combinator field effects
  vec3 yField = normalize(vPosition - yCombinator);
  vec3 zField = normalize(vPosition - zCombinator);
  float combinatorEffect = dot(yField, zField);
  
  // Final color combines polynomial structure
  vec3 color = vec3(
    monadRing * 0.8 + combinatorEffect * 0.2,
    functorRing * 0.6 + combinatorEffect * 0.4, 
    perceptronRing * 0.7 + combinatorEffect * 0.3
  );
  
  gl_FragColor = vec4(color, 1.0);
}
```

#### 2.4.4 2D CanvasL Knowledge Graph Cards (MUST)

Implementations MUST support 2D CanvasL knowledge graph cards that visualize agent thought processes as knowledge graphs.

**Requirements**:
- MUST render knowledge graphs in CanvasL format
- MUST extract thought processes from agent nodes
- MUST visualize thought processes as graph structures (nodes and edges)
- MUST support CanvasL directives and references
- SHOULD support interactive exploration of knowledge graphs
- SHOULD support filtering by dimension, pattern, or agent

**Implementation**:
```typescript
interface KnowledgeGraphCard {
  id: string;
  agentId: string;
  slideId: string;
  nodes: CanvasLNode[];
  edges: CanvasLEdge[];
  metadata: {
    dimension?: string;
    pattern?: string;
    timestamp: number;
    thoughtProcess: string;
  };
}

interface KnowledgeGraphRenderer {
  renderKnowledgeGraph(card: KnowledgeGraphCard): SVGElement;
  extractThoughtProcess(agentNode: ProvenanceNode): CanvasLNode[];
  buildKnowledgeGraph(slide: Slide, agentId: string): KnowledgeGraphCard;
}
```

**CanvasL Knowledge Graph Structure**:
```typescript
// Extract thought process from agent node
function extractThoughtProcess(agentNode: ProvenanceNode): CanvasLNode[] {
  const nodes: CanvasLNode[] = [];
  
  // Extract from metadata
  if (agentNode.metadata.churchEncoding) {
    nodes.push({
      id: `${agentNode.id}-church-encoding`,
      type: 'node',
      text: agentNode.metadata.churchEncoding,
      x: 0,
      y: 0
    });
  }
  
  // Extract from pattern
  if (agentNode.metadata.pattern) {
    nodes.push({
      id: `${agentNode.id}-pattern`,
      type: 'node',
      text: agentNode.metadata.pattern,
      x: 200,
      y: 0
    });
  }
  
  // Extract from dimension
  if (agentNode.metadata.dimension) {
    nodes.push({
      id: `${agentNode.id}-dimension`,
      type: 'node',
      text: agentNode.metadata.dimension,
      x: 400,
      y: 0
    });
  }
  
  return nodes;
}

// Build knowledge graph from slide
function buildKnowledgeGraph(slide: Slide, agentId: string): KnowledgeGraphCard {
  const agentNodes = slide.provenanceChain?.nodes.filter(
    n => n.metadata.agentId === agentId
  ) || [];
  
  const canvaslNodes: CanvasLNode[] = [];
  const canvaslEdges: CanvasLEdge[] = [];
  
  // Convert provenance nodes to CanvasL nodes
  agentNodes.forEach((node, index) => {
    canvaslNodes.push({
      id: node.id,
      type: 'node',
      text: node.metadata.agentId || node.id,
      x: (index % 3) * 200,
      y: Math.floor(index / 3) * 150
    });
    
    // Create edges for dimensional progression
    if (index > 0) {
      canvaslEdges.push({
        id: `edge-${agentNodes[index - 1].id}-${node.id}`,
        type: 'vertical',
        from: agentNodes[index - 1].id,
        to: node.id
      });
    }
  });
  
  return {
    id: `kg-${agentId}-${slide.id}`,
    agentId,
    slideId: slide.id,
    nodes: canvaslNodes,
    edges: canvaslEdges,
    metadata: {
      dimension: slide.dimension,
      timestamp: Date.now(),
      thoughtProcess: `Thought process for ${agentId} in ${slide.dimension}`
    }
  };
}
```

**2D CanvasL Card Rendering**:
```typescript
// Render knowledge graph as SVG (2D CanvasL card)
function renderKnowledgeGraph(card: KnowledgeGraphCard): SVGElement {
  const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
  svg.setAttribute('width', '800');
  svg.setAttribute('height', '600');
  svg.setAttribute('viewBox', '0 0 800 600');
  
  // Render edges first
  card.edges.forEach(edge => {
    const fromNode = card.nodes.find(n => n.id === edge.from);
    const toNode = card.nodes.find(n => n.id === edge.to);
    
    if (fromNode && toNode) {
      const line = document.createElementNS('http://www.w3.org/2000/svg', 'line');
      line.setAttribute('x1', String(fromNode.x || 0));
      line.setAttribute('y1', String(fromNode.y || 0));
      line.setAttribute('x2', String(toNode.x || 0));
      line.setAttribute('y2', String(toNode.y || 0));
      line.setAttribute('stroke', '#4b5563');
      line.setAttribute('stroke-width', '2');
      svg.appendChild(line);
    }
  });
  
  // Render nodes
  card.nodes.forEach(node => {
    const rect = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
    rect.setAttribute('x', String((node.x || 0) - 50));
    rect.setAttribute('y', String((node.y || 0) - 25));
    rect.setAttribute('width', '100');
    rect.setAttribute('height', '50');
    rect.setAttribute('fill', '#3b82f6');
    rect.setAttribute('rx', '5');
    svg.appendChild(rect);
    
    const text = document.createElementNS('http://www.w3.org/2000/svg', 'text');
    text.setAttribute('x', String(node.x || 0));
    text.setAttribute('y', String(node.y || 0));
    text.setAttribute('text-anchor', 'middle');
    text.setAttribute('fill', 'white');
    text.setAttribute('font-size', '12');
    text.textContent = node.text || node.id;
    svg.appendChild(text);
  });
  
  return svg;
}
```

### 2.4.5 A-Frame Integration (SHOULD)

Implementations SHOULD support A-Frame for VR/AR metaverse visualization and multiplayer synchronization.

**Requirements**:
- SHOULD support A-Frame scene setup with declarative HTML-like syntax
- SHOULD support Networked-A-Frame for multiplayer avatar synchronization
- SHOULD support WebRTC voice chat integration
- SHOULD support WebXR for VR/AR headsets
- SHOULD support avatar templates for consistent multiplayer appearance
- SHOULD support real-time position and rotation synchronization

**A-Frame Scene Setup**:
```html
<a-scene 
  vr-mode-ui="enabled: true"
  embedded
  networked-scene
  socketio="url: https://naf-server.glitch.me">
  
  <!-- Assets -->
  <a-assets>
    <a-asset-item id="helmet" src="DamagedHelmet.glb"></a-asset-item>
    <a-asset-item id="fox" src="Fox.glb"></a-asset-item>
  </a-assets>
  
  <!-- Human Avatar Template -->
  <template id="avatar-template">
    <a-entity class="avatar">
      <a-gltf-model 
        gltf-model="#helmet"
        scale="0.5 0.5 0.5"
        animation="property: rotation; to: 0 360 0; loop: true; dur: 10000">
      </a-gltf-model>
      <a-text 
        value="Player" 
        position="0 1.2 0" 
        align="center" 
        color="white" 
        width="3">
      </a-text>
    </a-entity>
  </template>
  
  <!-- AI Agent Template -->
  <template id="ai-template">
    <a-entity class="ai-agent">
      <a-gltf-model 
        gltf-model="#fox"
        scale="0.003 0.003 0.003">
      </a-gltf-model>
      <a-text 
        value="AI Agent" 
        position="0 0.5 0" 
        align="center" 
        color="#00ff88">
      </a-text>
    </a-entity>
  </template>
  
  <!-- Player Entity -->
  <a-entity 
    id="player" 
    networked="template:#avatar-template;attachTemplateToLocal:false">
    <a-camera 
      wasd-controls="enabled: true" 
      look-controls="enabled: true">
    </a-camera>
  </a-entity>
  
  <!-- Lights -->
  <a-light type="ambient" color="#404040"></a-light>
  <a-light type="directional" position="5 10 5" intensity="0.8"></a-light>
</a-scene>
```

**Networked-A-Frame Configuration**:
```javascript
// Networked-A-Frame setup
NAF.schemas.add({
  template: '#avatar-template',
  components: [
    'position',
    'rotation',
    'scale',
    'visible',
    'animation'
  ]
});
```

---

## 3. Performance Optimizations

### 3.1 Provenance Chain Building Optimizations

#### 3.1.1 Pagination (MUST)

Implementations MUST support pagination for large evolution directories.

**Requirements**:
- MUST provide `loadEvolutionFilesPaginated()` method
- MUST provide `getEvolutionFileCount()` method
- MUST support configurable page size
- SHOULD support cursor-based pagination

**Implementation**:
```typescript
interface PaginationOptions {
  page: number;
  pageSize: number;
  cursor?: string;
}

async loadEvolutionFilesPaginated(
  evolutionPath: string,
  options: PaginationOptions
): Promise<{ files: any[]; total: number; hasMore: boolean }>
```

#### 3.1.2 Caching (MUST)

Implementations MUST implement caching with LRU eviction.

**Requirements**:
- MUST use LRU eviction policy
- MUST support maximum cache size (default: 50 chains)
- MUST invalidate cache on file changes
- SHOULD support cache warming

**Implementation**:
```typescript
class ProvenanceChainCache {
  private maxSize: number = 50;
  private cache: Map<string, ProvenanceChain>;
  
  get(key: string): ProvenanceChain | null;
  set(key: string, chain: ProvenanceChain): void;
  clear(): void;
}
```

#### 3.1.3 Pattern Extraction Optimization (MUST)

Implementations MUST optimize pattern extraction.

**Requirements**:
- MUST use Map-based grouping for patterns
- MUST use Set for duplicate detection
- MUST batch federated provenance queries
- SHOULD use parallel processing where possible

#### 3.1.4 Lazy Loading (SHOULD)

Implementations SHOULD support lazy loading for provenance history.

**Requirements**:
- SHOULD provide `loadProvenanceHistory()` method
- SHOULD load history on-demand
- SHOULD cache loaded history
- MAY support progressive loading

### 3.2 Slide/Card Generation Optimizations

#### 3.2.1 Memoization (MUST)

Implementations MUST implement memoization for slide content generation.

**Requirements**:
- MUST cache slide content based on input parameters
- MUST generate cache keys from dimension and node data
- MUST invalidate cache when nodes change
- SHOULD use weak references for large caches

**Implementation**:
```typescript
function generateSlideContentMemoized(
  dimension: string,
  nodes: ProvenanceNode[]
): string {
  const cacheKey = generateCacheKey(dimension, nodes);
  if (cache.has(cacheKey)) {
    return cache.get(cacheKey);
  }
  const content = generateSlideContent(dimension, nodes);
  cache.set(cacheKey, content);
  return content;
}
```

#### 3.2.2 Card Aggregation Optimization (MUST)

Implementations MUST optimize card aggregation.

**Requirements**:
- MUST use single-pass Map-based aggregation
- MUST use early exit for empty groups
- SHOULD use parallel processing for large datasets
- SHOULD minimize memory allocations

#### 3.2.3 Virtual Scrolling (SHOULD)

Implementations SHOULD support virtual scrolling for large card lists (>50 cards).

**Requirements**:
- SHOULD render only visible cards
- SHOULD support configurable item height
- SHOULD support smooth scrolling
- MAY support infinite scrolling

**Implementation**:
```typescript
interface VirtualizedCardListProps {
  cards: Card[];
  itemHeight: number;
  containerHeight: number;
  overscan?: number;
}
```

#### 3.2.4 Debouncing (SHOULD)

Implementations SHOULD implement debouncing for dimension filter changes.

**Requirements**:
- SHOULD use 300ms delay for filter changes
- SHOULD cancel pending updates on new changes
- SHOULD provide configurable delay
- MAY support different delays for different operations

**Implementation**:
```typescript
function useDebounce<T>(value: T, delay: number = 300): T {
  const [debouncedValue, setDebouncedValue] = useState(value);
  
  useEffect(() => {
    const handler = setTimeout(() => {
      setDebouncedValue(value);
    }, delay);
    
    return () => {
      clearTimeout(handler);
    };
  }, [value, delay]);
  
  return debouncedValue;
}
```

### 3.3 Worker Rendering Optimizations

#### 3.3.1 Instancing (MUST)

Implementations MUST support instancing for node counts >100.

**Requirements**:
- MUST use THREE.InstancedMesh for nodes >100
- MUST update instance matrices efficiently
- MUST support per-instance attributes (color, size)
- SHOULD batch instance updates

**Implementation**:
```typescript
function renderInstancedNodes(
  nodes: ProvenanceNode[],
  scene: THREE.Scene
): void {
  if (nodes.length > 100) {
    const instancedMesh = new THREE.InstancedMesh(
      geometry,
      material,
      nodes.length
    );
    
    nodes.forEach((node, index) => {
      const matrix = new THREE.Matrix4();
      matrix.setPosition(node.position);
      instancedMesh.setMatrixAt(index, matrix);
    });
    
    scene.add(instancedMesh);
  }
}
```

#### 3.3.2 Edge Optimization (MUST)

Implementations MUST optimize edge rendering.

**Requirements**:
- MUST use shared geometry for edges
- MUST batch edge updates
- MUST implement distance culling
- SHOULD use line segments for efficiency

**Implementation**:
```typescript
function renderOptimizedEdges(
  edges: ProvenanceEdge[],
  scene: THREE.Scene
): void {
  const sharedGeometry = new THREE.BufferGeometry();
  const positions = new Float32Array(edges.length * 6);
  
  edges.forEach((edge, index) => {
    // Batch position updates
    const offset = index * 6;
    positions[offset] = edge.from.position[0];
    // ... set all positions
  });
  
  sharedGeometry.setAttribute('position', new THREE.BufferAttribute(positions, 3));
  const line = new THREE.LineSegments(sharedGeometry, material);
  scene.add(line);
}
```

#### 3.3.3 Level-of-Detail (LOD) (MUST)

Implementations MUST implement level-of-detail system.

**Requirements**:
- MUST support 3 detail levels (high/medium/low)
- MUST switch based on camera distance
- MUST use different geometries for each level
- SHOULD support smooth transitions

**Implementation**:
```typescript
interface LODLevel {
  distance: number;
  geometry: THREE.BufferGeometry;
  material: THREE.Material;
}

function updateLOD(
  node: ProvenanceNode,
  camera: THREE.Camera,
  lodLevels: LODLevel[]
): void {
  const distance = camera.position.distanceTo(node.position);
  
  if (distance < lodLevels[0].distance) {
    node.mesh.geometry = lodLevels[0].geometry;
    node.mesh.material = lodLevels[0].material;
  } else if (distance < lodLevels[1].distance) {
    node.mesh.geometry = lodLevels[1].geometry;
    node.mesh.material = lodLevels[1].material;
  } else {
    node.mesh.geometry = lodLevels[2].geometry;
    node.mesh.material = lodLevels[2].material;
  }
}
```

#### 3.3.4 Frustum Culling (MUST)

Implementations MUST implement frustum culling.

**Requirements**:
- MUST check nodes against camera frustum
- MUST skip rendering for culled nodes
- MUST update visibility efficiently
- SHOULD use spatial indexing for large scenes

**Implementation**:
```typescript
function frustumCull(
  nodes: ProvenanceNode[],
  camera: THREE.Camera
): ProvenanceNode[] {
  const frustum = new THREE.Frustum();
  frustum.setFromProjectionMatrix(
    camera.projectionMatrix.clone().multiply(camera.matrixWorldInverse)
  );
  
  return nodes.filter(node => {
    const sphere = new THREE.Sphere(node.position, node.radius);
    return frustum.intersectsSphere(sphere);
  });
}
```

---

## 4. Performance Monitoring

### 4.1 FPS Tracking (MUST)

Implementations MUST track FPS.

**Requirements**:
- MUST track current FPS
- MUST track average FPS over 60 frames
- MUST update FPS every frame
- SHOULD provide FPS history

**Implementation**:
```typescript
class PerformanceMonitoringService {
  private fpsHistory: number[] = [];
  private frameCount: number = 0;
  private lastTime: number = performance.now();
  
  updateFPS(): void {
    const now = performance.now();
    const delta = now - this.lastTime;
    const fps = 1000 / delta;
    
    this.fpsHistory.push(fps);
    if (this.fpsHistory.length > 60) {
      this.fpsHistory.shift();
    }
    
    this.lastTime = now;
    this.frameCount++;
  }
  
  getCurrentFPS(): number {
    return this.fpsHistory[this.fpsHistory.length - 1] || 0;
  }
  
  getAverageFPS(): number {
    return this.fpsHistory.reduce((a, b) => a + b, 0) / this.fpsHistory.length;
  }
}
```

### 4.2 Memory Usage Tracking (MUST)

Implementations MUST track memory usage.

**Requirements**:
- MUST track total memory usage
- MUST track memory breakdown by component
- MUST update memory metrics periodically
- SHOULD provide memory warnings

**Implementation**:
```typescript
interface MemoryMetrics {
  total: number;
  breakdown: {
    nodes: number;
    edges: number;
    textures: number;
    geometries: number;
    materials: number;
  };
}

getMemoryUsage(): MemoryMetrics {
  const memory = (performance as any).memory;
  return {
    total: memory.usedJSHeapSize,
    breakdown: {
      nodes: this.calculateNodeMemory(),
      edges: this.calculateEdgeMemory(),
      textures: this.calculateTextureMemory(),
      geometries: this.calculateGeometryMemory(),
      materials: this.calculateMaterialMemory()
    }
  };
}
```

### 4.3 Worker Message Latency Tracking (MUST)

Implementations MUST track worker message latency.

**Requirements**:
- MUST track latency per message type
- MUST track average latency
- MUST track maximum latency
- SHOULD provide latency warnings

**Implementation**:
```typescript
interface MessageLatency {
  type: string;
  latency: number;
  timestamp: number;
}

trackMessageLatency(type: string, startTime: number): void {
  const latency = performance.now() - startTime;
  this.latencyHistory.push({ type, latency, timestamp: Date.now() });
  
  if (latency > this.maxLatency) {
    this.emitWarning('high-latency', { type, latency });
  }
}
```

### 4.4 Performance Warnings (MUST)

Implementations MUST provide performance warnings.

**Requirements**:
- MUST warn on low FPS (<30 FPS)
- MUST warn on high memory usage (>500MB)
- MUST warn on high latency (>100ms)
- MUST warn on large node/edge counts (>1000 nodes, >5000 edges)

**Implementation**:
```typescript
interface PerformanceWarning {
  type: 'low-fps' | 'high-memory' | 'high-latency' | 'large-scene';
  severity: 'warning' | 'error';
  message: string;
  metrics: any;
}

checkPerformanceWarnings(): PerformanceWarning[] {
  const warnings: PerformanceWarning[] = [];
  
  if (this.getCurrentFPS() < 30) {
    warnings.push({
      type: 'low-fps',
      severity: 'warning',
      message: `Low FPS: ${this.getCurrentFPS().toFixed(1)}`,
      metrics: { fps: this.getCurrentFPS() }
    });
  }
  
  const memory = this.getMemoryUsage();
  if (memory.total > 500 * 1024 * 1024) {
    warnings.push({
      type: 'high-memory',
      severity: 'warning',
      message: `High memory usage: ${(memory.total / 1024 / 1024).toFixed(1)}MB`,
      metrics: memory
    });
  }
  
  return warnings;
}
```

---

## 5. API Requirements

### 5.1 Service Interfaces

#### 5.1.1 Provenance Slide Service

**File**: `ui/src/services/provenance-slide-service.ts`

**Required Methods**:
- `buildProvenanceChain(evolutionPath: string): Promise<ProvenanceChain>`
- `buildProvenanceChainPaginated(evolutionPath: string, options: PaginationOptions): Promise<PaginatedResult<ProvenanceChain>>`
- `generateSlidesFromEvolution(evolutionPath: string): Promise<Slide[]>`
- `generateCardsForDimension(dimension: string, nodes: ProvenanceNode[]): Promise<Card[]>`
- `loadProvenanceHistory(nodeId: string): Promise<ProvenanceHistory[]>`

#### 5.1.2 Provenance Canvas Worker Service

**File**: `ui/src/services/provenance-canvas-worker-service.ts`

**Required Methods**:
- `init(canvas: OffscreenCanvas, options: CanvasOptions): Promise<void>`
- `loadProvenanceChain(chain: ProvenanceChain): Promise<void>`
- `updateCamera(position: [number, number, number], target: [number, number, number]): Promise<void>`
- `handleClick(x: number, y: number, width: number, height: number): Promise<ProvenanceNode | null>`
- `dispose(): Promise<void>`

#### 5.1.3 Performance Monitoring Service

**File**: `ui/src/services/performance-monitoring-service.ts`

**Required Methods**:
- `startMonitoring(): void`
- `stopMonitoring(): void`
- `getCurrentFPS(): number`
- `getAverageFPS(): number`
- `getMemoryUsage(): MemoryMetrics`
- `getMessageLatency(type: string): number`
- `getPerformanceWarnings(): PerformanceWarning[]`

### 5.2 Cache Management

#### 5.2.1 Provenance Chain Cache

**File**: `ui/src/services/provenance-chain-cache.ts`

**Required Methods**:
- `get(key: string): ProvenanceChain | null`
- `set(key: string, chain: ProvenanceChain): void`
- `clear(): void`
- `invalidate(key: string): void`

### 5.3 Memoization Utilities

**File**: `ui/src/utils/memoization.ts`

**Required Functions**:
- `memoize<T>(fn: Function, keyGenerator?: Function): Function`
- `generateCacheKey(...args: any[]): string`
- `clearMemoizationCache(): void`

---

## 6. Implementation Requirements

### 6.1 File Structure Requirements

Implementations MUST follow this file structure:

```
ui/src/
├── services/
│   ├── provenance-slide-service.ts
│   ├── provenance-canvas-worker-service.ts
│   ├── performance-monitoring-service.ts
│   └── provenance-chain-cache.ts
├── hooks/
│   └── useDebounce.ts
├── utils/
│   └── memoization.ts
└── components/
    └── shared/
        └── VirtualizedCardList.tsx
```

### 6.2 Service Implementation Requirements

#### 6.2.1 Provenance Slide Service

- MUST implement pagination support
- MUST implement caching with LRU eviction
- MUST optimize pattern extraction
- SHOULD implement lazy loading

#### 6.2.2 Provenance Canvas Worker Service

- MUST integrate performance monitoring
- MUST support instancing for large node counts
- MUST optimize edge rendering
- MUST implement LOD and frustum culling

### 6.3 Worker Implementation Requirements

- MUST use THREE.InstancedMesh for nodes >100
- MUST optimize edge rendering with shared geometry
- MUST implement LOD system with 3 detail levels
- MUST implement frustum culling

### 6.4 Component Implementation Requirements

- SHOULD use virtual scrolling for large card lists
- SHOULD use debouncing for filter changes
- MUST integrate performance monitoring
- SHOULD display performance warnings

---

## 7. Validation Requirements

### 7.1 Performance Validation

Implementations MUST validate:

- FPS remains above 30 FPS for typical workloads
- Memory usage remains below 500MB for typical workloads
- Worker message latency remains below 100ms
- Large scenes (>1000 nodes) render efficiently

### 7.2 Functional Validation

Implementations MUST validate:

- Provenance chains build correctly with pagination
- Slides generate correctly with memoization
- Cards aggregate correctly with optimization
- Worker rendering works correctly with all optimizations

### 7.3 Compliance Validation

Implementations MUST:

- Support all MUST requirements
- Support SHOULD requirements (or document why not)
- Report compliance status
- Provide performance metrics

---

## 8. References

### 8.1 Package Specifications (v1.0.0)

- **`00-META-SPECIFICATION-RFC2119.md@1.0.0`** (`versions/v1.0.0/00-META-SPECIFICATION-RFC2119.md`): Meta-specification coordinating all specs
- **`02-PROTOCOL-SPECIFICATION-RFC2119.md@1.0.0`** (`versions/v1.0.0/02-PROTOCOL-SPECIFICATION-RFC2119.md`): Protocol specification

**Git Tags**: `v1.0.0`, `v1.0.0-immutable`  
**Package**: `@automaton/provenance-canvas-renderer-spec@1.0.0`

### 8.2 Base Specifications

- **`docs/29-Bipartite-BQF-Federated-Offscreen-Workers/README.md`**: Federated Provenance Canvas Integration Documentation
- **`docs/04-CanvasL/CANVASL-RFC2119-SPEC.md`**: Base CanvasL specification
- **`docs/13-Federated-Provenance-Meta-Log/FEDERATED-PROVENANCE-RFC2119-SPEC.md`**: Federated provenance specification

### 8.3 Standards

- **RFC 2119**: Key words for use in RFCs to Indicate Requirement Levels
- **Three.js Documentation**: https://threejs.org/docs/
- **Web Workers API**: https://developer.mozilla.org/en-US/docs/Web/API/Web_Workers_API

---

**End of Specification**

