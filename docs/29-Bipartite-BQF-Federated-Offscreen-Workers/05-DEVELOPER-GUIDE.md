---
id: developer-guide-federated-provenance-canvas
title: "Developer Guide: Extending Federated Provenance Canvas Services"
level: intermediate
type: guide
tags: [developer-guide, extension-guide, service-architecture, custom-services, integration-patterns]
keywords: [developer-guide, extension, custom-services, service-architecture, integration-patterns, best-practices]
prerequisites: [implementation-details-federated-provenance-canvas, api-reference-federated-provenance-canvas]
enables: [troubleshooting-guide-federated-provenance-canvas, workflow-walkthroughs-federated-provenance-canvas]
related: [federated-provenance-canvas-integration-docs, implementation-details-federated-provenance-canvas, api-reference-federated-provenance-canvas]
readingTime: 45
difficulty: 3
version: "1.0.0"
blackboard:
  status: active
  assignedAgent: "6D-Intelligence-Agent"
  lastUpdate: "2025-01-07"
  dependencies: [implementation-details-federated-provenance-canvas, api-reference-federated-provenance-canvas]
  watchers: ["4D-Network-Agent", "Query-Interface-Agent"]
---

# Developer Guide: Extending Federated Provenance Canvas Services

**Date**: 2025-01-07  
**Version**: 1.0.0

## Overview

This guide provides comprehensive instructions for extending the Federated Provenance Canvas Integration services. It covers service architecture, extension patterns, integration best practices, and examples for creating custom services.

## Table of Contents

1. [Service Architecture](#service-architecture)
2. [Extending ProvenanceSlideService](#extending-provenanceslideservice)
3. [Extending ProvenanceCanvasWorkerService](#extending-provenancecanvasworkerservice)
4. [Creating Custom Avatar Services](#creating-custom-avatar-services)
5. [Creating Custom Thought Card Renderers](#creating-custom-thought-card-renderers)
6. [Creating Custom Knowledge Graph Extractors](#creating-custom-knowledge-graph-extractors)
7. [Integration Patterns](#integration-patterns)
8. [Best Practices](#best-practices)

## Service Architecture

### Service Layer Structure

The Federated Provenance Canvas Integration follows a layered service architecture:

```
┌─────────────────────────────────────────────────────────┐
│              Component Layer (React)                    │
│         UnifiedProvenanceCanvas, etc.                    │
└─────────────────────────────────────────────────────────┘
                         │
┌─────────────────────────────────────────────────────────┐
│              Service Layer (TypeScript)                 │
│  ProvenanceSlideService, ProvenanceCanvasWorkerService  │
│  ThoughtCardService, KnowledgeGraphCardService, etc.    │
└─────────────────────────────────────────────────────────┘
                         │
┌─────────────────────────────────────────────────────────┐
│              Infrastructure Layer                       │
│  DatabaseService, ErrorLoggingService, Cache, etc.      │
└─────────────────────────────────────────────────────────┘
```

### Service Design Principles

1. **Single Responsibility**: Each service has one clear purpose
2. **Dependency Injection**: Services accept dependencies via constructor
3. **Error Handling**: All services use consistent error handling patterns
4. **Caching**: Services implement caching for performance
5. **Retry Logic**: Network operations use retry with backoff
6. **Performance Monitoring**: Services integrate with performance monitoring

## Extending ProvenanceSlideService

### Overview

The `ProvenanceSlideService` is responsible for building provenance chains and generating slides/cards. To extend it, you can:

1. Override methods for custom behavior
2. Add new methods for additional functionality
3. Extend the service class

### Example: Custom Pattern Extraction

```typescript
import { ProvenanceSlideService } from '@/services/provenance-slide-service';
import { ProvenanceNode, ProvenanceChain } from '@/services/provenance-slide-service';

export class CustomProvenanceSlideService extends ProvenanceSlideService {
  /**
   * Custom pattern extraction with additional metadata
   */
  protected async extractSelfExecutionPatternsWithProvenance(
    files: any[]
  ): Promise<any[]> {
    // Call parent method
    const patterns = await super.extractSelfExecutionPatternsWithProvenance(files);
    
    // Add custom metadata
    return patterns.map(pattern => ({
      ...pattern,
      customMetadata: {
        extractedAt: Date.now(),
        source: 'custom-extractor'
      }
    }));
  }
  
  /**
   * Custom slide content generation
   */
  protected generateSlideContent(
    dimension: string,
    nodes: ProvenanceNode[]
  ): string {
    const baseContent = super.generateSlideContent(dimension, nodes);
    
    // Add custom content
    return `${baseContent}\n\n## Custom Section\nCustom content here.`;
  }
}
```

### Example: Custom Card Generation

```typescript
export class CustomProvenanceSlideService extends ProvenanceSlideService {
  /**
   * Custom card generation with additional grouping
   */
  async generateCardsForDimension(
    dimension: string,
    nodes: ProvenanceNode[]
  ): Promise<Card[]> {
    // Get base cards
    const baseCards = await super.generateCardsForDimension(dimension, nodes);
    
    // Add custom grouping by agent
    const agentGroups = new Map<string, Card[]>();
    baseCards.forEach(card => {
      const agentId = card.metadata?.agentId || 'unknown';
      if (!agentGroups.has(agentId)) {
        agentGroups.set(agentId, []);
      }
      agentGroups.get(agentId)!.push(card);
    });
    
    // Create summary cards per agent
    const summaryCards: Card[] = [];
    agentGroups.forEach((cards, agentId) => {
      summaryCards.push({
        id: `summary-${agentId}-${dimension}`,
        type: 'summary',
        pattern: `Agent: ${agentId}`,
        jsonlLines: [],
        metadata: {
          agentId,
          cardCount: cards.length,
          dimension
        }
      });
    });
    
    return [...baseCards, ...summaryCards];
  }
}
```

## Extending ProvenanceCanvasWorkerService

### Overview

The `ProvenanceCanvasWorkerService` manages offscreen canvas rendering in a Web Worker. Extensions can:

1. Add custom message handlers
2. Extend rendering capabilities
3. Add custom worker initialization

### Example: Custom Message Handlers

```typescript
import { ProvenanceCanvasWorkerService } from '@/services/provenance-canvas-worker-service';

export class CustomProvenanceCanvasWorkerService extends ProvenanceCanvasWorkerService {
  /**
   * Initialize with custom message handlers
   */
  async init(canvas: OffscreenCanvas, options: CanvasOptions): Promise<void> {
    await super.init(canvas, options);
    
    // Register custom message handlers
    this.onMessage('customRender', (data) => {
      this.handleCustomRender(data);
    });
  }
  
  /**
   * Custom render handler
   */
  private handleCustomRender(data: any): void {
    // Custom rendering logic
    this.sendMessage({
      type: 'customRenderComplete',
      payload: { success: true }
    });
  }
}
```

## Creating Custom Avatar Services

### Overview

Avatar services manage GLTF model loading and template registration. You can create custom services for:

1. Custom avatar loading strategies
2. Dynamic avatar generation
3. Avatar animation systems

### Example: Custom Avatar Loader

```typescript
import { AvatarLoaderService } from '@/services/avatar-loader-service';
import { AvatarConfig } from '@/services/provenance-slide-service';
import * as THREE from 'three';

export class CustomAvatarLoaderService extends AvatarLoaderService {
  /**
   * Load avatar with custom post-processing
   */
  async loadAvatar(config: AvatarConfig): Promise<THREE.Group> {
    const model = await super.loadAvatar(config);
    
    // Add custom animations
    if (config.type === 'ai-agent') {
      this.addPulsingAnimation(model);
    }
    
    return model;
  }
  
  /**
   * Add pulsing animation to model
   */
  private addPulsingAnimation(model: THREE.Group): void {
    const clock = new THREE.Clock();
    
    const animate = () => {
      const time = clock.getElapsedTime();
      const scale = 1 + Math.sin(time * 2) * 0.1;
      model.scale.set(scale, scale, scale);
      requestAnimationFrame(animate);
    };
    
    animate();
  }
}
```

## Creating Custom Thought Card Renderers

### Overview

Thought card services manage 3D thought card creation and rendering. Custom renderers can:

1. Customize card appearance
2. Add interactive features
3. Implement custom layouts

### Example: Custom Thought Card Service

```typescript
import { ThoughtCardService, ThoughtCard } from '@/services/thought-card-service';

export class CustomThoughtCardService extends ThoughtCardService {
  /**
   * Create thought card with custom styling
   */
  createThoughtCard(
    avatarId: string,
    content: string,
    options: any = {}
  ): ThoughtCard {
    const card = super.createThoughtCard(avatarId, content, options);
    
    // Add custom metadata
    card.metadata = {
      ...card.metadata,
      customStyle: 'animated',
      animationSpeed: 1.0
    };
    
    return card;
  }
  
  /**
   * Create card texture with custom rendering
   */
  createCardTexture(card: ThoughtCard): HTMLCanvasElement {
    const canvas = super.createCardTexture(card);
    
    // Add custom effects
    const ctx = canvas.getContext('2d');
    if (ctx && card.metadata?.customStyle === 'animated') {
      // Add gradient background
      const gradient = ctx.createLinearGradient(0, 0, canvas.width, canvas.height);
      gradient.addColorStop(0, '#1e3a8a');
      gradient.addColorStop(1, '#3b82f6');
      ctx.fillStyle = gradient;
      ctx.fillRect(0, 0, canvas.width, canvas.height);
    }
    
    return canvas;
  }
}
```

## Creating Custom Knowledge Graph Extractors

### Overview

Knowledge graph services extract and render agent thought processes. Custom extractors can:

1. Extract additional metadata
2. Create custom graph layouts
3. Implement custom rendering

### Example: Custom Knowledge Graph Service

```typescript
import { KnowledgeGraphCardService, KnowledgeGraphCard } from '@/services/knowledge-graph-card-service';
import { ProvenanceNode } from '@/services/provenance-slide-service';

export class CustomKnowledgeGraphCardService extends KnowledgeGraphCardService {
  /**
   * Extract thought process with additional relationships
   */
  extractThoughtProcess(agentNode: ProvenanceNode): CanvasLNode[] {
    const baseNodes = super.extractThoughtProcess(agentNode);
    
    // Add relationship nodes
    if (agentNode.data?.relationships) {
      agentNode.data.relationships.forEach((rel: any, index: number) => {
        baseNodes.push({
          id: `${agentNode.id}-relationship-${index}`,
          type: 'relationship',
          text: `Related: ${rel.target}`,
          x: (baseNodes.length + index) * 200,
          y: 0
        });
      });
    }
    
    return baseNodes;
  }
  
  /**
   * Render with custom layout algorithm
   */
  renderKnowledgeGraph(card: KnowledgeGraphCard): SVGElement {
    // Apply force-directed layout
    const layout = this.applyForceDirectedLayout(card);
    
    // Render with layout
    return super.renderKnowledgeGraph({
      ...card,
      nodes: layout.nodes,
      edges: layout.edges
    });
  }
  
  /**
   * Apply force-directed layout
   */
  private applyForceDirectedLayout(card: KnowledgeGraphCard): KnowledgeGraphCard {
    // Force-directed layout implementation
    // ... (simplified for example)
    return card;
  }
}
```

## Integration Patterns

### Pattern 1: Service Composition

Compose multiple services to create complex functionality:

```typescript
import { provenanceSlideService } from '@/services/provenance-slide-service';
import { thoughtCardService } from '@/services/thought-card-service';
import { knowledgeGraphCardService } from '@/services/knowledge-graph-card-service';

export class ProvenanceVisualizationService {
  async createCompleteVisualization(evolutionPath: string) {
    // Build provenance chain
    const chain = await provenanceSlideService.buildProvenanceChain(evolutionPath);
    
    // Generate slides
    const slides = await provenanceSlideService.generateSlidesFromEvolution(evolutionPath);
    
    // Create thought cards for all agent nodes
    const thoughtCards = chain.nodes
      .filter(n => n.avatar)
      .map(node => thoughtCardService.createThoughtCardFromNode(node))
      .filter(Boolean);
    
    // Create knowledge graphs for all slides
    const knowledgeGraphs = slides.flatMap(slide =>
      knowledgeGraphCardService.buildKnowledgeGraphsForSlide(slide)
    );
    
    return {
      chain,
      slides,
      thoughtCards,
      knowledgeGraphs
    };
  }
}
```

### Pattern 2: Event-Driven Architecture

Use events to coordinate services:

```typescript
import { EventEmitter } from 'events';

export class ProvenanceEventService extends EventEmitter {
  constructor() {
    super();
    this.setupEventHandlers();
  }
  
  private setupEventHandlers(): void {
    // Listen for chain updates
    this.on('chain:updated', (chain) => {
      // Update related services
      this.emit('thoughtCards:refresh', chain);
      this.emit('knowledgeGraphs:refresh', chain);
    });
  }
}
```

### Pattern 3: Plugin Architecture

Create a plugin system for extensibility:

```typescript
export interface ProvenancePlugin {
  name: string;
  version: string;
  initialize(service: ProvenanceSlideService): void;
  processNode(node: ProvenanceNode): ProvenanceNode;
  processChain(chain: ProvenanceChain): ProvenanceChain;
}

export class ProvenancePluginManager {
  private plugins: Map<string, ProvenancePlugin> = new Map();
  
  register(plugin: ProvenancePlugin): void {
    this.plugins.set(plugin.name, plugin);
  }
  
  applyPlugins(chain: ProvenanceChain): ProvenanceChain {
    let processedChain = chain;
    
    for (const plugin of this.plugins.values()) {
      processedChain = {
        ...processedChain,
        nodes: processedChain.nodes.map(node => plugin.processNode(node)),
        edges: processedChain.edges
      };
      processedChain = plugin.processChain(processedChain);
    }
    
    return processedChain;
  }
}
```

## Best Practices

### 1. Error Handling

Always use consistent error handling:

```typescript
try {
  const result = await service.method();
  return result;
} catch (error) {
  const errorObj = error instanceof Error ? error : new Error(String(error));
  errorLoggingService.logError(errorObj, {
    service: 'YourService',
    action: 'method',
    metadata: { /* context */ },
    severity: 'error'
  });
  throw errorObj;
}
```

### 2. Caching

Implement caching for expensive operations:

```typescript
private cache: Map<string, CachedResult> = new Map();

async expensiveOperation(key: string): Promise<Result> {
  // Check cache
  const cached = this.cache.get(key);
  if (cached && Date.now() - cached.timestamp < this.cacheTimeout) {
    return cached.result;
  }
  
  // Perform operation
  const result = await this.performOperation(key);
  
  // Cache result
  this.cache.set(key, {
    result,
    timestamp: Date.now()
  });
  
  return result;
}
```

### 3. Performance Monitoring

Integrate with performance monitoring:

```typescript
import { performanceMonitoringService } from '@/services/performance-monitoring-service';

async performOperation(): Promise<void> {
  const messageId = performanceMonitoringService.trackMessageStart('operation');
  
  try {
    // Perform operation
    await this.doWork();
  } finally {
    performanceMonitoringService.trackMessageEnd(messageId);
  }
}
```

### 4. Type Safety

Use TypeScript types consistently:

```typescript
import { ProvenanceNode, ProvenanceChain, Slide, Card } from '@/services/provenance-slide-service';

// Use proper types
function processNode(node: ProvenanceNode): ProvenanceNode {
  // Type-safe operations
  return {
    ...node,
    metadata: {
      ...node.metadata,
      processed: true
    }
  };
}
```

### 5. Documentation

Document all public methods with JSDoc:

```typescript
/**
 * Brief description of the method.
 * 
 * Detailed description explaining what the method does,
 * any important behavior, edge cases, or performance considerations.
 * 
 * @param {Type} paramName - Description of parameter
 * @returns {ReturnType} Description of return value
 * @throws {ErrorType} When this error is thrown
 * @example
 * ```typescript
 * // Example usage
 * const result = service.method(param);
 * ```
 */
public async method(param: Type): Promise<ReturnType> {
  // Implementation
}
```

## Related Documentation

- [Implementation Details](./03-IMPLEMENTATION-DETAILS.md) - Detailed technical documentation
- [API Reference](./04-API-REFERENCE.md) - Complete API documentation
- [Troubleshooting Guide](./06-TROUBLESHOOTING-GUIDE.md) - Common issues and solutions
- [Workflow Walkthroughs](./07-WORKFLOW-WALKTHROUGHS.md) - Step-by-step workflows

---

**Last Updated**: 2025-01-07  
**Version**: 1.0.0

