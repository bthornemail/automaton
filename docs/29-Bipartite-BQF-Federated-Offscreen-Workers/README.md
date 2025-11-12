---
id: federated-provenance-canvas-integration-docs
title: "Federated Provenance Canvas Integration Documentation"
level: foundational
type: documentation
tags: [federated-provenance, canvas-integration, offscreen-workers, bipartite-bqf, web-workers, three-js]
keywords: [federated-provenance-canvas, offscreen-canvas, web-worker, provenance-chain, slide-generation, card-generation, bipartite-bqf, dimensional-progression]
prerequisites: [bipartite-bqf-extension-rfc2119-spec, federated-provenance-meta-log-spec, canvasl-rfc2119-spec]
enables: [federated-provenance-canvas-integration-plan, phase-completion-summary-federated-provenance-canvas, implementation-details-federated-provenance-canvas, api-reference-federated-provenance-canvas, worker-bundling-verification]
related: [bipartite-bqf-extension-rfc2119-spec, federated-provenance-meta-log-spec, canvasl-rfc2119-spec, automatons-docs-readme, automatons-canvasl-docs-readme]
readingTime: 10
difficulty: 2
version: "1.0.0"
blackboard:
  status: completed
  assignedAgent: "4D-Network-Agent"
  lastUpdate: "2025-01-07"
  dependencies: [bipartite-bqf-extension-rfc2119-spec, federated-provenance-meta-log-spec, canvasl-rfc2119-spec]
  watchers: ["6D-Intelligence-Agent", "Query-Interface-Agent"]
---

# Federated Provenance Canvas Integration Documentation

**Status**: ✅ **COMPLETED**  
**Date**: 2025-01-07  
**Version**: 1.0.0

## Overview

This documentation covers the integration of the `template-projector` functionality into the UI services architecture, creating a unified system for visualizing federated identity provenance chains of automaton self-execution patterns. The system represents self-modification as slides (one per recursion level 0D→7D→0D) and JSONL lines as cards (grouped by pattern), using offscreen canvas rendering and Bipartite-BQF structure.

## Documentation Structure

### 1. [Federated Provenance Canvas Integration Plan](./01-Federated-Provance-Canvas-Intergration-Plan.md)
The original implementation plan outlining all tasks, file structure, and success criteria.

### 2. [Phase Completion Summary](./02-PHASE-COMPLETION-SUMMARY.md)
Comprehensive summary of completed tasks, implementation statistics, and success criteria status.

### 3. [Implementation Details](./03-IMPLEMENTATION-DETAILS.md)
Detailed technical documentation covering:
- Architecture overview
- Service implementations
- Algorithms and data structures
- Error handling
- Performance considerations

### 4. [API Reference](./04-API-REFERENCE.md)
Complete API documentation for all services, methods, and types.

## Quick Start

### 1. Initialize Services

```typescript
import { provenanceSlideService } from '@/services/provenance-slide-service';
import { provenanceCanvasWorkerService } from '@/services/provenance-canvas-worker-service';

// Initialize services
await provenanceSlideService.init();
```

### 2. Build Provenance Chain

```typescript
const chain = await provenanceSlideService.buildProvenanceChain(
  '/evolutions/advanced-automaton'
);
```

### 3. Generate Slides

```typescript
const slides = await provenanceSlideService.generateSlidesFromEvolution(
  '/evolutions/advanced-automaton'
);
```

### 4. Render in Component

```typescript
import { UnifiedProvenanceCanvas } from '@/components/UnifiedProvenanceCanvas';

<UnifiedProvenanceCanvas
  filename="automaton.kernel.canvasl"
  provenanceChain={chain}
  showDimensionalCanvas={true}
  showProvenanceCanvas={true}
/>
```

## Key Features

### ✅ Federated Provenance Tracking
- Cross-file provenance queries
- Provenance history aggregation
- Self-reference pattern extraction
- Dimensional progression tracking

### ✅ Slide Generation
- One slide per recursion level (0D→7D→0D)
- Church encoding display
- BQF form representation
- Dimensional topology visualization

### ✅ Card Generation
- Pattern-based grouping
- JSONL line aggregation
- Provenance history tracking
- Church encoding metadata

### ✅ Bipartite-BQF Support
- Topology/system partition separation
- BQF encoding for dimensions
- Horizontal edge mapping
- Vertical edge progression

### ✅ Offscreen Canvas Rendering
- Web Worker integration
- Three.js rendering
- Provenance chain 3D visualization
- Interaction handling

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
│ Provenance   │  │ Provenance  │  │ Automaton   │
│ Slide        │  │ Canvas      │  │ File        │
│ Service      │  │ Worker      │  │ Generator   │
│              │  │ Service      │  │ Service     │
└───────┬──────┘  └──────┬──────┘  └──────┬──────┘
        │                │                │
        └────────────────┼────────────────┘
                         │
        ┌────────────────┼────────────────┐
        │                │                │
┌───────▼──────┐  ┌──────▼──────┐  ┌──────▼──────┐
│ Agent        │  │ CanvasL     │  │ Bipartite   │
│ Provenance   │  │ 3D Service  │  │ Service     │
│ Query        │  │             │  │             │
│ Service      │  │             │  │             │
└──────────────┘  └─────────────┘  └─────────────┘
```

## File Structure

```
ui/src/
├── services/
│   ├── provenance-slide-service.ts          ✅ NEW
│   ├── provenance-canvas-worker-service.ts  ✅ NEW
│   ├── automaton-file-generator-service.ts  ✅ NEW
│   ├── projector/                          ✅ MIGRATED
│   │   ├── Projector.ts
│   │   ├── MetaLogBridge.ts
│   │   ├── MacroExpander.ts
│   │   ├── CanvasLExecutor.ts
│   │   └── TopicSlideGenerator.ts
│   ├── agent-coordinator/                 ✅ MIGRATED
│   │   ├── AgentCoordinator.ts
│   │   ├── DimensionalAgent.ts
│   │   ├── ContentLoader.ts
│   │   └── agents/ (0D-7D agents)
│   ├── agent-provenance-query-service.ts   ✅ EXTENDED
│   ├── canvasl-3d-service.ts              ✅ EXTENDED
│   └── bipartite-service.ts                ✅ EXTENDED
├── components/
│   └── UnifiedProvenanceCanvas/            ✅ NEW
│       ├── UnifiedProvenanceCanvas.tsx
│       └── index.ts
└── workers/
    └── provenance-canvas-worker.ts          ✅ ENHANCED
```

## Dependencies

### External
- **meta-log-db/browser**: CanvasLMetaverseBrowser for CanvasL operations
- **three**: Three.js for 3D rendering
- **@react-three/fiber**: React Three.js integration
- **@react-three/drei**: Three.js helpers

### Internal
- **template-projector**: Source for migration (now integrated)
- **docs/28-Canvasl-Frontmatter-Knowledge-Model**: Bipartite-BQF specification
- **docs/13-Federated-Provenance-Meta-Log**: Federated provenance requirements

## Success Criteria

All success criteria have been met:

1. ✅ Template-projector functionality migrated to services
2. ✅ Provenance chains built from automaton self-execution patterns
3. ✅ Slides generated (one per recursion level 0D→7D→0D)
4. ✅ Cards generated (grouped by pattern)
5. ✅ Offscreen canvas rendering working
6. ✅ Bipartite-BQF structure implemented (topology/system files)
7. ✅ Federated provenance tracking across multiple files
8. ✅ Unified component combining MetaverseCanvas3D and DimensionalCanvas

## Next Steps

### Immediate
1. Add unit tests for new services
2. Add integration tests for UnifiedProvenanceCanvas
3. Verify worker bundling in production build
4. Add error handling improvements

### Future Enhancements
1. Real-time provenance chain updates
2. Interactive slide editing
3. Card detail views
4. Export provenance chains to various formats
5. Performance optimizations for large chains

## Related Documentation

- **docs/28-Canvasl-Frontmatter-Knowledge-Model/**: Bipartite-BQF specification
- **docs/13-Federated-Provenance-Meta-Log/**: Federated provenance requirements
- **docs/11-Automatons/**: Automaton execution documentation
- **docs/12-Automatons-CanvasL/**: CanvasL format integration

## Support

For questions or issues, please refer to:
- Implementation details: [03-IMPLEMENTATION-DETAILS.md](./03-IMPLEMENTATION-DETAILS.md)
- API reference: [04-API-REFERENCE.md](./04-API-REFERENCE.md)
- Phase summary: [02-PHASE-COMPLETION-SUMMARY.md](./02-PHASE-COMPLETION-SUMMARY.md)

