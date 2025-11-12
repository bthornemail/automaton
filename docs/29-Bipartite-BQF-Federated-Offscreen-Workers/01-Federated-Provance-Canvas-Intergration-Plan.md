---
id: federated-provenance-canvas-integration-plan
title: "Federated Provenance Canvas Integration Plan"
level: intermediate
type: guide
tags: [federated-provenance, canvas-integration, implementation-plan, offscreen-workers, bipartite-bqf]
keywords: [federated-provenance-canvas, integration-plan, template-projector, service-migration, provenance-chain, slide-generation, card-generation, offscreen-canvas]
prerequisites: [federated-provenance-canvas-integration-docs, bipartite-bqf-extension-rfc2119-spec, federated-provenance-meta-log-spec]
enables: [implementation-details-federated-provenance-canvas, api-reference-federated-provenance-canvas, phase-completion-summary-federated-provenance-canvas]
related: [federated-provenance-canvas-integration-docs, bipartite-bqf-extension-rfc2119-spec, federated-provenance-meta-log-spec, canvasl-rfc2119-spec]
readingTime: 15
difficulty: 3
blackboard:
  status: completed
  assignedAgent: "6D-Intelligence-Agent"
  lastUpdate: "2025-01-07"
  dependencies: [federated-provenance-canvas-integration-docs, bipartite-bqf-extension-rfc2119-spec, federated-provenance-meta-log-spec]
  watchers: ["4D-Network-Agent", "2D-Structural-Agent"]
---

# Federated Provenance Canvas Integration Plan

## Overview

Integrate template-projector functionality into UI services to create a unified system for visualizing federated identity provenance chains of automaton self-execution patterns. The system will represent self-modification as slides (one per recursion level 0D→7D→0D) and JSONL lines as cards (grouped by pattern), using offscreen canvas rendering and Bipartite-BQF structure.

## File Structure Requirements

### Standard Automaton Basis CanvasL Files (Per Agent)

Each agent will have:

- **`automaton.kernel.canvasl`**: Standard automaton basis for the agent
- **`automaton.seed.canvasl`**: Versioning and regeneration metadata
- **`metaverse.topology.canvasl`**: Topology partition (Bipartite-BQF left side)
- **`metaverse.system.canvasl`**: System partition (Bipartite-BQF right side)

These files map the "epistemic topology of automaton lattice" with:

- Topology partition: Mathematical foundations, Church encoding patterns
- System partition: Computational implementations, OpenCode operations
- Horizontal edges: Topology ↔ System mappings
- Vertical edges: Dimensional progression (0D→7D)

## Implementation Tasks

### 1. Service Migration and Integration

**Location**: `ui/src/services/`

#### 1.1 Migrate Template-Projector Core

- **Source**: `template-projector/src/projector/` → `ui/src/services/projector/`
  - Migrate `Projector.js` → `Projector.ts`
  - Migrate `MetaLogBridge.js` → `MetaLogBridge.ts`
  - Migrate `MacroExpander.js` → `MacroExpander.ts`
  - Migrate `CanvasLExecutor.js` → `CanvasLExecutor.ts`
  - Migrate `TopicSlideGenerator.js` → `TopicSlideGenerator.ts`

#### 1.2 Migrate Agent System

- **Source**: `template-projector/src/agents/` → `ui/src/services/agent-coordinator/`
  - Migrate `AgentCoordinator.js` → `AgentCoordinator.ts`
  - Migrate `DimensionalAgent.js` → `DimensionalAgent.ts`
  - Migrate all dimensional agents (0D-7D) → TypeScript
  - Migrate `ContentLoader.js` → `ContentLoader.ts`
  - Migrate `KernelLoader.js` → `KernelLoader.ts`

#### 1.3 Create Provenance Slide Service

- **New File**: `ui/src/services/provenance-slide-service.ts`
  - Integrate template-projector slide generation with provenance chain building
  - Generate slides from evolution directories (one chain per evolution)
  - Build provenance chains from automaton self-execution patterns
  - Support federated provenance across multiple CanvasL files

#### 1.4 Create Provenance Canvas Worker Service

- **New File**: `ui/src/services/provenance-canvas-worker-service.ts`
  - Wrapper service for `provenance-canvas-worker.ts`
  - Initialize offscreen canvas workers
  - Manage worker lifecycle and communication
  - Handle provenance chain rendering

### 2. CanvasL File Structure Implementation

**Location**: Per evolution directory (e.g., `evolutions/advanced-automaton/`)

#### 2.1 Standard Automaton Basis Files

- **`automaton.kernel.canvasl`**: Core automaton structure for agent
  - Contains dimensional topology (0D-7D)
  - Contains R5RS function registry
  - Contains self-reference patterns
  - Contains Church encoding patterns

- **`automaton.seed.canvasl`**: Versioning and regeneration
  - Minimal seed for kernel regeneration
  - Regeneration metadata with R5RS functions
  - Version tracking and provenance history
  - Bootstrap sequence

#### 2.2 Bipartite-BQF Metaverse Files

- **`metaverse.topology.canvasl`**: Topology partition (left side)
  - Mathematical foundations
  - Church encoding patterns
  - Dimensional progression (0D→7D)
  - BQF coefficients for each dimension

- **`metaverse.system.canvasl`**: System partition (right side)
  - Computational implementations
  - OpenCode operations
  - Network operations
  - AI/consensus/quantum systems

#### 2.3 File Generation Service

- **New File**: `ui/src/services/automaton-file-generator-service.ts`
  - Generate `automaton.kernel.canvasl` from automaton state
  - Generate `automaton.seed.canvasl` with versioning
  - Generate `metaverse.topology.canvasl` from topology partition
  - Generate `metaverse.system.canvasl` from system partition
  - Maintain Bipartite-BQF structure consistency

### 3. Provenance Chain Building

**Location**: `ui/src/services/provenance-slide-service.ts`

#### 3.1 Evolution Directory Provenance

- Build one provenance chain per evolution directory
- Extract self-execution patterns from automaton files
- Track cross-file provenance relationships
- Preserve federated identity across files

#### 3.2 Self-Execution Pattern Extraction

- Parse automaton CanvasL files for self-reference patterns
- Extract dimensional progression (0D→1D→2D...→7D→0D)
- Track self-modification events
- Build provenance edges between phases

#### 3.3 Federated Provenance Tracking

- Track provenance across multiple CanvasL files
- Preserve provenance history in `provenanceHistory` arrays
- Handle cross-file duplicates (preserve both)
- Handle same-file duplicates (merge provenance history)

### 4. Slide and Card Generation

**Location**: `ui/src/services/provenance-slide-service.ts`

#### 4.1 Slide Generation (One Per Recursion Level)

- **Slide Structure**: One slide per dimensional level (0D, 1D, 2D, ..., 7D, 0D)
- **Slide Content**: 
  - Dimensional topology visualization
  - Church encoding pattern
  - Self-execution pattern for that level
  - Provenance chain nodes for that level
  - Bipartite-BQF representation

#### 4.2 Card Generation (Grouped By Pattern)

- **Card Structure**: Cards grouped by pattern (e.g., "identity", "successor", "pair")
- **Card Content**:
  - All JSONL lines with the same pattern
  - Pattern metadata (Church encoding, BQF coefficients)
  - Provenance information (file, line, timestamp)
  - Self-reference relationships

#### 4.3 Pattern Extraction

- Extract patterns from `selfReference.pattern` fields
- Group JSONL lines by pattern
- Create pattern cards with aggregated information
- Link cards to slides by dimensional level

### 5. Unified Canvas Component

**Location**: `ui/src/components/UnifiedProvenanceCanvas/`

#### 5.1 Component Structure

- **New File**: `ui/src/components/UnifiedProvenanceCanvas/UnifiedProvenanceCanvas.tsx`
  - Combines MetaverseCanvas3D and DimensionalCanvas functionality
  - Integrates with provenance-canvas-worker for offscreen rendering
  - Supports slide navigation (one per recursion level)
  - Displays cards (grouped by pattern)

#### 5.2 Offscreen Canvas Integration

- Use `provenance-canvas-worker.ts` for 3D rendering
- Transfer canvas control to offscreen worker
- Handle worker messages (init, load, interact, updateCamera, resize)
- Render provenance chains in 3D space

#### 5.3 Bipartite-BQF Visualization

- Render topology partition (left side) and system partition (right side)
- Visualize horizontal edges (topology ↔ system mappings)
- Visualize vertical edges (dimensional progression)
- Color-code by dimension and partition

### 6. Integration with Existing Services

#### 6.1 Agent Provenance Query Service

- **File**: `ui/src/services/agent-provenance-query-service.ts`
  - Extend to support CanvasL file queries
  - Query provenance chains from evolution directories
  - Support federated provenance queries across files

#### 6.2 Agent History Logging Service

- **File**: `ui/src/services/agent-history-logging-service.ts`
  - Track self-execution patterns in history
  - Log provenance information for each automaton action
  - Support pattern-based grouping

#### 6.3 CanvasL 3D Service

- **File**: `ui/src/services/canvasl-3d-service.ts`
  - Extend to support Bipartite-BQF structure
  - Support topology/system partition rendering
  - Integrate with provenance chain visualization

### 7. Bipartite-BQF Integration

**Location**: `ui/src/services/bipartite-service.ts` (extend existing)

#### 7.1 Bipartite Graph Building

- Build bipartite graph from CanvasL files
- Separate topology and system partitions
- Extract horizontal edges (h:* edges)
- Extract vertical edges (v:* edges)

#### 7.2 BQF Encoding

- Encode each dimension as BQF (ax² + bxy + cy²)
- Extract BQF coefficients from CanvasL metadata
- Map symbols to polynomials to BQF to R5RS procedures

#### 7.3 Frontmatter Integration

- Sync CanvasL bipartite metadata with frontmatter
- Maintain consistency between CanvasL and frontmatter
- Support Bipartite-BQF frontmatter schema

## File Structure

```
ui/src/
├── services/
│   ├── provenance-slide-service.ts          # NEW: Unified slide/provenance service
│   ├── provenance-canvas-worker-service.ts  # NEW: Worker wrapper service
│   ├── automaton-file-generator-service.ts # NEW: Generate CanvasL files
│   ├── projector/                          # MIGRATED from template-projector
│   │   ├── Projector.ts
│   │   ├── MetaLogBridge.ts
│   │   ├── MacroExpander.ts
│   │   ├── CanvasLExecutor.ts
│   │   └── TopicSlideGenerator.ts
│   ├── agent-coordinator/                 # MIGRATED from template-projector
│   │   ├── AgentCoordinator.ts
│   │   ├── DimensionalAgent.ts
│   │   ├── ContentLoader.ts
│   │   └── agents/ (0D-7D agents)
│   ├── agent-provenance-query-service.ts   # EXTENDED: CanvasL support
│   ├── agent-history-logging-service.ts    # EXTENDED: Pattern tracking
│   ├── canvasl-3d-service.ts              # EXTENDED: Bipartite-BQF support
│   └── bipartite-service.ts                # EXTENDED: BQF encoding
├── components/
│   └── UnifiedProvenanceCanvas/            # NEW: Combined component
│       ├── UnifiedProvenanceCanvas.tsx
│       └── index.ts
└── workers/
    └── provenance-canvas-worker.ts          # ENHANCED: Slide/card support

evolutions/
└── {evolution-name}/
    ├── automaton.kernel.canvasl            # NEW: Standard automaton basis
    ├── automaton.seed.canvasl              # NEW: Versioning
    ├── metaverse.topology.canvasl          # NEW: Topology partition
    └── metaverse.system.canvasl            # NEW: System partition
```

## Dependencies

- **template-projector**: Source for migration
- **meta-log-db/browser**: CanvasLMetaverseBrowser for CanvasL operations
- **provenance-canvas-worker**: Offscreen canvas rendering
- **docs/28-Canvasl-Frontmatter-Knowledge-Model**: Bipartite-BQF specification
- **docs/13-Federated-Provenance-Meta-Log**: Federated provenance requirements

## Success Criteria

1. ✅ Template-projector functionality migrated to services
2. ✅ Provenance chains built from automaton self-execution patterns
3. ✅ Slides generated (one per recursion level 0D→7D→0D)
4. ✅ Cards generated (grouped by pattern)
5. ✅ Offscreen canvas rendering working
6. ✅ Bipartite-BQF structure implemented (topology/system files)
7. ✅ Federated provenance tracking across multiple files
8. ✅ Unified component combining MetaverseCanvas3D and DimensionalCanvas