---
id: implementation-details-federated-provenance-canvas
title: "Implementation Details: Federated Provenance Canvas Integration"
level: advanced
type: guide
tags: [federated-provenance, canvas-integration, implementation-details, technical-guide, offscreen-workers, bipartite-bqf, architecture]
keywords: [implementation-details, technical-guide, architecture, service-implementations, algorithms, data-structures, error-handling, performance]
prerequisites: [federated-provenance-canvas-integration-plan]
enables: [api-reference-federated-provenance-canvas, phase-completion-summary-federated-provenance-canvas, worker-bundling-verification]
related: [federated-provenance-canvas-integration-docs, federated-provenance-canvas-integration-plan, api-reference-federated-provenance-canvas]
readingTime: 30
difficulty: 4
version: "1.0.0"
blackboard:
  status: completed
  assignedAgent: "6D-Intelligence-Agent"
  lastUpdate: "2025-01-07"
  dependencies: [federated-provenance-canvas-integration-plan]
  watchers: ["4D-Network-Agent", "Query-Interface-Agent", "2D-Structural-Agent"]
---

# Implementation Details: Federated Provenance Canvas Integration

**Date**: 2025-01-07  
**Version**: 1.0.0

## Architecture Overview

The federated provenance canvas integration follows a layered architecture:

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

## Service Implementations

### 1. Provenance Slide Service

**File**: `ui/src/services/provenance-slide-service.ts`

#### Core Responsibilities
- Build provenance chains from evolution directories
- Generate slides (one per recursion level 0D→7D→0D)
- Generate cards (grouped by pattern)
- Extract self-execution patterns
- Track federated provenance

#### Key Methods

##### `buildProvenanceChain(evolutionPath: string): Promise<ProvenanceChain>`
Builds a provenance chain from an evolution directory.

**Process**:
1. Load evolution files (database → fetch → fallback)
2. Extract self-execution patterns with federated provenance
3. Build nodes for each pattern
4. Build edges for dimensional progression
5. Build edges for cross-file references

**Returns**: `ProvenanceChain` with nodes and edges

##### `generateSlidesFromEvolution(evolutionPath: string): Promise<Slide[]>`
Generates slides from an evolution directory.

**Process**:
1. Build provenance chain
2. Group nodes by dimension
3. Create one slide per dimension (0D→7D→0D)
4. Generate cards for each dimension
5. Generate slide content with Church encoding and BQF

**Returns**: Array of `Slide` objects

##### `generateCardsForDimension(dimension: string, nodes: ProvenanceNode[]): Promise<Card[]>`
Generates cards grouped by pattern for a dimension.

**Process**:
1. Group nodes by pattern
2. Aggregate JSONL lines per pattern
3. Collect provenance history
4. Calculate BQF coefficients
5. Create card objects

**Returns**: Array of `Card` objects

#### Data Structures

```typescript
interface ProvenanceNode {
  id: string;
  type: 'agent' | 'document' | 'code' | 'interaction' | 'evolution';
  position: [number, number, number];
  metadata: {
    timestamp: number;
    file: string;
    line: number;
    agentId: string;
    dimension?: string;
    churchEncoding?: string;
    pattern?: string;
  };
  data: any;
}

interface ProvenanceEdge {
  id: string;
  type: 'consumes' | 'produces' | 'references' | 'evolves' | 'interacts';
  from: string;
  to: string;
  metadata: {
    timestamp: number;
    weight: number;
    context: string;
  };
}

interface ProvenanceChain {
  nodes: ProvenanceNode[];
  edges: ProvenanceEdge[];
}

interface Slide {
  id: string;
  type: string;
  title?: string;
  dimension?: string;
  description?: string;
  content?: string;
  provenanceChain?: ProvenanceChain;
  cards?: Card[];
}

interface Card {
  id: string;
  pattern: string;
  jsonlLines: any[];
  metadata: {
    churchEncoding?: string;
    bqfCoefficients?: number[];
    provenanceHistory?: any[];
  };
}
```

### 2. Provenance Canvas Worker Service

**File**: `ui/src/services/provenance-canvas-worker-service.ts`

#### Core Responsibilities
- Manage offscreen canvas worker lifecycle
- Handle worker communication
- Load provenance chains into worker
- Handle camera and interaction events

#### Key Methods

##### `init(canvas: OffscreenCanvas, options: CanvasOptions): Promise<void>`
Initializes the offscreen canvas worker.

**Process**:
1. Create worker instance
2. Send init message with canvas and options
3. Wait for initialization confirmation
4. Set up message handlers

##### `loadProvenanceChain(chain: ProvenanceChain): Promise<void>`
Loads a provenance chain into the worker for rendering.

**Process**:
1. Send load message with chain data
2. Wait for loaded confirmation
3. Update internal state

##### `handleClick(x: number, y: number, width: number, height: number): Promise<ProvenanceNode | null>`
Handles click interactions on the canvas.

**Process**:
1. Send interact message with click coordinates
2. Wait for nodeSelected response
3. Return selected node or null

#### Worker Communication Protocol

```typescript
// Main thread → Worker
interface WorkerMessage {
  type: 'init' | 'load' | 'query' | 'render' | 'interact' | 'updateCamera' | 'resize' | 'dispose';
  payload: any;
}

// Worker → Main thread
interface WorkerResponse {
  type: 'initialized' | 'loaded' | 'nodeSelected' | 'error';
  payload?: any;
}
```

### 3. Automaton File Generator Service

**File**: `ui/src/services/automaton-file-generator-service.ts`

#### Core Responsibilities
- Generate standard automaton CanvasL files
- Maintain Bipartite-BQF structure
- Generate kernel, seed, topology, and system files

#### Key Methods

##### `generateKernelCanvasL(automatonState: any): Promise<CanvasLFileContent>`
Generates `automaton.kernel.canvasl` from automaton state.

**Process**:
1. Extract dimensional topology (0D-7D)
2. Extract R5RS function registry
3. Extract self-reference patterns
4. Extract Church encoding patterns
5. Format as CanvasL

##### `generateMetaverseTopologyCanvasL(topologyData: any): Promise<CanvasLFileContent>`
Generates `metaverse.topology.canvasl` from topology partition.

**Process**:
1. Extract topology nodes (mathematical foundations)
2. Extract Church encoding patterns
3. Extract dimensional progression
4. Calculate BQF coefficients
5. Format as CanvasL

##### `generateMetaverseSystemCanvasL(systemData: any): Promise<CanvasLFileContent>`
Generates `metaverse.system.canvasl` from system partition.

**Process**:
1. Extract system nodes (computational implementations)
2. Extract OpenCode operations
3. Extract network operations
4. Extract AI/consensus/quantum systems
5. Format as CanvasL

### 4. Extended Services

#### Agent Provenance Query Service Extensions

**File**: `ui/src/services/agent-provenance-query-service.ts`

##### New Methods

**`queryCanvasLFile(canvasLFile: string, query: string, queryType: QueryType): Promise<any>`**
Queries a specific CanvasL file for provenance information.

**`queryFederatedProvenance(query: FederatedProvenanceQuery): Promise<any>`**
Executes federated provenance queries across multiple CanvasL files.

**`extractProvenanceFromCanvasL(canvasLFile: string): Promise<any[]>`**
Extracts raw provenance data from a CanvasL file.

#### CanvasL 3D Service Extensions

**File**: `ui/src/services/canvasl-3d-service.ts`

##### New Methods

**`loadBipartiteCanvasL(topologyFile: string, systemFile: string): Promise<BipartiteCanvas3D>`**
Loads topology and system partitions as separate Canvas3D structures.

**`renderBipartitePartition(canvas3D: Canvas3D, partition: 'topology' | 'system'): Canvas3D`**
Filters and renders a specific partition from a unified Canvas3D.

**`extractBipartiteStructure(canvas3D: Canvas3D): BipartiteCanvas3D`**
Extracts bipartite structure from a unified Canvas3D.

#### Bipartite Service Extensions

**File**: `ui/src/services/bipartite-service.ts`

##### New Methods

**`encodeBQF(dimension: string, partition: 'topology' | 'system'): BQFCoefficients`**
Encodes a dimension as BQF coefficients.

**BQF Encoding Rules**:
- Topology partition: `a=1, b=0, c=dimNum`
- System partition: `a=dimNum, b=1, c=1`

**`buildBipartiteGraphFromCanvasL(canvasLFile: string): Promise<BipartiteGraph>`**
Builds a bipartite graph from CanvasL files.

**`validateBipartiteBQF(graph: BipartiteGraph): Promise<{ valid: boolean; errors: string[] }>`**
Validates BQF structure consistency.

**`syncBipartiteFrontmatter(graph: BipartiteGraph, targetDirectory: string): Promise<{ updatedFiles: string[] }>`**
Synchronizes CanvasL bipartite metadata with frontmatter.

## Provenance Chain Building Algorithm

### Step 1: Load Evolution Files
```typescript
async loadEvolutionFiles(evolutionPath: string): Promise<any[]>
```
1. Try database query for evolution files
2. Fallback to fetch from file system
3. Parse JSONL lines
4. Filter for entries with selfReference

### Step 2: Extract Patterns with Provenance
```typescript
async extractSelfExecutionPatternsWithProvenance(files: any[]): Promise<any[]>
```
1. Group files by source file
2. For each entry with selfReference:
   - Query federated provenance
   - Extract Church encoding
   - Infer dimension from pattern
   - Infer agent from dimension
   - Collect provenance history
3. Sort by dimension and timestamp

### Step 3: Build Nodes
```typescript
for (const pattern of patterns) {
  const node: ProvenanceNode = {
    id: `pattern-${pattern.id}`,
    type: 'evolution',
    position: calculatePosition(pattern.dimension),
    metadata: { ... },
    data: { ...pattern, provenanceHistory }
  };
  nodes.push(node);
}
```

### Step 4: Build Edges
```typescript
// Dimensional progression edges
if (toDim === fromDim + 1 || (fromDim === 7 && toDim === 0)) {
  edges.push({
    type: 'evolves',
    from: fromNode.id,
    to: toNode.id,
    ...
  });
}

// Cross-file reference edges
if (fromNode.metadata.file !== toNode.metadata.file) {
  edges.push({
    type: 'references',
    from: fromNode.id,
    to: toNode.id,
    ...
  });
}
```

## Slide Generation Algorithm

### Step 1: Group Nodes by Dimension
```typescript
const nodesByDimension = new Map<string, ProvenanceNode[]>();
for (const node of chain.nodes) {
  const dim = node.metadata.dimension || '0D';
  nodesByDimension.get(dim)?.push(node);
}
```

### Step 2: Create Slides for Each Dimension
```typescript
const dimensions = ['0D', '1D', '2D', '3D', '4D', '5D', '6D', '7D', '0D'];
for (const dimension of dimensions) {
  const dimensionNodes = nodesByDimension.get(dimension) || [];
  const dimensionChain = { nodes: dimensionNodes, edges: ... };
  const cards = await generateCardsForDimension(dimension, dimensionNodes);
  const slide = {
    id: `slide-${dimension}-${Date.now()}`,
    dimension,
    content: generateSlideContent(dimension, dimensionNodes),
    provenanceChain: dimensionChain,
    cards
  };
  slides.push(slide);
}
```

### Step 3: Generate Slide Content
```typescript
private generateSlideContent(dimension: string, nodes: ProvenanceNode[]): string
```
1. Add dimension header
2. Add Church encoding
3. Add BQF form
4. Add node and pattern statistics
5. Add pattern details with Church encoding and BQF
6. Add provenance history summary

## Card Generation Algorithm

### Step 1: Group Nodes by Pattern
```typescript
const patternGroups = new Map<string, { nodes: ProvenanceNode[]; jsonlLines: any[] }>();
for (const node of nodes) {
  const pattern = node.metadata.pattern || 'unknown';
  patternGroups.get(pattern)?.nodes.push(node);
  patternGroups.get(pattern)?.jsonlLines.push(node.data.rawEntry || ...);
}
```

### Step 2: Create Cards
```typescript
for (const [pattern, group] of patternGroups) {
  const allProvenanceHistory = group.nodes.flatMap(n => n.data.provenanceHistory || []);
  const card: Card = {
    id: `card-${dimension}-${pattern}-${Date.now()}`,
    pattern,
    jsonlLines: group.jsonlLines,
    metadata: {
      churchEncoding: ...,
      bqfCoefficients: calculateBQFCoefficients(dimension),
      provenanceHistory: [...]
    }
  };
  cards.push(card);
}
```

## Church Encoding Mapping

```typescript
const churchEncodings: Record<string, string> = {
  '0D': 'λf.λx.x',
  '1D': 'λn.λf.λx.f(nfx)',
  '2D': 'λx.λy.λf.fxy',
  '3D': 'λm.λn.λf.λx.mf(nfx)',
  '4D': 'λm.λn.λf.m(nf)',
  '5D': 'λm.λn.nm',
  '6D': 'λf.(λx.f(xx))(λx.f(xx))',
  '7D': 'λf.λx.f(f(f(f(f(f(f(fx)))))))'
};
```

## BQF Coefficient Calculation

```typescript
private calculateBQFCoefficients(dimension: string): number[] {
  const dimNum = parseInt(dimension.replace('D', '')) || 0;
  // Topology partition: a=1, b=0, c=dimNum
  return [1, 0, dimNum];
}
```

## Offscreen Canvas Worker Implementation

**File**: `ui/src/workers/provenance-canvas-worker.ts`

### Worker Architecture
- Uses Three.js directly in worker context
- Renders to OffscreenCanvas
- Handles interactions via raycasting
- Manages scene, camera, and renderer

### Rendering Pipeline
1. Initialize scene, camera, renderer
2. Load provenance chain
3. Create node meshes (spheres)
4. Create edge lines
5. Render loop (requestAnimationFrame)

### Interaction Handling
- Click: Raycast to find intersected node
- Hover: Raycast to find hovered node
- Camera: Update position and target

## Error Handling

### Provenance Slide Service
- Database query failures → fallback to fetch
- Fetch failures → fallback to empty array
- Pattern extraction failures → use defaults
- Federated provenance query failures → use selfReference as provenance

### Worker Service
- Worker initialization failures → throw error
- Message handling failures → log and continue
- Rendering failures → clear scene and retry

## Performance Considerations

### Provenance Chain Building
- Large evolution directories may need pagination
- Pattern extraction can be expensive for large files
- Federated provenance queries may be slow

### Slide/Card Generation
- Card generation aggregates all JSONL lines (memory intensive)
- Slide content generation is synchronous (may block)

### Worker Rendering
- Large provenance chains may impact rendering performance
- Node/edge creation is done synchronously
- Consider instancing for large numbers of nodes

## Testing Strategy

### Unit Tests
- Test provenance chain building with mock data
- Test slide generation with known patterns
- Test card generation with sample JSONL lines
- Test Church encoding extraction
- Test BQF coefficient calculation

### Integration Tests
- Test end-to-end slide generation from evolution directory
- Test worker communication
- Test federated provenance queries
- Test bipartite graph building

### E2E Tests
- Test UnifiedProvenanceCanvas component
- Test worker rendering
- Test interaction handling
- Test camera controls

## Future Enhancements

1. **Real-time Updates**: Stream provenance chain updates
2. **Caching**: Cache provenance chains and slides
3. **Lazy Loading**: Load slides on demand
4. **Optimization**: Optimize rendering for large chains
5. **Export**: Export provenance chains to various formats
6. **Search**: Search provenance chains by pattern, dimension, agent
7. **Filtering**: Filter nodes/edges by type, dimension, agent
8. **Visualization**: Enhanced 3D visualization with animations

