---
id: api-reference-federated-provenance-canvas
title: "API Reference: Federated Provenance Canvas Integration"
level: intermediate
type: reference
tags: [federated-provenance, canvas-integration, api-reference, documentation, offscreen-workers, bipartite-bqf]
keywords: [api-reference, api-documentation, provenance-slide-service, provenance-canvas-worker-service, automaton-file-generator-service, types, interfaces]
prerequisites: [implementation-details-federated-provenance-canvas]
enables: [developer-usage, integration-guide]
related: [federated-provenance-canvas-integration-docs, implementation-details-federated-provenance-canvas, phase-completion-summary-federated-provenance-canvas]
readingTime: 30
difficulty: 3
version: "1.0.0"
blackboard:
  status: completed
  assignedAgent: "Query-Interface-Agent"
  lastUpdate: "2025-01-07"
  dependencies: [implementation-details-federated-provenance-canvas]
  watchers: ["6D-Intelligence-Agent", "4D-Network-Agent"]
---

# API Reference: Federated Provenance Canvas Integration

**Date**: 2025-01-07  
**Version**: 1.0.0

## Provenance Slide Service

**File**: `ui/src/services/provenance-slide-service.ts`  
**Export**: `provenanceSlideService`

### Methods

#### `init(): Promise<void>`
Initializes the service and its dependencies.

```typescript
await provenanceSlideService.init();
```

#### `buildProvenanceChain(evolutionPath: string): Promise<ProvenanceChain>`
Builds a provenance chain from an evolution directory.

**Parameters**:
- `evolutionPath: string` - Path to evolution directory

**Returns**: `Promise<ProvenanceChain>`

**Example**:
```typescript
const chain = await provenanceSlideService.buildProvenanceChain('/evolutions/advanced-automaton');
```

#### `generateSlidesFromEvolution(evolutionPath: string): Promise<Slide[]>`
Generates slides from an evolution directory (one per recursion level 0D→7D→0D).

**Parameters**:
- `evolutionPath: string` - Path to evolution directory

**Returns**: `Promise<Slide[]>`

**Example**:
```typescript
const slides = await provenanceSlideService.generateSlidesFromEvolution('/evolutions/advanced-automaton');
```

#### `generateCardsForDimension(dimension: string, nodes: ProvenanceNode[]): Promise<Card[]>`
Generates cards grouped by pattern for a dimension.

**Parameters**:
- `dimension: string` - Dimension (e.g., '0D', '1D', ...)
- `nodes: ProvenanceNode[]` - Nodes for the dimension

**Returns**: `Promise<Card[]>`

**Example**:
```typescript
const cards = await provenanceSlideService.generateCardsForDimension('0D', nodes);
```

### Types

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
  [key: string]: any;
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

## Provenance Canvas Worker Service

**File**: `ui/src/services/provenance-canvas-worker-service.ts`  
**Export**: `provenanceCanvasWorkerService`

### Methods

#### `init(canvas: OffscreenCanvas, options: CanvasOptions): Promise<void>`
Initializes the offscreen canvas worker.

**Parameters**:
- `canvas: OffscreenCanvas` - Offscreen canvas to render to
- `options: CanvasOptions` - Canvas options

**Returns**: `Promise<void>`

**Example**:
```typescript
await provenanceCanvasWorkerService.init(canvas, {
  width: 800,
  height: 600,
  antialias: true
});
```

#### `loadProvenanceChain(chain: ProvenanceChain): Promise<void>`
Loads a provenance chain into the worker for rendering.

**Parameters**:
- `chain: ProvenanceChain` - Provenance chain to render

**Returns**: `Promise<void>`

**Example**:
```typescript
await provenanceCanvasWorkerService.loadProvenanceChain(chain);
```

#### `updateCamera(position: [number, number, number], target: [number, number, number]): Promise<void>`
Updates the camera position and target.

**Parameters**:
- `position: [number, number, number]` - Camera position
- `target: [number, number, number]` - Camera target

**Returns**: `Promise<void>`

**Example**:
```typescript
await provenanceCanvasWorkerService.updateCamera([10, 10, 10], [0, 0, 0]);
```

#### `resize(width: number, height: number): Promise<void>`
Resizes the canvas.

**Parameters**:
- `width: number` - New width
- `height: number` - New height

**Returns**: `Promise<void>`

**Example**:
```typescript
await provenanceCanvasWorkerService.resize(1024, 768);
```

#### `handleClick(x: number, y: number, width: number, height: number): Promise<ProvenanceNode | null>`
Handles click interactions on the canvas.

**Parameters**:
- `x: number` - Click X coordinate
- `y: number` - Click Y coordinate
- `width: number` - Canvas width
- `height: number` - Canvas height

**Returns**: `Promise<ProvenanceNode | null>`

**Example**:
```typescript
const node = await provenanceCanvasWorkerService.handleClick(400, 300, 800, 600);
if (node) {
  console.log('Selected node:', node);
}
```

#### `handleHover(x: number, y: number, width: number, height: number): Promise<ProvenanceNode | null>`
Handles hover interactions on the canvas.

**Parameters**:
- `x: number` - Hover X coordinate
- `y: number` - Hover Y coordinate
- `width: number` - Canvas width
- `height: number` - Canvas height

**Returns**: `Promise<ProvenanceNode | null>`

**Example**:
```typescript
const node = await provenanceCanvasWorkerService.handleHover(400, 300, 800, 600);
if (node) {
  console.log('Hovered node:', node);
}
```

#### `dispose(): Promise<void>`
Disposes of the worker and cleans up resources.

**Returns**: `Promise<void>`

**Example**:
```typescript
await provenanceCanvasWorkerService.dispose();
```

### Types

```typescript
interface CanvasOptions {
  width: number;
  height: number;
  antialias?: boolean;
}
```

## Automaton File Generator Service

**File**: `ui/src/services/automaton-file-generator-service.ts`  
**Export**: `automatonFileGeneratorService`

### Methods

#### `generateKernelCanvasL(automatonState: any): Promise<CanvasLFileContent>`
Generates `automaton.kernel.canvasl` from automaton state.

**Parameters**:
- `automatonState: any` - Automaton state object

**Returns**: `Promise<CanvasLFileContent>`

**Example**:
```typescript
const kernel = await automatonFileGeneratorService.generateKernelCanvasL(automatonState);
```

#### `generateSeedCanvasL(seedData: any): Promise<CanvasLFileContent>`
Generates `automaton.seed.canvasl` with versioning.

**Parameters**:
- `seedData: any` - Seed data object

**Returns**: `Promise<CanvasLFileContent>`

**Example**:
```typescript
const seed = await automatonFileGeneratorService.generateSeedCanvasL(seedData);
```

#### `generateMetaverseTopologyCanvasL(topologyData: any): Promise<CanvasLFileContent>`
Generates `metaverse.topology.canvasl` from topology partition.

**Parameters**:
- `topologyData: any` - Topology data object

**Returns**: `Promise<CanvasLFileContent>`

**Example**:
```typescript
const topology = await automatonFileGeneratorService.generateMetaverseTopologyCanvasL(topologyData);
```

#### `generateMetaverseSystemCanvasL(systemData: any): Promise<CanvasLFileContent>`
Generates `metaverse.system.canvasl` from system partition.

**Parameters**:
- `systemData: any` - System data object

**Returns**: `Promise<CanvasLFileContent>`

**Example**:
```typescript
const system = await automatonFileGeneratorService.generateMetaverseSystemCanvasL(systemData);
```

#### `saveCanvasLFile(content: CanvasLFileContent, filepath: string): Promise<void>`
Saves a CanvasL file to disk.

**Parameters**:
- `content: CanvasLFileContent` - CanvasL file content
- `filepath: string` - File path to save to

**Returns**: `Promise<void>`

**Example**:
```typescript
await automatonFileGeneratorService.saveCanvasLFile(kernel, '/evolutions/advanced-automaton/automaton.kernel.canvasl');
```

### Types

```typescript
interface CanvasLFileContent {
  version: string;
  schema: string;
  objects: any[];
}
```

## Extended Services API

### Agent Provenance Query Service

**File**: `ui/src/services/agent-provenance-query-service.ts`  
**Export**: `agentProvenanceQueryService`

#### New Methods

##### `queryCanvasLFile(canvasLFile: string, query: string, queryType: QueryType): Promise<any>`
Queries a specific CanvasL file for provenance information.

**Parameters**:
- `canvasLFile: string` - Path to CanvasL file
- `query: string` - Query string (SPARQL, Prolog, or Datalog)
- `queryType: QueryType` - Type of query

**Returns**: `Promise<any>`

**Example**:
```typescript
const results = await agentProvenanceQueryService.queryCanvasLFile(
  '/evolutions/advanced-automaton/automaton.kernel.canvasl',
  'SELECT ?node WHERE { ?node rdf:type evolution:Pattern }',
  QueryType.SPARQL
);
```

##### `queryFederatedProvenance(query: FederatedProvenanceQuery): Promise<any>`
Executes federated provenance queries across multiple CanvasL files.

**Parameters**:
- `query: FederatedProvenanceQuery` - Federated query object

**Returns**: `Promise<any>`

**Example**:
```typescript
const results = await agentProvenanceQueryService.queryFederatedProvenance({
  files: [
    '/evolutions/advanced-automaton/automaton.kernel.canvasl',
    '/evolutions/continuous-automaton/automaton.kernel.canvasl'
  ],
  query: 'SELECT ?provenance WHERE { ?entry prov:wasDerivedFrom ?provenance }',
  queryType: QueryType.SPARQL
});
```

##### `extractProvenanceFromCanvasL(canvasLFile: string): Promise<any[]>`
Extracts raw provenance data from a CanvasL file.

**Parameters**:
- `canvasLFile: string` - Path to CanvasL file

**Returns**: `Promise<any[]>`

**Example**:
```typescript
const provenance = await agentProvenanceQueryService.extractProvenanceFromCanvasL(
  '/evolutions/advanced-automaton/automaton.kernel.canvasl'
);
```

### CanvasL 3D Service

**File**: `ui/src/services/canvasl-3d-service.ts`  
**Export**: `canvasl3DService`

#### New Methods

##### `loadBipartiteCanvasL(topologyFile: string, systemFile: string): Promise<BipartiteCanvas3D>`
Loads topology and system partitions as separate Canvas3D structures.

**Parameters**:
- `topologyFile: string` - Path to topology CanvasL file
- `systemFile: string` - Path to system CanvasL file

**Returns**: `Promise<BipartiteCanvas3D>`

**Example**:
```typescript
const bipartite = await canvasl3DService.loadBipartiteCanvasL(
  '/evolutions/advanced-automaton/metaverse.topology.canvasl',
  '/evolutions/advanced-automaton/metaverse.system.canvasl'
);
```

##### `renderBipartitePartition(canvas3D: Canvas3D, partition: 'topology' | 'system'): Canvas3D`
Filters and renders a specific partition from a unified Canvas3D.

**Parameters**:
- `canvas3D: Canvas3D` - Unified Canvas3D
- `partition: 'topology' | 'system'` - Partition to render

**Returns**: `Canvas3D`

**Example**:
```typescript
const topology3D = canvasl3DService.renderBipartitePartition(canvas3D, 'topology');
```

##### `extractBipartiteStructure(canvas3D: Canvas3D): BipartiteCanvas3D`
Extracts bipartite structure from a unified Canvas3D.

**Parameters**:
- `canvas3D: Canvas3D` - Unified Canvas3D

**Returns**: `BipartiteCanvas3D`

**Example**:
```typescript
const bipartite = canvasl3DService.extractBipartiteStructure(canvas3D);
```

### Bipartite Service

**File**: `ui/src/services/bipartite-service.ts`  
**Export**: `bipartiteService`

#### New Methods

##### `encodeBQF(dimension: string, partition: 'topology' | 'system'): BQFCoefficients`
Encodes a dimension as BQF coefficients.

**Parameters**:
- `dimension: string` - Dimension (e.g., '0D', '1D', ...)
- `partition: 'topology' | 'system'` - Partition type

**Returns**: `BQFCoefficients`

**Example**:
```typescript
const bqf = bipartiteService.encodeBQF('0D', 'topology');
// Returns: { a: 1, b: 0, c: 0 }
```

##### `buildBipartiteGraphFromCanvasL(canvasLFile: string): Promise<BipartiteGraph>`
Builds a bipartite graph from CanvasL files.

**Parameters**:
- `canvasLFile: string` - Path to CanvasL file

**Returns**: `Promise<BipartiteGraph>`

**Example**:
```typescript
const graph = await bipartiteService.buildBipartiteGraphFromCanvasL(
  '/evolutions/advanced-automaton/automaton.kernel.canvasl'
);
```

##### `validateBipartiteBQF(graph: BipartiteGraph): Promise<{ valid: boolean; errors: string[] }>`
Validates BQF structure consistency.

**Parameters**:
- `graph: BipartiteGraph` - Bipartite graph to validate

**Returns**: `Promise<{ valid: boolean; errors: string[] }>`

**Example**:
```typescript
const validation = await bipartiteService.validateBipartiteBQF(graph);
if (!validation.valid) {
  console.error('Validation errors:', validation.errors);
}
```

##### `syncBipartiteFrontmatter(graph: BipartiteGraph, targetDirectory: string): Promise<{ updatedFiles: string[] }>`
Synchronizes CanvasL bipartite metadata with frontmatter.

**Parameters**:
- `graph: BipartiteGraph` - Bipartite graph
- `targetDirectory: string` - Target directory for frontmatter files

**Returns**: `Promise<{ updatedFiles: string[] }>`

**Example**:
```typescript
const result = await bipartiteService.syncBipartiteFrontmatter(graph, './docs');
console.log('Updated files:', result.updatedFiles);
```

## Unified Provenance Canvas Component

**File**: `ui/src/components/UnifiedProvenanceCanvas/UnifiedProvenanceCanvas.tsx`

### Props

```typescript
interface UnifiedProvenanceCanvasProps extends MetaverseCanvas3DProps {
  provenanceChain?: ProvenanceChain;
  showDimensionalCanvas?: boolean;
  showProvenanceCanvas?: boolean;
}
```

### Usage

```typescript
import { UnifiedProvenanceCanvas } from '@/components/UnifiedProvenanceCanvas';
import { provenanceSlideService } from '@/services/provenance-slide-service';

function MyComponent() {
  const [chain, setChain] = useState<ProvenanceChain | null>(null);
  
  useEffect(() => {
    provenanceSlideService.buildProvenanceChain('/evolutions/advanced-automaton')
      .then(setChain);
  }, []);
  
  return (
    <UnifiedProvenanceCanvas
      filename="automaton.kernel.canvasl"
      provenanceChain={chain}
      showDimensionalCanvas={true}
      showProvenanceCanvas={true}
    />
  );
}
```

## Error Handling

All services use consistent error handling patterns:

- **Database errors**: Fallback to file system fetch
- **Fetch errors**: Fallback to empty arrays/defaults
- **Worker errors**: Log and continue, or throw if critical
- **Validation errors**: Return validation result with errors array

## Type Definitions

All types are exported from their respective service files. Common types are also available:

```typescript
// From provenance-slide-service.ts
export type { ProvenanceNode, ProvenanceEdge, ProvenanceChain, Slide, Card };

// From canvasl-3d-service.ts
export type { BipartiteCanvas3D, Canvas3D, Node3D, Edge3D };

// From bipartite-service.ts
export type { BipartiteGraph, BQFCoefficients };
```

