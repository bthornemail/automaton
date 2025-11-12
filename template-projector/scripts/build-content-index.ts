#!/usr/bin/env tsx
/**
 * Build Content Index
 * 
 * Pre-processes all content sources (wiki, docs, grok_files, evolutions)
 * into a unified JSONL file for agent content population.
 */

import * as fs from 'fs';
import * as path from 'path';
import yaml from 'js-yaml';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

interface DocumentEntry {
  type: 'document';
  id: string;
  source: 'wiki' | 'docs' | 'grok_files' | 'evolutions';
  filePath: string;
  dimension?: string;
  level?: string;
  docType?: string;
  title?: string;
  description?: string;
  tags?: string[];
  keywords?: string[];
  frontmatter?: any;
  body?: string; // Markdown content body
  relationships?: {
    prerequisites?: string[];
    enables?: string[];
    related?: string[];
  };
  readingTime?: number;
  difficulty?: number;
}

interface RelationshipEntry {
  type: 'relationship';
  from: string;
  to: string;
  relType: 'prerequisite' | 'enables' | 'related';
}

interface RDFTripleEntry {
  type: 'rdf-triple';
  subject: string;
  predicate: string;
  object: string;
}

interface BipartiteNode {
  id: string;
  partition: 'topology' | 'system';
  dimension: string;
  metadata: {
    title?: string;
    filePath?: string;
    source?: string;
    [key: string]: any;
  };
}

interface BipartiteEdge {
  id: string;
  from: string;
  to: string;
  type: 'horizontal' | 'vertical';
  label?: string;
}

interface BipartiteGraph {
  nodes: BipartiteNode[];
  edges: BipartiteEdge[];
  metadata: {
    topologyNodeCount: number;
    systemNodeCount: number;
    horizontalEdgeCount: number;
    verticalEdgeCount: number;
  };
}

interface BipartiteGraphEntry {
  type: 'bipartite-graph';
  graph: BipartiteGraph;
}

type ContentIndexEntry = DocumentEntry | RelationshipEntry | RDFTripleEntry | BipartiteGraphEntry;

/**
 * Parse frontmatter from markdown file
 */
function parseFrontmatter(filePath: string): { frontmatter: any | null; body: string } | null {
  try {
    const content = fs.readFileSync(filePath, 'utf-8');
    const frontmatterMatch = content.match(/^---\n([\s\S]*?)\n---\n([\s\S]*)$/);
    
    if (!frontmatterMatch) {
      return { frontmatter: null, body: content };
    }

    const frontmatterYaml = frontmatterMatch[1];
    const body = frontmatterMatch[2];

    if (!frontmatterYaml) {
      return { frontmatter: null, body: content };
    }
    
    const frontmatter = yaml.load(frontmatterYaml) as any;
    return { frontmatter, body: body || '' };
  } catch (error) {
    console.warn(`Failed to parse frontmatter from ${filePath}:`, error);
    return null;
  }
}

/**
 * Find all markdown files in directory
 */
function findMarkdownFiles(dir: string, baseDir: string = dir): string[] {
  const files: string[] = [];
  
  if (!fs.existsSync(dir)) {
    return files;
  }

  const entries = fs.readdirSync(dir, { withFileTypes: true });
  
  for (const entry of entries) {
    const fullPath = path.join(dir, entry.name);
    
    if (entry.isDirectory()) {
      // Skip node_modules and other common ignore dirs
      if (entry.name === 'node_modules' || entry.name === '.git' || entry.name.startsWith('.')) {
        continue;
      }
      files.push(...findMarkdownFiles(fullPath, baseDir));
    } else if (entry.isFile() && entry.name.endsWith('.md')) {
      files.push(fullPath);
    }
  }
  
  return files;
}

/**
 * Generate document ID from file path
 */
function generateDocId(filePath: string, source: string): string {
  // Use relative path as base for ID
  const relativePath = filePath.replace(/^.*\/(wiki|docs|grok_files|evolutions)\//, '');
  const baseName = path.basename(relativePath, '.md');
  const dirName = path.dirname(relativePath).replace(/\//g, '-');
  
  if (dirName && dirName !== '.') {
    return `${source}-${dirName}-${baseName}`.toLowerCase().replace(/[^a-z0-9-]/g, '-');
  }
  return `${source}-${baseName}`.toLowerCase().replace(/[^a-z0-9-]/g, '-');
}

/**
 * Extract dimension from frontmatter, tags/keywords, or file path
 */
function extractDimension(frontmatter: any, filePath?: string): string | undefined {
  // Check direct dimension field
  if (frontmatter.dimension) {
    return frontmatter.dimension;
  }
  
  // Check tags (exact match: "0D", "1D", etc. or prefix: "0d-topology")
  if (frontmatter.tags && Array.isArray(frontmatter.tags)) {
    const exactDim = frontmatter.tags.find((t: string) => /^\d+D$/i.test(t));
    if (exactDim) return exactDim.toUpperCase();
    
    // Check for dimension prefix in tags (e.g., "0d-topology" -> "0D")
    const prefixDim = frontmatter.tags.find((t: string) => /^(\d+)d-/i.test(t));
    if (prefixDim) {
      const match = prefixDim.match(/^(\d+)d-/i);
      if (match) return `${match[1]}D`;
    }
  }
  
  // Check keywords (same logic as tags)
  if (frontmatter.keywords && Array.isArray(frontmatter.keywords)) {
    const exactDim = frontmatter.keywords.find((k: string) => /^\d+D$/i.test(k));
    if (exactDim) return exactDim.toUpperCase();
    
    const prefixDim = frontmatter.keywords.find((k: string) => /^(\d+)d-/i.test(k));
    if (prefixDim) {
      const match = prefixDim.match(/^(\d+)d-/i);
      if (match) return `${match[1]}D`;
    }
  }
  
  // Extract from file path (e.g., "docs/03-Metaverse-Canvas/" -> "3D" if pattern matches)
  if (filePath) {
    const pathDim = filePath.match(/(\d+)[dD]-/i);
    if (pathDim) {
      return `${pathDim[1]}D`;
    }
  }
  
  return undefined;
}

/**
 * Process a single markdown file
 */
function processFile(filePath: string, source: string, workspaceRoot: string): ContentIndexEntry[] {
  const entries: ContentIndexEntry[] = [];
  
  const parsed = parseFrontmatter(filePath);
  if (!parsed) {
    return entries;
  }
  
  const { frontmatter, body } = parsed;
  
  // Skip if no frontmatter
  if (!frontmatter) {
    return entries;
  }
  
  // Generate document ID
  const docId = frontmatter.id || generateDocId(filePath, source);
  
  // Get relative file path
  const relativePath = path.relative(workspaceRoot, filePath);
  
  // Extract dimension
  const dimension = extractDimension(frontmatter, relativePath);
  
  // Create document entry
  const docEntry: DocumentEntry = {
    type: 'document',
    id: docId,
    source: source as any,
    filePath: relativePath,
    dimension,
    level: frontmatter.level,
    docType: frontmatter.type,
    title: frontmatter.title,
    description: frontmatter.description,
    tags: frontmatter.tags,
    keywords: frontmatter.keywords,
    frontmatter,
    body: body || '', // Include markdown body content
    relationships: {
      prerequisites: frontmatter.prerequisites || [],
      enables: frontmatter.enables || [],
      related: frontmatter.related || []
    },
    readingTime: frontmatter.readingTime,
    difficulty: frontmatter.difficulty
  };
  
  entries.push(docEntry);
  
  // Create relationship entries
  if (frontmatter.prerequisites && Array.isArray(frontmatter.prerequisites)) {
    for (const prereq of frontmatter.prerequisites) {
      entries.push({
        type: 'relationship',
        from: docId,
        to: prereq,
        relType: 'prerequisite'
      });
      
      // Create RDF triple
      entries.push({
        type: 'rdf-triple',
        subject: `#${docId}`,
        predicate: 'rdfs:prerequisite',
        object: `#${prereq}`
      });
    }
  }
  
  if (frontmatter.enables && Array.isArray(frontmatter.enables)) {
    for (const enable of frontmatter.enables) {
      entries.push({
        type: 'relationship',
        from: docId,
        to: enable,
        relType: 'enables'
      });
      
      // Create RDF triple
      entries.push({
        type: 'rdf-triple',
        subject: `#${docId}`,
        predicate: 'rdfs:enables',
        object: `#${enable}`
      });
    }
  }
  
  if (frontmatter.related && Array.isArray(frontmatter.related)) {
    for (const related of frontmatter.related) {
      entries.push({
        type: 'relationship',
        from: docId,
        to: related,
        relType: 'related'
      });
      
      // Create RDF triple
      entries.push({
        type: 'rdf-triple',
        subject: `#${docId}`,
        predicate: 'rdfs:seeAlso',
        object: `#${related}`
      });
    }
  }
  
  return entries;
}

/**
 * Build bipartite graph from document entries
 */
function buildBipartiteGraph(entries: ContentIndexEntry[]): BipartiteGraph {
  const nodes: BipartiteNode[] = [];
  const edges: BipartiteEdge[] = [];
  const nodeMap = new Map<string, BipartiteNode>();

  // Extract bipartite nodes from document entries
  for (const entry of entries) {
    if (entry.type !== 'document' || !entry.frontmatter?.bipartite) {
      continue;
    }

    const bipartite = entry.frontmatter.bipartite;
    const partition = bipartite.partition;
    const dimension = bipartite.dimension;

    if (!partition || !dimension) {
      continue;
    }

    const nodeId = `${dimension}-${partition}`;
    
    if (!nodeMap.has(nodeId)) {
      const node: BipartiteNode = {
        id: nodeId,
        partition: partition as 'topology' | 'system',
        dimension: dimension.toUpperCase(),
        metadata: {
          title: entry.title,
          filePath: entry.filePath,
          source: entry.source,
          docId: entry.id
        }
      };
      nodes.push(node);
      nodeMap.set(nodeId, node);
    }
  }

  // Create horizontal edges (topology ↔ system) for same dimension
  const dimensions = new Set(nodes.map(n => n.dimension));
  for (const dim of dimensions) {
    const topologyNode = nodes.find(n => n.dimension === dim && n.partition === 'topology');
    const systemNode = nodes.find(n => n.dimension === dim && n.partition === 'system');

    if (topologyNode && systemNode) {
      edges.push({
        id: `h-${dim}-topology-system`,
        from: topologyNode.id,
        to: systemNode.id,
        type: 'horizontal',
        label: `${dim} topology ↔ system`
      });
    }
  }

  // Create vertical edges (dimensional progression) for same partition
  const dimensionOrder = ['0D', '1D', '2D', '3D', '4D', '5D', '6D', '7D'];
  for (const partition of ['topology', 'system'] as const) {
    for (let i = 0; i < dimensionOrder.length - 1; i++) {
      const prevDim = dimensionOrder[i];
      const currDim = dimensionOrder[i + 1];

      const prevNode = nodes.find(n => n.dimension === prevDim && n.partition === partition);
      const currNode = nodes.find(n => n.dimension === currDim && n.partition === partition);

      if (prevNode && currNode) {
        edges.push({
          id: `v-${prevDim}-${currDim}-${partition}`,
          from: prevNode.id,
          to: currNode.id,
          type: 'vertical',
          label: `${prevDim} → ${currDim} (${partition})`
        });
      }
    }
  }

  // Also create edges from bipartite relationships
  for (const entry of entries) {
    if (entry.type !== 'document' || !entry.frontmatter?.bipartite?.relationships) {
      continue;
    }

    const relationships = entry.frontmatter.bipartite.relationships;
    const dimension = entry.frontmatter.bipartite.dimension?.toUpperCase();
    const partition = entry.frontmatter.bipartite.partition;

    if (!dimension || !partition) {
      continue;
    }

    const currentNodeId = `${dimension}-${partition}`;

    if (relationships.topology && typeof relationships.topology === 'string') {
      const targetNode = nodes.find(n => n.id === relationships.topology || n.metadata.docId === relationships.topology);
      if (targetNode && targetNode.id !== currentNodeId) {
        edges.push({
          id: `rel-topology-${currentNodeId}-${targetNode.id}`,
          from: currentNodeId,
          to: targetNode.id,
          type: 'horizontal',
          label: 'topology relationship'
        });
      }
    }

    if (relationships.system && typeof relationships.system === 'string') {
      const targetNode = nodes.find(n => n.id === relationships.system || n.metadata.docId === relationships.system);
      if (targetNode && targetNode.id !== currentNodeId) {
        edges.push({
          id: `rel-system-${currentNodeId}-${targetNode.id}`,
          from: currentNodeId,
          to: targetNode.id,
          type: 'horizontal',
          label: 'system relationship'
        });
      }
    }
  }

  const topologyNodes = nodes.filter(n => n.partition === 'topology');
  const systemNodes = nodes.filter(n => n.partition === 'system');
  const horizontalEdges = edges.filter(e => e.type === 'horizontal');
  const verticalEdges = edges.filter(e => e.type === 'vertical');

  return {
    nodes,
    edges,
    metadata: {
      topologyNodeCount: topologyNodes.length,
      systemNodeCount: systemNodes.length,
      horizontalEdgeCount: horizontalEdges.length,
      verticalEdgeCount: verticalEdges.length
    }
  };
}

/**
 * Generate relationship graphs
 */
function generateRelationshipGraphs(entries: ContentIndexEntry[]): {
  topologyGraph: any;
  systemGraph: any;
  mappingGraph: any;
  progressionGraph: any;
} {
  const topologyNodes: any[] = [];
  const systemNodes: any[] = [];
  const topologyEdges: any[] = [];
  const systemEdges: any[] = [];
  const mappingEdges: any[] = [];
  const progressionEdges: any[] = [];

  for (const entry of entries) {
    if (entry.type !== 'document' || !entry.frontmatter?.bipartite) {
      continue;
    }

    const bipartite = entry.frontmatter.bipartite;
    const partition = bipartite.partition;
    const dimension = bipartite.dimension?.toUpperCase();

    if (!partition || !dimension) {
      continue;
    }

    const node = {
      id: entry.id,
      dimension,
      partition,
      title: entry.title
    };

    if (partition === 'topology') {
      topologyNodes.push(node);
    } else if (partition === 'system') {
      systemNodes.push(node);
    }

    // Add progression edges
    const dimIndex = ['0D', '1D', '2D', '3D', '4D', '5D', '6D', '7D'].indexOf(dimension);
    if (dimIndex > 0) {
      const prevDim = ['0D', '1D', '2D', '3D', '4D', '5D', '6D', '7D'][dimIndex - 1];
      progressionEdges.push({
        from: `${prevDim}-${partition}`,
        to: `${dimension}-${partition}`,
        dimension: `${prevDim} → ${dimension}`
      });
    }

    // Add mapping edges (topology ↔ system)
    if (partition === 'topology') {
      const systemNode = systemNodes.find(n => n.dimension === dimension);
      if (systemNode) {
        mappingEdges.push({
          from: entry.id,
          to: systemNode.id,
          dimension
        });
      }
    }
  }

  return {
    topologyGraph: { nodes: topologyNodes, edges: topologyEdges },
    systemGraph: { nodes: systemNodes, edges: systemEdges },
    mappingGraph: { nodes: [...topologyNodes, ...systemNodes], edges: mappingEdges },
    progressionGraph: { nodes: [...topologyNodes, ...systemNodes], edges: progressionEdges }
  };
}

/**
 * Build content index from all sources
 */
async function buildContentIndex(): Promise<void> {
  const workspaceRoot = path.resolve(__dirname, '../..');
  const outputPath = path.join(__dirname, '..', 'content-index.jsonl');
  
  console.log('📚 Building content index...');
  console.log(`   Workspace root: ${workspaceRoot}`);
  console.log(`   Output: ${outputPath}`);
  
  const allEntries: ContentIndexEntry[] = [];
  
  // Process each source directory
  const sources = [
    { name: 'wiki', path: path.join(workspaceRoot, 'wiki') },
    { name: 'docs', path: path.join(workspaceRoot, 'docs') },
    { name: 'grok_files', path: path.join(workspaceRoot, 'grok_files') },
    { name: 'evolutions', path: path.join(workspaceRoot, 'evolutions') }
  ];
  
  for (const source of sources) {
    console.log(`\n   Processing ${source.name}...`);
    
    if (!fs.existsSync(source.path)) {
      console.warn(`   ⚠️  Directory not found: ${source.path}`);
      continue;
    }
    
    const files = findMarkdownFiles(source.path);
    console.log(`   Found ${files.length} markdown files`);
    
    let processed = 0;
    for (const file of files) {
      const entries = processFile(file, source.name, workspaceRoot);
      allEntries.push(...entries);
      processed++;
      
      if (processed % 50 === 0) {
        console.log(`   Processed ${processed}/${files.length} files...`);
      }
    }
    
    console.log(`   ✅ Processed ${processed} files from ${source.name}`);
  }
  
  // Build bipartite graph
  console.log('\n   Building bipartite graph...');
  const bipartiteGraph = buildBipartiteGraph(allEntries);
  console.log(`   Topology nodes: ${bipartiteGraph.metadata.topologyNodeCount}`);
  console.log(`   System nodes: ${bipartiteGraph.metadata.systemNodeCount}`);
  console.log(`   Horizontal edges: ${bipartiteGraph.metadata.horizontalEdgeCount}`);
  console.log(`   Vertical edges: ${bipartiteGraph.metadata.verticalEdgeCount}`);

  // Generate relationship graphs
  console.log('\n   Generating relationship graphs...');
  const relationshipGraphs = generateRelationshipGraphs(allEntries);
  console.log(`   Topology graph: ${relationshipGraphs.topologyGraph.nodes.length} nodes`);
  console.log(`   System graph: ${relationshipGraphs.systemGraph.nodes.length} nodes`);
  console.log(`   Mapping graph: ${relationshipGraphs.mappingGraph.edges.length} edges`);
  console.log(`   Progression graph: ${relationshipGraphs.progressionGraph.edges.length} edges`);

  // Add bipartite graph entry
  const graphEntry: BipartiteGraphEntry = {
    type: 'bipartite-graph',
    graph: bipartiteGraph
  };
  allEntries.push(graphEntry);

  // Write to JSONL file
  console.log(`\n   Writing ${allEntries.length} entries to ${outputPath}...`);
  const jsonl = allEntries.map(entry => JSON.stringify(entry)).join('\n');
  fs.writeFileSync(outputPath, jsonl, 'utf-8');
  
  // Print summary
  const docCount = allEntries.filter(e => e.type === 'document').length;
  const relCount = allEntries.filter(e => e.type === 'relationship').length;
  const rdfCount = allEntries.filter(e => e.type === 'rdf-triple').length;
  const graphCount = allEntries.filter(e => e.type === 'bipartite-graph').length;
  
  console.log('\n✅ Content index built successfully!');
  console.log(`   Documents: ${docCount}`);
  console.log(`   Relationships: ${relCount}`);
  console.log(`   RDF Triples: ${rdfCount}`);
  console.log(`   Bipartite Graphs: ${graphCount}`);
  console.log(`   Total entries: ${allEntries.length}`);
  
  // Validate the built content index
  console.log('\n🔍 Validating content index...');
  try {
    const { validateContentIndex } = await import('./validate-content-index.js');
    const validationResult = validateContentIndex(outputPath);
    
    if (validationResult.valid) {
      console.log('✅ Content index validation passed!');
    } else {
      console.warn(`⚠️  Content index has ${validationResult.errors.length} validation errors`);
      if (validationResult.errors.length > 0) {
        console.warn('   First few errors:');
        for (const error of validationResult.errors.slice(0, 5)) {
          console.warn(`     [${error.code}] ${error.message}`);
        }
      }
      if (validationResult.warnings.length > 0) {
        console.warn(`   Warnings: ${validationResult.warnings.length}`);
      }
    }
  } catch (error) {
    console.warn('⚠️  Validation skipped (validator not available):', error instanceof Error ? error.message : String(error));
  }
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  buildContentIndex().catch(error => {
    console.error('❌ Error building content index:', error);
    process.exit(1);
  });
}

export { buildContentIndex };

