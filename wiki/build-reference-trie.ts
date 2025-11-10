#!/usr/bin/env tsx
/**
 * Build Reference Trie
 * 
 * Transforms extracted frontmatter into a hierarchical trie structure
 * for easy navigation and reference.
 */

import * as fs from 'fs';
import * as path from 'path';
import { ReferenceTrie } from './extract-frontmatter-trie';

interface HierarchicalTrie {
  root: {
    dimensions: DimensionHierarchy;
    levels: LevelHierarchy;
    types: TypeHierarchy;
    concepts: ConceptHierarchy;
    agents: AgentHierarchy;
    functions: FunctionHierarchy;
  };
  navigation: NavigationStructure;
  metadata: {
    version: string;
    createdAt: string;
    totalNodes: number;
    totalConcepts: number;
  };
}

interface DimensionHierarchy {
  [dimension: string]: {
    documents: string[];
    agents: string[];
    functions: string[];
    concepts: string[];
    subdimensions?: string[];
  };
}

interface LevelHierarchy {
  [level: string]: {
    documents: string[];
    count: number;
    nextLevel?: string;
    previousLevel?: string;
  };
}

interface TypeHierarchy {
  [type: string]: {
    documents: string[];
    count: number;
    subtypes?: string[];
  };
}

interface ConceptHierarchy {
  [concept: string]: {
    documents: string[];
    relatedConcepts: string[];
    parentConcepts?: string[];
    childConcepts?: string[];
    wikipedia?: string;
    arxiv?: string[];
  };
}

interface AgentHierarchy {
  [agentId: string]: {
    name: string;
    dimension?: string;
    dependencies: string[];
    capabilities: string[];
  };
}

interface FunctionHierarchy {
  [functionId: string]: {
    name: string;
    dimension?: string;
    module?: string;
    signature?: string;
  };
}

interface NavigationStructure {
  byDimension: Array<{
    dimension: string;
    documents: string[];
    agents: string[];
    concepts: string[];
  }>;
  byLevel: Array<{
    level: string;
    documents: string[];
    count: number;
  }>;
  byType: Array<{
    type: string;
    documents: string[];
    count: number;
  }>;
  byConcept: Array<{
    concept: string;
    documents: string[];
    relatedConcepts: string[];
  }>;
}

/**
 * Build hierarchical reference trie from extracted frontmatter
 */
async function buildReferenceTrie(): Promise<HierarchicalTrie> {
  const triePath = path.join(__dirname, 'frontmatter-trie.json');
  
  if (!fs.existsSync(triePath)) {
    throw new Error(`Frontmatter trie not found: ${triePath}. Run extract-frontmatter-trie.ts first.`);
  }
  
  console.log('📚 Building hierarchical reference trie...');
  
  const trieData = JSON.parse(fs.readFileSync(triePath, 'utf-8'));
  const trie = trieData as any;
  
  const hierarchical: HierarchicalTrie = {
    root: {
      dimensions: {},
      levels: {},
      types: {},
      concepts: {},
      agents: {},
      functions: {}
    },
    navigation: {
      byDimension: [],
      byLevel: [],
      byType: [],
      byConcept: []
    },
    metadata: {
      version: '1.0.0',
      createdAt: new Date().toISOString(),
      totalNodes: Object.keys(trie.nodes || {}).length,
      totalConcepts: Object.keys(trie.root?.concepts || {}).length
    }
  };
  
  // Build dimension hierarchy
  console.log('   Building dimension hierarchy...');
  const dimensions = ['0D', '1D', '2D', '3D', '4D', '5D', '6D', '7D'];
  for (const dim of dimensions) {
    if (trie.root?.dimensions?.[dim]) {
      hierarchical.root.dimensions[dim] = {
        documents: trie.root.dimensions[dim].documents || [],
        agents: trie.root.dimensions[dim].agents || [],
        functions: trie.root.dimensions[dim].functions || [],
        concepts: trie.root.dimensions[dim].concepts || []
      };
      
      hierarchical.navigation.byDimension.push({
        dimension: dim,
        documents: hierarchical.root.dimensions[dim].documents,
        agents: hierarchical.root.dimensions[dim].agents,
        concepts: hierarchical.root.dimensions[dim].concepts
      });
    }
  }
  
  // Build level hierarchy with progression
  console.log('   Building level hierarchy...');
  const levelOrder = ['gateway', 'foundational', 'practical', 'applied'];
  for (let i = 0; i < levelOrder.length; i++) {
    const level = levelOrder[i];
    if (trie.root?.levels?.[level]) {
      hierarchical.root.levels[level] = {
        documents: trie.root.levels[level].documents || [],
        count: trie.root.levels[level].count || 0,
        previousLevel: i > 0 ? levelOrder[i - 1] : undefined,
        nextLevel: i < levelOrder.length - 1 ? levelOrder[i + 1] : undefined
      };
      
      hierarchical.navigation.byLevel.push({
        level,
        documents: hierarchical.root.levels[level].documents,
        count: hierarchical.root.levels[level].count
      });
    }
  }
  
  // Build type hierarchy
  console.log('   Building type hierarchy...');
  const types = ['navigation', 'concept', 'implementation', 'guide', 'specification', 'documentation'];
  for (const type of types) {
    if (trie.root?.types?.[type]) {
      hierarchical.root.types[type] = {
        documents: trie.root.types[type].documents || [],
        count: trie.root.types[type].count || 0
      };
      
      hierarchical.navigation.byType.push({
        type,
        documents: hierarchical.root.types[type].documents,
        count: hierarchical.root.types[type].count
      });
    }
  }
  
  // Build concept hierarchy with relationships
  console.log('   Building concept hierarchy...');
  if (trie.root?.concepts) {
    for (const [conceptId, concept] of Object.entries(trie.root.concepts) as any[]) {
      hierarchical.root.concepts[conceptId] = {
        documents: concept.documents || [],
        relatedConcepts: concept.relatedConcepts || [],
        parentConcepts: [],
        childConcepts: []
      };
      
      hierarchical.navigation.byConcept.push({
        concept: conceptId,
        documents: concept.documents,
        relatedConcepts: concept.relatedConcepts
      });
    }
    
    // Build parent-child relationships
    for (const [conceptId, concept] of Object.entries(hierarchical.root.concepts)) {
      for (const relatedId of concept.relatedConcepts) {
        if (hierarchical.root.concepts[relatedId]) {
          // Determine parent-child relationship based on document count
          if (concept.documents.length > hierarchical.root.concepts[relatedId].documents.length) {
            if (!hierarchical.root.concepts[relatedId].parentConcepts) {
              hierarchical.root.concepts[relatedId].parentConcepts = [];
            }
            hierarchical.root.concepts[relatedId].parentConcepts!.push(conceptId);
            if (!concept.childConcepts) {
              concept.childConcepts = [];
            }
            concept.childConcepts.push(relatedId);
          }
        }
      }
    }
  }
  
  // Load citations
  const citationsPath = path.join(__dirname, 'academic-citations.json');
  if (fs.existsSync(citationsPath)) {
    const citations = JSON.parse(fs.readFileSync(citationsPath, 'utf-8'));
    for (const [conceptId, citation] of Object.entries(citations) as any[]) {
      if (hierarchical.root.concepts[conceptId]) {
        hierarchical.root.concepts[conceptId].wikipedia = citation.wikipedia;
        hierarchical.root.concepts[conceptId].arxiv = citation.arxiv;
      }
    }
  }
  
  // Build agent hierarchy
  console.log('   Building agent hierarchy...');
  // Agents would come from knowledge base, but for now we'll extract from nodes
  for (const [nodeId, node] of Object.entries(trie.nodes || {}) as any[]) {
    if (node.tags?.some((t: string) => t.includes('agent'))) {
      hierarchical.root.agents[nodeId] = {
        name: node.title,
        dimension: node.dimension,
        dependencies: node.prerequisites || [],
        capabilities: []
      };
    }
  }
  
  // Build function hierarchy
  console.log('   Building function hierarchy...');
  // Functions would come from knowledge base
  for (const [nodeId, node] of Object.entries(trie.nodes || {}) as any[]) {
    if (node.type === 'implementation' && node.keywords?.some((k: string) => k.includes('r5rs'))) {
      hierarchical.root.functions[nodeId] = {
        name: node.title,
        dimension: node.dimension,
        module: node.frontmatter?.module,
        signature: node.frontmatter?.signature
      };
    }
  }
  
  console.log('✅ Reference trie built successfully!');
  console.log(`   Dimensions: ${Object.keys(hierarchical.root.dimensions).length}`);
  console.log(`   Levels: ${Object.keys(hierarchical.root.levels).length}`);
  console.log(`   Types: ${Object.keys(hierarchical.root.types).length}`);
  console.log(`   Concepts: ${Object.keys(hierarchical.root.concepts).length}`);
  console.log(`   Agents: ${Object.keys(hierarchical.root.agents).length}`);
  console.log(`   Functions: ${Object.keys(hierarchical.root.functions).length}`);
  
  return hierarchical;
}

/**
 * Main execution
 */
async function main() {
  try {
    const hierarchical = await buildReferenceTrie();
    
    const outputPath = path.join(__dirname, 'reference-trie.json');
    fs.writeFileSync(outputPath, JSON.stringify(hierarchical, null, 2));
    
    console.log(`\n✅ Reference trie saved to: ${outputPath}`);
    console.log(`   Size: ${(JSON.stringify(hierarchical).length / 1024).toFixed(2)} KB`);
  } catch (error) {
    console.error('❌ Error building reference trie:', error);
    process.exit(1);
  }
}

if (require.main === module) {
  main();
}

export { buildReferenceTrie, HierarchicalTrie };
