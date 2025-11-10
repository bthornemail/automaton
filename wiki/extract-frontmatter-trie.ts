#!/usr/bin/env tsx
/**
 * Extract Frontmatter Reference Trie
 * 
 * Uses Meta-Log tools to extract all frontmatter from markdown files
 * and build a complete reference trie structure for Wikipedia/arXiv documentation.
 */

import * as fs from 'fs';
import * as path from 'path';
import { ObsidianFrontmatterKnowledgeModel } from '../evolutions/obsidian-frontmatter-knowledge-model/obsidian-frontmatter-knowledge-model';
import { DocumentKnowledgeExtractor } from '../evolutions/document-knowledge-extractor/document-knowledge-extractor';

interface ReferenceTrie {
  root: {
    dimensions: Map<string, DimensionNode>;
    levels: Map<string, LevelNode>;
    types: Map<string, TypeNode>;
    tags: Map<string, TagNode>;
    concepts: Map<string, ConceptNode>;
  };
  nodes: Map<string, DocumentNode>;
  relationships: RelationshipGraph;
  citations: CitationMap;
  metadata: {
    version: string;
    createdAt: string;
    totalDocuments: number;
    totalFacts: number;
    totalRules: number;
    totalAgents: number;
    totalFunctions: number;
  };
}

interface DimensionNode {
  dimension: string; // 0D-7D
  documents: string[];
  agents: string[];
  concepts: string[];
  functions: string[];
}

interface LevelNode {
  level: string; // gateway, foundational, practical, applied
  documents: string[];
  count: number;
}

interface TypeNode {
  type: string; // navigation, concept, implementation, guide, specification
  documents: string[];
  count: number;
}

interface TagNode {
  tag: string;
  documents: string[];
  count: number;
}

interface ConceptNode {
  concept: string;
  documents: string[];
  relatedConcepts: string[];
  wikipedia?: string;
  arxiv?: string[];
}

interface DocumentNode {
  id: string;
  title: string;
  filePath: string;
  level: string;
  type: string;
  dimension?: string;
  tags: string[];
  keywords: string[];
  prerequisites: string[];
  enables: string[];
  related: string[];
  readingTime: number;
  difficulty: number;
  completeness: number;
  frontmatter: any;
}

interface RelationshipGraph {
  edges: Array<{
    from: string;
    to: string;
    type: 'prerequisite' | 'enables' | 'related' | 'depends' | 'uses';
  }>;
}

interface CitationMap {
  [concept: string]: {
    wikipedia?: string;
    arxiv?: string[];
    concepts?: string[];
  };
}

/**
 * Extract frontmatter and build reference trie
 */
async function extractFrontmatterTrie(): Promise<ReferenceTrie> {
  const workspaceRoot = '/home/main/automaton';
  const docsPath = path.join(workspaceRoot, 'docs');
  const grokPath = path.join(workspaceRoot, 'grok_files');
  
  console.log('📚 Extracting frontmatter from documentation...');
  
  // Initialize models
  const obsidianModel = new ObsidianFrontmatterKnowledgeModel(docsPath);
  const knowledgeExtractor = new DocumentKnowledgeExtractor(docsPath);
  
  // Build knowledge graphs
  console.log('   Building Obsidian knowledge graph...');
  const knowledgeGraph = await obsidianModel.buildKnowledgeGraph();
  
  console.log('   Extracting document knowledge...');
  await knowledgeExtractor.extractAll();
  const knowledgeBaseManager = knowledgeExtractor.getKnowledgeBase();
  const knowledgeBase = knowledgeBaseManager?.getKnowledgeBase();
  
  // Initialize trie structure
  const trie: ReferenceTrie = {
    root: {
      dimensions: new Map(),
      levels: new Map(),
      types: new Map(),
      tags: new Map(),
      concepts: new Map()
    },
    nodes: new Map(),
    relationships: {
      edges: []
    },
    citations: {},
    metadata: {
      version: '1.0.0',
      createdAt: new Date().toISOString(),
      totalDocuments: knowledgeGraph.nodes.size,
      totalFacts: knowledgeBase?.facts?.length || 0,
      totalRules: knowledgeBase?.rules?.length || 0,
      totalAgents: knowledgeBase?.agents?.length || 0,
      totalFunctions: knowledgeBase?.functions?.length || 0
    }
  };
  
  // Process all nodes from knowledge graph
  console.log('   Processing knowledge graph nodes...');
  for (const [id, node] of knowledgeGraph.nodes) {
    const docNode: DocumentNode = {
      id: node.id,
      title: node.title,
      filePath: node.filePath,
      level: node.level,
      type: node.type,
      tags: node.tags,
      keywords: node.keywords,
      prerequisites: node.relationships.prerequisites,
      enables: node.relationships.enables,
      related: node.relationships.related,
      readingTime: node.readingTime,
      difficulty: node.difficulty,
      completeness: node.understanding.completeness,
      frontmatter: node.frontmatter
    };
    
    // Extract dimension from tags or keywords
    const dimensionMatch = [...node.tags, ...node.keywords].find(t => /^\d+D$/.test(t));
    if (dimensionMatch) {
      docNode.dimension = dimensionMatch;
    }
    
    trie.nodes.set(id, docNode);
    
    // Add to dimension index
    if (docNode.dimension) {
      if (!trie.root.dimensions.has(docNode.dimension)) {
        trie.root.dimensions.set(docNode.dimension, {
          dimension: docNode.dimension,
          documents: [],
          agents: [],
          concepts: [],
          functions: []
        });
      }
      trie.root.dimensions.get(docNode.dimension)!.documents.push(id);
    }
    
    // Add to level index
    if (!trie.root.levels.has(node.level)) {
      trie.root.levels.set(node.level, {
        level: node.level,
        documents: [],
        count: 0
      });
    }
    trie.root.levels.get(node.level)!.documents.push(id);
    trie.root.levels.get(node.level)!.count++;
    
    // Add to type index
    if (!trie.root.types.has(node.type)) {
      trie.root.types.set(node.type, {
        type: node.type,
        documents: [],
        count: 0
      });
    }
    trie.root.types.get(node.type)!.documents.push(id);
    trie.root.types.get(node.type)!.count++;
    
    // Add to tag index
    for (const tag of node.tags) {
      if (!trie.root.tags.has(tag)) {
        trie.root.tags.set(tag, {
          tag,
          documents: [],
          count: 0
        });
      }
      trie.root.tags.get(tag)!.documents.push(id);
      trie.root.tags.get(tag)!.count++;
    }
    
    // Extract concepts from keywords
    for (const keyword of node.keywords) {
      if (!trie.root.concepts.has(keyword)) {
        trie.root.concepts.set(keyword, {
          concept: keyword,
          documents: [],
          relatedConcepts: []
        });
      }
      trie.root.concepts.get(keyword)!.documents.push(id);
    }
  }
  
  // Process relationships
  console.log('   Processing relationships...');
  if (knowledgeGraph.edges && Array.isArray(knowledgeGraph.edges)) {
    for (const edge of knowledgeGraph.edges) {
      trie.relationships.edges.push({
        from: edge.from,
        to: edge.to,
        type: edge.type
      });
    }
  }
  
  // Process agents
  console.log('   Processing agents...');
  for (const agent of knowledgeBase.agents) {
    if (agent.dimension) {
      if (!trie.root.dimensions.has(agent.dimension)) {
        trie.root.dimensions.set(agent.dimension, {
          dimension: agent.dimension,
          documents: [],
          agents: [],
          concepts: [],
          functions: []
        });
      }
      trie.root.dimensions.get(agent.dimension)!.agents.push(agent.id);
    }
  }
  
  // Process functions
  console.log('   Processing functions...');
  for (const func of knowledgeBase.functions) {
    // Try to extract dimension from function name (e.g., r5rs:church-zero -> 0D)
    const dimensionMatch = func.name.match(/church-(zero|one|succ|add|mult|exp)/);
    if (dimensionMatch) {
      const dimMap: Record<string, string> = {
        'zero': '0D',
        'one': '0D',
        'succ': '1D',
        'add': '3D',
        'mult': '3D',
        'exp': '3D'
      };
      const dim = dimMap[dimensionMatch[1]];
      if (dim && trie.root.dimensions.has(dim)) {
        trie.root.dimensions.get(dim)!.functions.push(func.id);
      }
    }
  }
  
  // Build concept relationships
  console.log('   Building concept relationships...');
  for (const [conceptId, concept] of trie.root.concepts) {
    const relatedConcepts = new Set<string>();
    
    // Find related concepts through document relationships
    for (const docId of concept.documents) {
      const doc = trie.nodes.get(docId);
      if (doc) {
        for (const relatedDocId of [...doc.related, ...doc.enables, ...doc.prerequisites]) {
          const relatedDoc = trie.nodes.get(relatedDocId);
          if (relatedDoc) {
            for (const keyword of relatedDoc.keywords) {
              if (keyword !== conceptId && trie.root.concepts.has(keyword)) {
                relatedConcepts.add(keyword);
              }
            }
          }
        }
      }
    }
    
    concept.relatedConcepts = Array.from(relatedConcepts);
  }
  
  console.log('✅ Frontmatter extraction complete!');
  console.log(`   Documents: ${trie.metadata.totalDocuments}`);
  console.log(`   Dimensions: ${trie.root.dimensions.size}`);
  console.log(`   Concepts: ${trie.root.concepts.size}`);
  console.log(`   Agents: ${trie.metadata.totalAgents}`);
  console.log(`   Functions: ${trie.metadata.totalFunctions}`);
  
  return trie;
}

/**
 * Convert Map to plain object for JSON serialization
 */
function serializeTrie(trie: ReferenceTrie): any {
  return {
    root: {
      dimensions: Object.fromEntries(
        Array.from(trie.root.dimensions.entries()).map(([k, v]) => [k, {
          ...v,
          documents: v.documents,
          agents: v.agents,
          concepts: v.concepts,
          functions: v.functions
        }])
      ),
      levels: Object.fromEntries(
        Array.from(trie.root.levels.entries()).map(([k, v]) => [k, v])
      ),
      types: Object.fromEntries(
        Array.from(trie.root.types.entries()).map(([k, v]) => [k, v])
      ),
      tags: Object.fromEntries(
        Array.from(trie.root.tags.entries()).map(([k, v]) => [k, v])
      ),
      concepts: Object.fromEntries(
        Array.from(trie.root.concepts.entries()).map(([k, v]) => [k, v])
      )
    },
    nodes: Object.fromEntries(
      Array.from(trie.nodes.entries()).map(([k, v]) => [k, v])
    ),
    relationships: trie.relationships,
    citations: trie.citations,
    metadata: trie.metadata
  };
}

/**
 * Main execution
 */
async function main() {
  try {
    const trie = await extractFrontmatterTrie();
    const serialized = serializeTrie(trie);
    
    const outputPath = path.join(__dirname, 'frontmatter-trie.json');
    fs.writeFileSync(outputPath, JSON.stringify(serialized, null, 2));
    
    console.log(`\n✅ Frontmatter trie saved to: ${outputPath}`);
    console.log(`   Size: ${(JSON.stringify(serialized).length / 1024).toFixed(2)} KB`);
  } catch (error) {
    console.error('❌ Error extracting frontmatter:', error);
    process.exit(1);
  }
}

if (require.main === module) {
  main();
}

export { extractFrontmatterTrie, serializeTrie, ReferenceTrie };
