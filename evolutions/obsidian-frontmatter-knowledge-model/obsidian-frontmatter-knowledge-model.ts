#!/usr/bin/env tsx
/**
 * Obsidian Frontmatter Knowledge Model
 * 
 * Evaluates document frontmatter to build knowledge graphs and understanding
 * Based on the Meta-Log Plugin's frontmatter structure
 * 
 * This model analyzes:
 * - Document structure (id, title, level, type)
 * - Relationships (prerequisites, enables, related)
 * - Metadata (tags, keywords, readingTime, difficulty)
 * - Blackboard state (status, assignedAgent, dependencies)
 * 
 * Reference: docs/06-Meta-Log-Adapters/02-Meta-Log-Plugin/
 */

import * as fs from 'fs';
import * as path from 'path';
import * as yaml from 'js-yaml';

/**
 * Frontmatter structure based on Obsidian Meta-Log Plugin
 */
interface DocumentFrontmatter {
  id?: string;
  title?: string;
  level?: 'gateway' | 'foundational' | 'practical' | 'applied';
  type?: 'navigation' | 'concept' | 'implementation' | 'guide' | 'specification' | 'documentation';
  tags?: string[];
  keywords?: string[];
  prerequisites?: string[];
  enables?: string[];
  related?: string[];
  readingTime?: number;
  difficulty?: number;
  blackboard?: {
    status?: 'active' | 'processing' | 'completed';
    assignedAgent?: string | null;
    lastUpdate?: string | null;
    dependencies?: string[];
    watchers?: string[];
    r5rsEngine?: string;
    selfBuilding?: {
      enabled?: boolean;
      source?: string;
      pattern?: string;
      regeneration?: {
        function?: string;
        args?: any[];
        context?: any;
      };
    };
    [key: string]: any;
  };
  bipartite?: {
    partition?: 'topology' | 'system';
    dimension?: '0D' | '1D' | '2D' | '3D' | '4D' | '5D' | '6D' | '7D';
    bqf?: {
      form: string;
      coefficients?: number[];
      signature?: string;
      variables?: string[];
      polynomial?: string;
      symbol?: string;
      procedure?: string;
    };
    polynomial?: {
      monad?: number[];
      functor?: number[];
      perceptron?: number[];
    };
    relationships?: {
      topology?: string | null;
      system?: string | null;
    };
  };
  [key: string]: any;
}

/**
 * Knowledge graph node representing a document
 */
interface KnowledgeNode {
  id: string;
  title: string;
  level: string;
  type: string;
  tags: string[];
  keywords: string[];
  readingTime: number;
  difficulty: number;
  filePath: string;
  frontmatter: DocumentFrontmatter;
  relationships: {
    prerequisites: string[];
    enables: string[];
    related: string[];
  };
  blackboard: DocumentFrontmatter['blackboard'];
  bipartite?: DocumentFrontmatter['bipartite'];
  understanding: {
    completeness: number; // 0-1 score
    missingFields: string[];
    relationshipIntegrity: {
      brokenPrerequisites: string[];
      brokenEnables: string[];
      brokenRelated: string[];
    };
    metadataQuality: number; // 0-1 score
  };
}

/**
 * Knowledge graph representing all documents
 */
interface KnowledgeGraph {
  nodes: Map<string, KnowledgeNode>;
  edges: Array<{
    from: string;
    to: string;
    type: 'prerequisite' | 'enables' | 'related';
  }>;
  statistics: {
    totalDocuments: number;
    byLevel: Record<string, number>;
    byType: Record<string, number>;
    completeness: {
      average: number;
      distribution: Array<{ range: string; count: number }>;
    };
    relationshipIntegrity: {
      brokenLinks: number;
      totalLinks: number;
      integrityScore: number;
    };
  };
}

/**
 * Obsidian Frontmatter Knowledge Model
 * 
 * Analyzes document frontmatter to build knowledge graphs
 */
export class ObsidianFrontmatterKnowledgeModel {
  private vaultPath: string;
  private knowledgeGraph: KnowledgeGraph;

  constructor(vaultPath: string) {
    this.vaultPath = vaultPath;
    this.knowledgeGraph = {
      nodes: new Map(),
      edges: [],
      statistics: {
        totalDocuments: 0,
        byLevel: {},
        byType: {},
        completeness: {
          average: 0,
          distribution: []
        },
        relationshipIntegrity: {
          brokenLinks: 0,
          totalLinks: 0,
          integrityScore: 0
        }
      }
    };
  }

  /**
   * Parse frontmatter from markdown file
   */
  private parseFrontmatter(filePath: string): { frontmatter: DocumentFrontmatter | null; body: string } | null {
    try {
      const content = fs.readFileSync(filePath, 'utf-8');
      const frontmatterMatch = content.match(/^---\n([\s\S]*?)\n---\n([\s\S]*)$/);
      
      if (!frontmatterMatch) {
        return null;
      }

      const frontmatterYaml = frontmatterMatch[1];
      const body = frontmatterMatch[2];

      if (!frontmatterYaml) {
        return { frontmatter: null, body: content };
      }
      const frontmatter = yaml.load(frontmatterYaml) as DocumentFrontmatter;
      const bodyContent = body || '';
      
      return { frontmatter, body: bodyContent };
    } catch (error) {
      console.warn(`Failed to parse frontmatter from ${filePath}:`, error);
      return null;
    }
  }

  /**
   * Evaluate document understanding based on frontmatter
   */
  private evaluateUnderstanding(
    frontmatter: DocumentFrontmatter,
    filePath: string,
    allIds: Set<string>
  ): KnowledgeNode['understanding'] {
    const requiredFields = ['id', 'title', 'level', 'type'];
    const recommendedFields = ['tags', 'keywords', 'prerequisites', 'enables', 'related', 'readingTime', 'difficulty'];
    
    const missingFields: string[] = [];
    let completenessScore = 0;
    let metadataQualityScore = 0;

    // Check required fields
    for (const field of requiredFields) {
      if (!frontmatter[field]) {
        missingFields.push(field);
      } else {
        completenessScore += 0.25; // 25% per required field
      }
    }

    // Check recommended fields
    let recommendedCount = 0;
    for (const field of recommendedFields) {
      if (frontmatter[field] !== undefined && frontmatter[field] !== null) {
        recommendedCount++;
      }
    }
    metadataQualityScore = recommendedCount / recommendedFields.length;

    // Check relationship integrity
    const brokenPrerequisites: string[] = [];
    const brokenEnables: string[] = [];
    const brokenRelated: string[] = [];

    if (frontmatter.prerequisites) {
      for (const prereq of frontmatter.prerequisites) {
        if (!allIds.has(prereq)) {
          brokenPrerequisites.push(prereq);
        }
      }
    }

    if (frontmatter.enables) {
      for (const enable of frontmatter.enables) {
        if (!allIds.has(enable)) {
          brokenEnables.push(enable);
        }
      }
    }

    if (frontmatter.related) {
      for (const related of frontmatter.related) {
        if (!allIds.has(related)) {
          brokenRelated.push(related);
        }
      }
    }

    // Calculate overall completeness (required fields + metadata quality)
    completenessScore = completenessScore * 0.6 + metadataQualityScore * 0.4;

    return {
      completeness: Math.min(1, completenessScore),
      missingFields,
      relationshipIntegrity: {
        brokenPrerequisites,
        brokenEnables,
        brokenRelated
      },
      metadataQuality: metadataQualityScore
    };
  }

  /**
   * Build knowledge graph from vault documents
   */
  public async buildKnowledgeGraph(directory: string = ''): Promise<KnowledgeGraph> {
    const searchPath = path.join(this.vaultPath, directory);
    const allIds = new Set<string>();
    const files: string[] = [];

    // First pass: collect all IDs
    this.collectFiles(searchPath, files);
    
    for (const file of files) {
      const parsed = this.parseFrontmatter(file);
      if (parsed?.frontmatter?.id) {
        allIds.add(parsed.frontmatter.id);
      }
    }

    // Second pass: build knowledge nodes
    for (const file of files) {
      const parsed = this.parseFrontmatter(file);
      if (!parsed || !parsed.frontmatter || !parsed.frontmatter.id) {
        continue;
      }

      const understanding = this.evaluateUnderstanding(parsed.frontmatter, file, allIds);
      
      const node: KnowledgeNode = {
        id: parsed.frontmatter.id,
        title: parsed.frontmatter.title || path.basename(file, '.md'),
        level: parsed.frontmatter.level || 'unknown',
        type: parsed.frontmatter.type || 'unknown',
        tags: parsed.frontmatter.tags || [],
        keywords: parsed.frontmatter.keywords || [],
        readingTime: parsed.frontmatter.readingTime || 0,
        difficulty: parsed.frontmatter.difficulty || 0,
        filePath: file,
        frontmatter: parsed.frontmatter,
        relationships: {
          prerequisites: parsed.frontmatter.prerequisites || [],
          enables: parsed.frontmatter.enables || [],
          related: parsed.frontmatter.related || []
        },
        blackboard: parsed.frontmatter.blackboard,
        bipartite: parsed.frontmatter.bipartite,
        understanding
      };

      this.knowledgeGraph.nodes.set(node.id, node);

      // Add edges
      if (parsed.frontmatter.prerequisites) {
        for (const prereq of parsed.frontmatter.prerequisites) {
          this.knowledgeGraph.edges.push({
            from: prereq,
            to: node.id,
            type: 'prerequisite'
          });
        }
      }

      if (parsed.frontmatter.enables) {
        for (const enable of parsed.frontmatter.enables) {
          this.knowledgeGraph.edges.push({
            from: node.id,
            to: enable,
            type: 'enables'
          });
        }
      }

      if (parsed.frontmatter.related) {
        for (const related of parsed.frontmatter.related) {
          this.knowledgeGraph.edges.push({
            from: node.id,
            to: related,
            type: 'related'
          });
        }
      }
    }

    // Calculate statistics
    this.calculateStatistics();

    return this.knowledgeGraph;
  }

  /**
   * Recursively collect markdown files
   */
  private collectFiles(dir: string, files: string[]): void {
    try {
      const entries = fs.readdirSync(dir, { withFileTypes: true });
      
      for (const entry of entries) {
        const fullPath = path.join(dir, entry.name);
        
        // Skip hidden directories and node_modules
        if (entry.name.startsWith('.') || entry.name === 'node_modules') {
          continue;
        }

        if (entry.isDirectory()) {
          this.collectFiles(fullPath, files);
        } else if (entry.isFile() && entry.name.endsWith('.md')) {
          files.push(fullPath);
        }
      }
    } catch (error) {
      console.warn(`Failed to read directory ${dir}:`, error);
    }
  }

  /**
   * Calculate knowledge graph statistics
   */
  private calculateStatistics(): void {
    const nodes = Array.from(this.knowledgeGraph.nodes.values());
    
    this.knowledgeGraph.statistics.totalDocuments = nodes.length;

    // Count by level and type
    for (const node of nodes) {
      this.knowledgeGraph.statistics.byLevel[node.level] = 
        (this.knowledgeGraph.statistics.byLevel[node.level] || 0) + 1;
      this.knowledgeGraph.statistics.byType[node.type] = 
        (this.knowledgeGraph.statistics.byType[node.type] || 0) + 1;
    }

    // Calculate completeness distribution
    const completenessScores = nodes.map(n => n.understanding.completeness);
    const average = completenessScores.reduce((a, b) => a + b, 0) / completenessScores.length;
    this.knowledgeGraph.statistics.completeness.average = average;

    const ranges = [
      { range: '0.0-0.2', min: 0.0, max: 0.2 },
      { range: '0.2-0.4', min: 0.2, max: 0.4 },
      { range: '0.4-0.6', min: 0.4, max: 0.6 },
      { range: '0.6-0.8', min: 0.6, max: 0.8 },
      { range: '0.8-1.0', min: 0.8, max: 1.0 }
    ];

    for (const range of ranges) {
      const count = completenessScores.filter(s => s >= range.min && s < range.max).length;
      this.knowledgeGraph.statistics.completeness.distribution.push({
        range: range.range,
        count
      });
    }

    // Calculate relationship integrity
    let brokenLinks = 0;
    let totalLinks = 0;

    for (const edge of this.knowledgeGraph.edges) {
      totalLinks++;
      if (!this.knowledgeGraph.nodes.has(edge.from) || !this.knowledgeGraph.nodes.has(edge.to)) {
        brokenLinks++;
      }
    }

    this.knowledgeGraph.statistics.relationshipIntegrity.brokenLinks = brokenLinks;
    this.knowledgeGraph.statistics.relationshipIntegrity.totalLinks = totalLinks;
    this.knowledgeGraph.statistics.relationshipIntegrity.integrityScore = 
      totalLinks > 0 ? 1 - (brokenLinks / totalLinks) : 1;
  }

  /**
   * Generate knowledge report
   */
  public generateReport(): string {
    const stats = this.knowledgeGraph.statistics;
    const nodes = Array.from(this.knowledgeGraph.nodes.values());

    let report = '# Obsidian Frontmatter Knowledge Model Report\n\n';
    report += `Generated: ${new Date().toISOString()}\n\n`;

    report += '## Overview\n\n';
    report += `- **Total Documents**: ${stats.totalDocuments}\n`;
    report += `- **Average Completeness**: ${(stats.completeness.average * 100).toFixed(1)}%\n`;
    report += `- **Relationship Integrity**: ${(stats.relationshipIntegrity.integrityScore * 100).toFixed(1)}%\n`;
    report += `- **Broken Links**: ${stats.relationshipIntegrity.brokenLinks} / ${stats.relationshipIntegrity.totalLinks}\n\n`;

    report += '## Documents by Level\n\n';
    for (const [level, count] of Object.entries(stats.byLevel)) {
      report += `- **${level}**: ${count}\n`;
    }
    report += '\n';

    report += '## Documents by Type\n\n';
    for (const [type, count] of Object.entries(stats.byType)) {
      report += `- **${type}**: ${count}\n`;
    }
    report += '\n';

    report += '## Completeness Distribution\n\n';
    for (const dist of stats.completeness.distribution) {
      const percentage = stats.totalDocuments > 0 
        ? (dist.count / stats.totalDocuments * 100).toFixed(1)
        : '0.0';
      report += `- **${dist.range}**: ${dist.count} (${percentage}%)\n`;
    }
    report += '\n';

    report += '## Documents Needing Attention\n\n';
    const incompleteDocs = nodes
      .filter(n => n.understanding.completeness < 0.6)
      .sort((a, b) => a.understanding.completeness - b.understanding.completeness)
      .slice(0, 10);

    for (const doc of incompleteDocs) {
      report += `### ${doc.title} (${doc.id})\n`;
      report += `- **Completeness**: ${(doc.understanding.completeness * 100).toFixed(1)}%\n`;
      report += `- **Missing Fields**: ${doc.understanding.missingFields.join(', ') || 'None'}\n`;
      report += `- **Broken Prerequisites**: ${doc.understanding.relationshipIntegrity.brokenPrerequisites.join(', ') || 'None'}\n`;
      report += `- **Broken Enables**: ${doc.understanding.relationshipIntegrity.brokenEnables.join(', ') || 'None'}\n`;
      report += `- **File**: ${doc.filePath}\n\n`;
    }

    report += '## Relationship Graph\n\n';
    report += `Total Relationships: ${stats.relationshipIntegrity.totalLinks}\n`;
    report += `- Prerequisites: ${this.knowledgeGraph.edges.filter(e => e.type === 'prerequisite').length}\n`;
    report += `- Enables: ${this.knowledgeGraph.edges.filter(e => e.type === 'enables').length}\n`;
    report += `- Related: ${this.knowledgeGraph.edges.filter(e => e.type === 'related').length}\n`;

    // Add bipartite graph statistics if available
    const bipartiteStats = this.getBipartiteStatistics();
    if (bipartiteStats.totalNodes > 0) {
      report += '\n## Bipartite Graph Statistics\n\n';
      report += `- **Total Bipartite Nodes**: ${bipartiteStats.totalNodes}\n`;
      report += `- **Topology Nodes**: ${bipartiteStats.topologyNodes}\n`;
      report += `- **System Nodes**: ${bipartiteStats.systemNodes}\n`;
      report += `- **Horizontal Edges**: ${bipartiteStats.horizontalEdges}\n`;
      report += `- **Vertical Edges**: ${bipartiteStats.verticalEdges}\n`;
      report += `- **Dimensions Covered**: ${bipartiteStats.dimensions.join(', ')}\n`;
    }

    return report;
  }

  /**
   * Build bipartite graph structure from knowledge graph
   */
  public buildBipartiteGraph(): {
    topologyNodes: KnowledgeNode[];
    systemNodes: KnowledgeNode[];
    horizontalEdges: Array<{ from: string; to: string; mapping?: string }>;
    verticalEdges: Array<{ from: string; to: string; progression?: string }>;
  } {
    const topologyNodes: KnowledgeNode[] = [];
    const systemNodes: KnowledgeNode[] = [];
    const horizontalEdges: Array<{ from: string; to: string; mapping?: string }> = [];
    const verticalEdges: Array<{ from: string; to: string; progression?: string }> = [];

    // Separate nodes by partition
    for (const node of this.knowledgeGraph.nodes.values()) {
      if (node.bipartite?.partition === 'topology') {
        topologyNodes.push(node);
      } else if (node.bipartite?.partition === 'system') {
        systemNodes.push(node);
      }
    }

    // Build horizontal edges (topology ↔ system)
    for (const topologyNode of topologyNodes) {
      if (topologyNode.bipartite?.relationships?.system) {
        const systemId = topologyNode.bipartite.relationships.system;
        const systemNode = systemNodes.find(n => n.id === systemId);
        if (systemNode) {
          horizontalEdges.push({
            from: topologyNode.id,
            to: systemId,
            mapping: `h:${topologyNode.id}→${systemId}`
          });
        }
      }
    }

    // Build vertical edges (dimensional progression)
    // Sort nodes by dimension for progression detection
    const allBipartiteNodes = [...topologyNodes, ...systemNodes].sort((a, b) => {
      const dimA = this.getDimensionNumber(a.bipartite?.dimension);
      const dimB = this.getDimensionNumber(b.bipartite?.dimension);
      return dimA - dimB;
    });

    for (let i = 0; i < allBipartiteNodes.length - 1; i++) {
      const current = allBipartiteNodes[i];
      const next = allBipartiteNodes[i + 1];
      
      if (current.bipartite?.dimension && next.bipartite?.dimension) {
        const currentDim = this.getDimensionNumber(current.bipartite.dimension);
        const nextDim = this.getDimensionNumber(next.bipartite.dimension);
        
        // Check if they're in the same partition and consecutive dimensions
        if (current.bipartite.partition === next.bipartite.partition && 
            nextDim === currentDim + 1) {
          verticalEdges.push({
            from: current.id,
            to: next.id,
            progression: `${current.bipartite.dimension} → ${next.bipartite.dimension}`
          });
        }
      }
    }

    return {
      topologyNodes,
      systemNodes,
      horizontalEdges,
      verticalEdges
    };
  }

  /**
   * Get bipartite statistics
   */
  public getBipartiteStatistics(): {
    totalNodes: number;
    topologyNodes: number;
    systemNodes: number;
    horizontalEdges: number;
    verticalEdges: number;
    dimensions: string[];
  } {
    const bipartiteGraph = this.buildBipartiteGraph();
    const dimensions = new Set<string>();
    
    for (const node of this.knowledgeGraph.nodes.values()) {
      if (node.bipartite?.dimension) {
        dimensions.add(node.bipartite.dimension);
      }
    }

    return {
      totalNodes: bipartiteGraph.topologyNodes.length + bipartiteGraph.systemNodes.length,
      topologyNodes: bipartiteGraph.topologyNodes.length,
      systemNodes: bipartiteGraph.systemNodes.length,
      horizontalEdges: bipartiteGraph.horizontalEdges.length,
      verticalEdges: bipartiteGraph.verticalEdges.length,
      dimensions: Array.from(dimensions).sort()
    };
  }

  /**
   * Helper to convert dimension string to number
   */
  private getDimensionNumber(dimension?: string): number {
    if (!dimension) return -1;
    const match = dimension.match(/^(\d)D$/);
    return match ? parseInt(match[1]) : -1;
  }

  /**
   * Validate BQF forms in bipartite metadata
   */
  public validateBQFForms(): Array<{
    nodeId: string;
    errors: string[];
  }> {
    const errors: Array<{ nodeId: string; errors: string[] }> = [];

    for (const node of this.knowledgeGraph.nodes.values()) {
      if (!node.bipartite?.bqf) continue;

      const bqf = node.bipartite.bqf;
      const nodeErrors: string[] = [];

      // Validate form
      if (!bqf.form || typeof bqf.form !== 'string') {
        nodeErrors.push('BQF form is required and must be a string');
      }

      // Validate coefficients
      if (bqf.coefficients && !Array.isArray(bqf.coefficients)) {
        nodeErrors.push('BQF coefficients must be an array');
      }

      // Validate signature
      if (bqf.signature) {
        const validSignatures = ['euclidean', 'lorentz', 'minkowski', 'riemannian'];
        if (!validSignatures.includes(bqf.signature)) {
          nodeErrors.push(`BQF signature must be one of: ${validSignatures.join(', ')}`);
        }
      }

      // Validate variables match dimension
      if (bqf.variables && node.bipartite.dimension) {
        const expectedDim = this.getDimensionNumber(node.bipartite.dimension);
        if (expectedDim >= 0 && bqf.variables.length !== expectedDim) {
          nodeErrors.push(`BQF variables count (${bqf.variables.length}) does not match dimension ${node.bipartite.dimension}`);
        }
      }

      // Validate polynomial arrays
      if (node.bipartite.polynomial) {
        const poly = node.bipartite.polynomial;
        if (poly.monad && poly.monad.length !== 8) {
          nodeErrors.push(`Polynomial monad must have exactly 8 components, got ${poly.monad.length}`);
        }
        if (poly.functor && poly.functor.length !== 8) {
          nodeErrors.push(`Polynomial functor must have exactly 8 components, got ${poly.functor.length}`);
        }
        if (poly.perceptron && poly.perceptron.length !== 8) {
          nodeErrors.push(`Polynomial perceptron must have exactly 8 components, got ${poly.perceptron.length}`);
        }
      }

      if (nodeErrors.length > 0) {
        errors.push({ nodeId: node.id, errors: nodeErrors });
      }
    }

    return errors;
  }

  /**
   * Export knowledge graph as JSON
   */
  public exportJSON(): string {
    const exportData = {
      nodes: Array.from(this.knowledgeGraph.nodes.values()),
      edges: this.knowledgeGraph.edges,
      statistics: this.knowledgeGraph.statistics,
      generated: new Date().toISOString()
    };

    return JSON.stringify(exportData, null, 2);
  }
}

// CLI usage
if (require.main === module) {
  const vaultPath = process.argv[2] || process.cwd();
  const model = new ObsidianFrontmatterKnowledgeModel(vaultPath);
  
  console.log(`🔍 Building knowledge graph from: ${vaultPath}\n`);
  
  model.buildKnowledgeGraph().then(graph => {
    console.log('✅ Knowledge graph built successfully!\n');
    console.log(model.generateReport());
    
    // Export JSON
    const jsonPath = path.join(vaultPath, 'knowledge-graph.json');
    fs.writeFileSync(jsonPath, model.exportJSON());
    console.log(`\n💾 Knowledge graph exported to: ${jsonPath}`);
  }).catch(error => {
    console.error('❌ Error building knowledge graph:', error);
    process.exit(1);
  });
}
