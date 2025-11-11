#!/usr/bin/env tsx
/**
 * Generate Reference Files from Frontmatter and Academic Citations
 * 
 * This script:
 * 1. Parses frontmatter-trie.json to extract concept relationships
 * 2. Maps concepts to academic-citations.json entries
 * 3. Generates concept-specific reference files
 * 4. Generates dimension-specific reference files
 * 5. Generates paradigm-specific reference files
 * 6. Expands references/index.md
 */

import * as fs from 'fs';
import * as path from 'path';

interface AcademicCitation {
  wikipedia?: string;
  arxiv?: string[];
  concepts?: string[];
}

interface AcademicCitations {
  [key: string]: AcademicCitation;
}

interface FrontmatterTrie {
  root?: {
    dimensions?: {
      [dim: string]: {
        dimension: string;
        documents: string[];
        agents: string[];
        concepts: string[];
        functions: string[];
      };
    };
    [key: string]: any;
  };
  [key: string]: any;
}

// Load data files
const wikiDir = path.dirname(__filename);
const citationsPath = path.join(wikiDir, 'academic-citations.json');
const frontmatterPath = path.join(wikiDir, 'frontmatter-trie.json');

const citations: AcademicCitations = JSON.parse(fs.readFileSync(citationsPath, 'utf-8'));
const frontmatter: FrontmatterTrie = JSON.parse(fs.readFileSync(frontmatterPath, 'utf-8'));

// Concept to dimension mapping
const conceptToDimension: { [key: string]: string } = {
  'church-encoding': '0D',
  'lambda-calculus': '0D',
  'y-combinator': '0D',
  'r5rs': '0D',
  'automaton': '0D',
  'self-reference': '0D',
  'metacircular-evaluator': '0D',
  'dimensional-progression': '1D',
  'prolog': '2D',
  'datalog': '2D',
  'rdf': '3D',
  'sparql': '3D',
  'shacl': '3D',
  'knowledge-graph': '3D',
  'multi-agent-system': '4D',
  'blackboard-architecture': '5D',
  'computational-topology': '0D',
  'canvas-format': '2D',
  'provenance': '3D',
  'ci-cd': '4D',
};

// Concept to paradigm mapping
const conceptToParadigm: { [key: string]: string[] } = {
  'church-encoding': ['functional-programming'],
  'lambda-calculus': ['functional-programming'],
  'y-combinator': ['functional-programming'],
  'r5rs': ['functional-programming'],
  'prolog': ['logic-programming'],
  'datalog': ['logic-programming'],
  'rdf': ['semantic-web'],
  'sparql': ['semantic-web'],
  'shacl': ['semantic-web'],
  'knowledge-graph': ['semantic-web'],
  'multi-agent-system': ['multi-agent-systems'],
  'blackboard-architecture': ['multi-agent-systems'],
};

/**
 * Generate concept-specific reference file
 */
function generateConceptReference(conceptId: string, citation: AcademicCitation): string {
  const dimension = conceptToDimension[conceptId] || 'N/A';
  const paradigms = conceptToParadigm[conceptId] || [];
  
  let content = `# ${conceptId.replace(/-/g, ' ').replace(/\b\w/g, l => l.toUpperCase())}: Academic References\n\n`;
  content += `**Academic resources for understanding ${conceptId} in the Computational Topology Canvas**\n\n`;
  content += `---\n\n`;
  
  content += `## Overview\n\n`;
  content += `This document provides academic references for **${conceptId}**, a core concept in CTC.\n\n`;
  content += `- **Dimension**: ${dimension}\n`;
  if (paradigms.length > 0) {
    content += `- **Paradigms**: ${paradigms.join(', ')}\n`;
  }
  content += `- **Related Concepts**: ${citation.concepts?.join(', ') || 'N/A'}\n\n`;
  
  content += `---\n\n`;
  
  if (citation.wikipedia) {
    content += `## Wikipedia Articles\n\n`;
    content += `### Primary Article\n\n`;
    content += `- **[${conceptId.replace(/-/g, ' ')}](${citation.wikipedia})**\n`;
    content += `  - Comprehensive overview of ${conceptId}\n`;
    content += `  - Historical context and theoretical foundations\n`;
    content += `  - Key concepts and definitions\n\n`;
  }
  
  if (citation.arxiv && citation.arxiv.length > 0) {
    content += `## arXiv Papers\n\n`;
    content += `### Search Queries\n\n`;
    citation.arxiv.forEach((query, idx) => {
      const searchTerm = query.includes('query=') 
        ? decodeURIComponent(query.split('query=')[1]?.replace(/\+/g, ' ') || '')
        : 'Search arXiv';
      content += `${idx + 1}. **[${searchTerm}](${query})**\n`;
      content += `   - Recent research papers on ${conceptId}\n`;
      content += `   - Implementation techniques and algorithms\n\n`;
    });
  }
  
  content += `## How This Relates to CTC\n\n`;
  content += `In the Computational Topology Canvas:\n\n`;
  content += `- **Topology**: ${conceptId} provides mathematical foundations\n`;
  content += `- **System**: ${conceptId} enables computational implementations\n`;
  content += `- **Dimension**: ${dimension} - ${getDimensionDescription(dimension)}\n\n`;
  
  if (citation.concepts && citation.concepts.length > 0) {
    content += `## Related Concepts\n\n`;
    citation.concepts.forEach(concept => {
      const conceptIdLower = concept.toLowerCase().replace(/\s+/g, '-');
      if (citations[conceptIdLower]) {
        content += `- **[${concept}](${conceptIdLower}.md)**\n`;
      } else {
        content += `- **${concept}**\n`;
      }
    });
    content += `\n`;
  }
  
  content += `## Prerequisites\n\n`;
  content += `Before understanding ${conceptId}, you should understand:\n\n`;
  if (dimension !== '0D' && dimension !== 'N/A') {
    const prevDim = getPreviousDimension(dimension);
    content += `- **${prevDim} concepts**: Read dimension-specific references\n`;
  }
  content += `- **Foundational concepts**: See \`references/by-dimension/0D-references.md\`\n\n`;
  
  content += `## Enables\n\n`;
  content += `Understanding ${conceptId} enables:\n\n`;
  if (dimension !== '7D' && dimension !== 'N/A') {
    const nextDim = getNextDimension(dimension);
    content += `- **${nextDim} concepts**: See \`references/by-dimension/${nextDim}-references.md\`\n`;
  }
  content += `- **Advanced topics**: See related concept references\n\n`;
  
  content += `---\n\n`;
  content += `**Last Updated**: ${new Date().toISOString().split('T')[0]}\n`;
  content += `**Version**: 1.0.0\n`;
  
  return content;
}

/**
 * Generate dimension-specific reference file
 */
function generateDimensionReference(dimension: string): string {
  const concepts = Object.keys(citations).filter(c => conceptToDimension[c] === dimension);
  const topologyConcepts = concepts.filter(c => isTopologyConcept(c));
  const systemConcepts = concepts.filter(c => !isTopologyConcept(c));
  
  let content = `# ${dimension} References: Academic Resources\n\n`;
  content += `**Academic references for ${dimension} dimension concepts**\n\n`;
  content += `---\n\n`;
  
  content += `## Overview\n\n`;
  content += `This document provides academic references for all concepts in the **${dimension}** dimension.\n\n`;
  content += `- **Dimension**: ${dimension} - ${getDimensionDescription(dimension)}\n`;
  content += `- **Topology Concepts**: ${topologyConcepts.length}\n`;
  content += `- **System Concepts**: ${systemConcepts.length}\n\n`;
  
  content += `---\n\n`;
  
  content += `## Topology References (Mathematical Foundations)\n\n`;
  if (topologyConcepts.length > 0) {
    topologyConcepts.forEach(concept => {
      const citation = citations[concept];
      content += `### ${concept.replace(/-/g, ' ').replace(/\b\w/g, l => l.toUpperCase())}\n\n`;
      if (citation.wikipedia) {
        content += `- **Wikipedia**: [${concept}](${citation.wikipedia})\n`;
      }
      if (citation.arxiv && citation.arxiv.length > 0) {
        content += `- **arXiv**: [Search](${citation.arxiv[0]})\n`;
      }
      content += `- **Details**: See [by-concept/${concept}.md](../by-concept/${concept}.md)\n\n`;
    });
  } else {
    content += `No topology-specific concepts in ${dimension}.\n\n`;
  }
  
  content += `## System References (Computational Implementations)\n\n`;
  if (systemConcepts.length > 0) {
    systemConcepts.forEach(concept => {
      const citation = citations[concept];
      content += `### ${concept.replace(/-/g, ' ').replace(/\b\w/g, l => l.toUpperCase())}\n\n`;
      if (citation.wikipedia) {
        content += `- **Wikipedia**: [${concept}](${citation.wikipedia})\n`;
      }
      if (citation.arxiv && citation.arxiv.length > 0) {
        content += `- **arXiv**: [Search](${citation.arxiv[0]})\n`;
      }
      content += `- **Details**: See [by-concept/${concept}.md](../by-concept/${concept}.md)\n\n`;
    });
  } else {
    content += `No system-specific concepts in ${dimension}.\n\n`;
  }
  
  content += `## Horizontal Mappings\n\n`;
  content += `Topology concepts map to system implementations:\n\n`;
  content += `- See \`../../horizontal/integration-guides/topology-to-system-mappings.md\`\n\n`;
  
  content += `## Vertical Connections\n\n`;
  if (dimension !== '0D') {
    const prevDim = getPreviousDimension(dimension);
    content += `- **Previous**: [${prevDim} References](./${prevDim}-references.md)\n`;
  }
  if (dimension !== '7D') {
    const nextDim = getNextDimension(dimension);
    content += `- **Next**: [${nextDim} References](./${nextDim}-references.md)\n`;
  }
  content += `- **Progression**: See \`../../vertical/progression-guides/\`\n\n`;
  
  content += `---\n\n`;
  content += `**Last Updated**: ${new Date().toISOString().split('T')[0]}\n`;
  content += `**Version**: 1.0.0\n`;
  
  return content;
}

/**
 * Generate paradigm-specific reference file
 */
function generateParadigmReference(paradigm: string): string {
  const concepts = Object.keys(citations).filter(c => 
    conceptToParadigm[c]?.includes(paradigm)
  );
  
  let content = `# ${paradigm.replace(/-/g, ' ').replace(/\b\w/g, l => l.toUpperCase())}: Academic References\n\n`;
  content += `**Academic resources for ${paradigm} paradigm in CTC**\n\n`;
  content += `---\n\n`;
  
  content += `## Overview\n\n`;
  content += `This document provides academic references for the **${paradigm}** programming paradigm.\n\n`;
  content += `- **Concepts**: ${concepts.length}\n`;
  content += `- **Integration**: See \`../../horizontal/integration-guides/paradigm-integration.md\`\n\n`;
  
  content += `---\n\n`;
  
  content += `## Core Concepts\n\n`;
  concepts.forEach(concept => {
    const citation = citations[concept];
    content += `### ${concept.replace(/-/g, ' ').replace(/\b\w/g, l => l.toUpperCase())}\n\n`;
    if (citation.wikipedia) {
      content += `- **Wikipedia**: [${concept}](${citation.wikipedia})\n`;
    }
    if (citation.arxiv && citation.arxiv.length > 0) {
      content += `- **arXiv**: [Search](${citation.arxiv[0]})\n`;
    }
    content += `- **Details**: See [by-concept/${concept}.md](../by-concept/${concept}.md)\n\n`;
  });
  
  content += `## Cross-Paradigm Integration\n\n`;
  content += `See how ${paradigm} integrates with other paradigms:\n\n`;
  content += `- **Paradigm Integration**: \`../../horizontal/integration-guides/paradigm-integration.md\`\n`;
  content += `- **Architecture Overview**: \`../../horizontal/Architecture_Overview.md\`\n\n`;
  
  content += `---\n\n`;
  content += `**Last Updated**: ${new Date().toISOString().split('T')[0]}\n`;
  content += `**Version**: 1.0.0\n`;
  
  return content;
}

// Helper functions
function getDimensionDescription(dim: string): string {
  const descriptions: { [key: string]: string } = {
    '0D': 'Foundation (Topology and Identity)',
    '1D': 'Temporal (Progression and Sequences)',
    '2D': 'Structural (Patterns and Logic)',
    '3D': 'Algebraic (Types and Semantics)',
    '4D': 'Network (Connectivity and Agents)',
    '5D': 'Consensus (Agreement and Coordination)',
    '6D': 'Intelligence (Learning and Reasoning)',
    '7D': 'Quantum (Superposition and Entanglement)',
  };
  return descriptions[dim] || 'Unknown';
}

function getPreviousDimension(dim: string): string {
  const dims = ['0D', '1D', '2D', '3D', '4D', '5D', '6D', '7D'];
  const idx = dims.indexOf(dim);
  return idx > 0 ? dims[idx - 1] : '0D';
}

function getNextDimension(dim: string): string {
  const dims = ['0D', '1D', '2D', '3D', '4D', '5D', '6D', '7D'];
  const idx = dims.indexOf(dim);
  return idx < dims.length - 1 ? dims[idx + 1] : '7D';
}

function isTopologyConcept(concept: string): boolean {
  // Topology concepts are foundational/mathematical
  const topologyConcepts = ['church-encoding', 'lambda-calculus', 'y-combinator', 'computational-topology'];
  return topologyConcepts.includes(concept);
}

// Generate all files
const refsDir = path.join(wikiDir, 'references');
const byConceptDir = path.join(refsDir, 'by-concept');
const byDimensionDir = path.join(refsDir, 'by-dimension');
const byParadigmDir = path.join(refsDir, 'by-paradigm');

// Ensure directories exist
[byConceptDir, byDimensionDir, byParadigmDir].forEach(dir => {
  if (!fs.existsSync(dir)) {
    fs.mkdirSync(dir, { recursive: true });
  }
});

// Generate concept references
console.log('Generating concept references...');
Object.keys(citations).forEach(conceptId => {
  const content = generateConceptReference(conceptId, citations[conceptId]);
  const filePath = path.join(byConceptDir, `${conceptId}.md`);
  fs.writeFileSync(filePath, content);
  console.log(`  Generated: ${filePath}`);
});

// Generate dimension references
console.log('\nGenerating dimension references...');
['0D', '1D', '2D', '3D', '4D', '5D', '6D', '7D'].forEach(dim => {
  const content = generateDimensionReference(dim);
  const filePath = path.join(byDimensionDir, `${dim}-references.md`);
  fs.writeFileSync(filePath, content);
  console.log(`  Generated: ${filePath}`);
});

// Generate paradigm references
console.log('\nGenerating paradigm references...');
const paradigms = ['functional-programming', 'logic-programming', 'semantic-web', 'multi-agent-systems'];
paradigms.forEach(paradigm => {
  const content = generateParadigmReference(paradigm);
  const filePath = path.join(byParadigmDir, `${paradigm}.md`);
  fs.writeFileSync(filePath, content);
  console.log(`  Generated: ${filePath}`);
});

console.log('\n✅ Reference generation complete!');
