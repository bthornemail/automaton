#!/usr/bin/env tsx
/**
 * Generate Comprehensive Index and Navigation
 * 
 * Generates INDEX.md and NAVIGATION.md for the wiki.
 */

import * as fs from 'fs';
import * as path from 'path';
// Using frontmatter-trie.json directly

/**
 * Generate INDEX.md
 */
async function generateIndex(): Promise<string> {
  console.log('📑 Generating INDEX.md...');
  
  const triePath = path.join(__dirname, 'frontmatter-trie.json');
  if (!fs.existsSync(triePath)) {
    throw new Error(`Frontmatter trie not found: ${triePath}. Run extract-frontmatter-trie.ts first.`);
  }
  
  const trie: any = JSON.parse(fs.readFileSync(triePath, 'utf-8'));
  
  let index = `# Index

Complete index of all concepts, agents, functions, and documentation in the Computational Topology Canvas wiki.

## Concepts

`;
  
  // Sort concepts alphabetically
  const concepts = Object.keys(trie.root.concepts || {}).sort();
  for (const concept of concepts) {
    const conceptData = trie.root.concepts[concept];
    index += `- **${concept}**: ${conceptData.documents.length} document(s)`;
    if (conceptData.wikipedia) {
      index += ` - [Wikipedia](${conceptData.wikipedia})`;
    }
    if (conceptData.arxiv && conceptData.arxiv.length > 0) {
      index += ` - [arXiv](${conceptData.arxiv[0]})`;
    }
    index += `\n`;
  }
  
  index += `\n## Agents\n\n`;
  
  // List agents by dimension
  const dimensions = ['0D', '1D', '2D', '3D', '4D', '5D', '6D', '7D'];
  for (const dim of dimensions) {
    if (trie.root.dimensions[dim]?.agents.length > 0) {
      index += `### ${dim} Agents\n\n`;
      for (const agentId of trie.root.dimensions[dim].agents) {
        const agent = trie.root.agents[agentId];
        if (agent) {
          index += `- **${agent.name}**: ${agent.dependencies.length} dependency(ies)\n`;
        }
      }
      index += `\n`;
    }
  }
  
  // Interface, Collaborative, Evolutionary agents
  const otherAgents = Object.entries(trie.root.agents || {}).filter(([_, agent]: any) => !agent.dimension);
  if (otherAgents.length > 0) {
    index += `### Other Agents\n\n`;
    for (const [agentId, agent] of otherAgents as any[]) {
      index += `- **${agent.name}**: ${agent.dependencies.length} dependency(ies)\n`;
    }
    index += `\n`;
  }
  
  index += `## Functions\n\n`;
  
  // Group functions by module
  const functionsByModule = new Map<string, string[]>();
  for (const [funcId, func] of Object.entries(trie.root.functions || {})) {
    const module = (func as any).module || 'other';
    if (!functionsByModule.has(module)) {
      functionsByModule.set(module, []);
    }
    functionsByModule.get(module)!.push(funcId);
  }
  
  for (const [module, funcIds] of functionsByModule) {
    index += `### ${module}\n\n`;
    for (const funcId of funcIds) {
      const func = trie.root.functions[funcId];
      if (func) {
        index += `- **${func.name}**`;
        if (func.signature) {
          index += `: \`${func.signature}\``;
        }
        index += `\n`;
      }
    }
    index += `\n`;
  }
  
  index += `## Documents\n\n`;
  
  // Documents by level
  index += `### By Level\n\n`;
  const levels = ['gateway', 'foundational', 'practical', 'applied'];
  for (const level of levels) {
    if (trie.root.levels[level]) {
      index += `- **${level}**: ${trie.root.levels[level].count} document(s)\n`;
    }
  }
  
  index += `\n### By Type\n\n`;
  const types = ['navigation', 'concept', 'implementation', 'guide', 'specification', 'documentation'];
  for (const type of types) {
    if (trie.root.types[type]) {
      index += `- **${type}**: ${trie.root.types[type].count} document(s)\n`;
    }
  }
  
  index += `\n## Citations\n\n`;
  
  // Load citations
  const citationsPath = path.join(__dirname, 'academic-citations.json');
  if (fs.existsSync(citationsPath)) {
    const citations = JSON.parse(fs.readFileSync(citationsPath, 'utf-8'));
    for (const [concept, citation] of Object.entries(citations) as any[]) {
      index += `### ${concept}\n\n`;
      if (citation.wikipedia) {
        index += `- Wikipedia: [${concept}](${citation.wikipedia})\n`;
      }
      if (citation.arxiv && citation.arxiv.length > 0) {
        index += `- arXiv: ${citation.arxiv.map((url: string) => `[Link](${url})`).join(', ')}\n`;
      }
      index += `\n`;
    }
  }
  
  return index;
}

/**
 * Generate NAVIGATION.md
 */
async function generateNavigation(): Promise<string> {
  console.log('🧭 Generating NAVIGATION.md...');
  
  const triePath = path.join(__dirname, 'frontmatter-trie.json');
  if (!fs.existsSync(triePath)) {
    throw new Error(`Frontmatter trie not found: ${triePath}. Run extract-frontmatter-trie.ts first.`);
  }
  
  const trie: any = JSON.parse(fs.readFileSync(triePath, 'utf-8'));
  
  let nav = `# Navigation Guide

Navigation guide for exploring the Computational Topology Canvas wiki.

## By Dimension

`;
  
  // Navigation by dimension
  const dimensions = ['0D', '1D', '2D', '3D', '4D', '5D', '6D', '7D'];
  for (const dim of dimensions) {
    if (trie.root.dimensions[dim]) {
      nav += `### ${dim}\n\n`;
      nav += `- Documents: ${trie.root.dimensions[dim].documents.length}\n`;
      nav += `- Agents: ${trie.root.dimensions[dim].agents.length}\n`;
      nav += `- Functions: ${trie.root.dimensions[dim].functions.length}\n`;
      nav += `- Concepts: ${trie.root.dimensions[dim].concepts.length}\n\n`;
    }
  }
  
  nav += `## By Level\n\n`;
  
  // Navigation by level
  const levels = ['gateway', 'foundational', 'practical', 'applied'];
  for (let i = 0; i < levels.length; i++) {
    const level = levels[i];
    if (trie.root.levels[level]) {
      nav += `### ${level}\n\n`;
      nav += `- Documents: ${trie.root.levels[level].count}\n`;
      if (i > 0) {
        nav += `- Previous: ${levels[i - 1]}\n`;
      }
      if (i < levels.length - 1) {
        nav += `- Next: ${levels[i + 1]}\n`;
      }
      nav += `\n`;
    }
  }
  
  nav += `## By Type\n\n`;
  
  // Navigation by type
  const types = ['navigation', 'concept', 'implementation', 'guide', 'specification', 'documentation'];
  for (const type of types) {
    if (trie.root.types[type]) {
      nav += `### ${type}\n\n`;
      nav += `- Documents: ${trie.root.types[type].count}\n\n`;
    }
  }
  
  nav += `## By Topic\n\n`;
  
  // Navigation by topic (concepts)
  const topics = ['church-encoding', 'multi-agent-system', 'prolog', 'datalog', 'r5rs', 'computational-topology'];
  for (const topic of topics) {
    if (trie.root.concepts[topic]) {
      nav += `### ${topic}\n\n`;
      nav += `- Documents: ${trie.root.concepts[topic].documents.length}\n`;
      nav += `- Related: ${trie.root.concepts[topic].relatedConcepts.join(', ')}\n\n`;
    }
  }
  
  nav += `## Quick Links\n\n`;
  nav += `- [Main Article](Computational_Topology_Canvas.md)\n`;
  nav += `- [Church Encoding](Church_Encoding.md)\n`;
  nav += `- [Multi Agent System](Multi_Agent_System.md)\n`;
  nav += `- [Meta Log Framework](Meta_Log_Framework.md)\n`;
  nav += `- [Dimensional Progression](Dimensional_Progression.md)\n`;
  nav += `- [Blackboard Architecture](Blackboard_Architecture.md)\n`;
  nav += `- [CanvasL Format](CanvasL_Format.md)\n`;
  nav += `- [Automaton System](Automaton_System.md)\n`;
  
  return nav;
}

/**
 * Main execution
 */
async function main() {
  try {
    const index = await generateIndex();
    const navigation = await generateNavigation();
    
    const indexPath = path.join(__dirname, 'INDEX.md');
    const navPath = path.join(__dirname, 'NAVIGATION.md');
    
    fs.writeFileSync(indexPath, index);
    fs.writeFileSync(navPath, navigation);
    
    console.log(`✅ Generated INDEX.md: ${indexPath}`);
    console.log(`✅ Generated NAVIGATION.md: ${navPath}`);
  } catch (error) {
    console.error('❌ Error generating index/navigation:', error);
    process.exit(1);
  }
}

if (require.main === module) {
  main();
}

export { generateIndex, generateNavigation };
