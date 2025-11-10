#!/usr/bin/env tsx
/**
 * Generate Wiki Documentation
 * 
 * Main orchestrator script that runs all generation steps:
 * 1. Extract frontmatter using ObsidianFrontmatterKnowledgeModel
 * 2. Build reference trie structure
 * 3. Generate Wikipedia articles
 * 4. Generate arXiv paper
 * 5. Generate index and navigation
 * 6. Validate all links and citations
 */

import * as fs from 'fs';
import * as path from 'path';
import { extractFrontmatterTrie } from './extract-frontmatter-trie';
import { generateAllWikipediaArticles } from './generate-wikipedia-docs';
import { generateArxivPaper, generateBibliography } from './generate-arxiv-paper';
// Import generateIndex and generateNavigation from this file

/**
 * Validate all generated files
 */
async function validateGeneratedFiles(): Promise<void> {
  console.log('🔍 Validating generated files...');
  
  const requiredFiles = [
    'frontmatter-trie.json',
    'academic-citations.json',
    'Computational_Topology_Canvas.md',
    'Church_Encoding.md',
    'Multi_Agent_System.md',
    'Meta_Log_Framework.md',
    'Dimensional_Progression.md',
    'Blackboard_Architecture.md',
    'CanvasL_Format.md',
    'Automaton_System.md',
    'arxiv-paper.tex',
    'bibliography.bib',
    'INDEX.md',
    'NAVIGATION.md'
  ];
  
  const missingFiles: string[] = [];
  const existingFiles: string[] = [];
  
  for (const file of requiredFiles) {
    const filePath = path.join(__dirname, file);
    if (fs.existsSync(filePath)) {
      existingFiles.push(file);
      const stats = fs.statSync(filePath);
      console.log(`   ✓ ${file} (${(stats.size / 1024).toFixed(2)} KB)`);
    } else {
      missingFiles.push(file);
      console.log(`   ✗ ${file} (missing)`);
    }
  }
  
  if (missingFiles.length > 0) {
    console.log(`\n⚠️  Warning: ${missingFiles.length} file(s) missing`);
    console.log(`   Missing: ${missingFiles.join(', ')}`);
  } else {
    console.log(`\n✅ All ${existingFiles.length} required files generated successfully`);
  }
}

/**
 * Generate comprehensive index
 */
function generateIndex(trie: any, citations: any): string {
  let index = `# Complete Index\n\n`;
  index += `This index provides comprehensive coverage of all concepts, agents, functions, and documentation in the Computational Topology Canvas system.\n\n`;
  
  const root = trie?.root || {};
  const nodes = trie?.nodes || {};
  
  // Concepts
  index += `## Concepts\n\n`;
  const concepts = Object.keys(root.concepts || {});
  concepts.sort().slice(0, 50).forEach(concept => {
    const conceptData = root.concepts[concept];
    index += `- **[${concept}](${concept.replace(/\s+/g, '_')}.md)** - ${conceptData.documents?.length || 0} documents\n`;
  });
  if (concepts.length > 50) {
    index += `\n... and ${concepts.length - 50} more concepts\n`;
  }
  index += `\n`;
  
  // Agents
  index += `## Agents\n\n`;
  const agents = Object.values(root.dimensions || {}).flatMap((dim: any) => 
    (dim.agents || []).map((agentId: string) => ({ agentId, dimension: dim.dimension }))
  );
  agents.forEach(({ agentId, dimension }) => {
    index += `- **[${agentId}](${dimension.replace('D', 'D_')}_${agentId.replace(/-/g, '_')}.md)** - ${dimension}\n`;
  });
  index += `\n`;
  
  // Documents by dimension
  index += `## Documents by Dimension\n\n`;
  Object.keys(root.dimensions || {}).sort().forEach(dim => {
    const dimData = root.dimensions[dim];
    index += `### ${dim}\n\n`;
    (dimData.documents || []).slice(0, 10).forEach((docId: string) => {
      const doc = nodes[docId];
      if (doc) {
        index += `- **[${doc.title}](${doc.filePath})** - ${doc.type}\n`;
      }
    });
    if ((dimData.documents || []).length > 10) {
      index += `\n... and ${(dimData.documents || []).length - 10} more documents\n`;
    }
    index += `\n`;
  });
  
  // Citations
  index += `## Citations\n\n`;
  Object.keys(citations).slice(0, 20).forEach(concept => {
    const citation = citations[concept];
    index += `### ${concept}\n\n`;
    if (citation.wikipedia) {
      index += `- Wikipedia: [${concept}](${citation.wikipedia})\n`;
    }
    if (citation.arxiv && citation.arxiv.length > 0) {
      citation.arxiv.slice(0, 2).forEach((url: string) => {
        index += `- arXiv: [${concept}](${url})\n`;
      });
    }
    index += `\n`;
  });
  
  return index;
}

/**
 * Generate navigation structure
 */
function generateNavigation(trie: any): string {
  let nav = `# Navigation Guide\n\n`;
  nav += `This guide helps you navigate the Computational Topology Canvas documentation.\n\n`;
  
  const root = trie?.root || {};
  
  // By dimension
  nav += `## By Dimension\n\n`;
  nav += `The system progresses dimensionally from 0D to 7D:\n\n`;
  Object.keys(root.dimensions || {}).sort().forEach(dim => {
    const dimData = root.dimensions[dim];
    nav += `- **[${dim}](#${dim.toLowerCase()})** - ${dimData.documents?.length || 0} documents, ${dimData.agents?.length || 0} agents\n`;
  });
  nav += `\n`;
  
  // By level
  nav += `## By Level\n\n`;
  Object.keys(root.levels || {}).forEach(level => {
    const levelData = root.levels[level];
    nav += `- **[${level}](${level}.md)** - ${levelData.count || 0} documents\n`;
  });
  nav += `\n`;
  
  // By type
  nav += `## By Type\n\n`;
  Object.keys(root.types || {}).forEach(type => {
    const typeData = root.types[type];
    nav += `- **[${type}](${type}.md)** - ${typeData.count || 0} documents\n`;
  });
  nav += `\n`;
  
  // By topic
  nav += `## By Topic\n\n`;
  const topics = ['Church Encoding', 'Multi-Agent System', 'Meta-Log Framework', 'Dimensional Progression', 'Blackboard Architecture'];
  topics.forEach(topic => {
    nav += `- **[${topic}](${topic.replace(/\s+/g, '_')}.md)**\n`;
  });
  
  return nav;
}

/**
 * Validate citations
 */
async function validateCitations(): Promise<void> {
  console.log('🔍 Validating citations...');
  
  const citationsPath = path.join(__dirname, 'academic-citations.json');
  if (!fs.existsSync(citationsPath)) {
    console.log('   ⚠️  Citations file not found');
    return;
  }
  
  const citations = JSON.parse(fs.readFileSync(citationsPath, 'utf-8'));
  let validCount = 0;
  let invalidCount = 0;
  
  for (const [concept, citation] of Object.entries(citations) as any[]) {
    if (citation.wikipedia && citation.wikipedia.startsWith('https://en.wikipedia.org/wiki/')) {
      validCount++;
    } else {
      invalidCount++;
      console.log(`   ⚠️  Invalid Wikipedia URL for ${concept}: ${citation.wikipedia}`);
    }
    
    if (citation.arxiv && Array.isArray(citation.arxiv)) {
      for (const arxivUrl of citation.arxiv) {
        if (arxivUrl.startsWith('https://arxiv.org/')) {
          validCount++;
        } else {
          invalidCount++;
          console.log(`   ⚠️  Invalid arXiv URL for ${concept}: ${arxivUrl}`);
        }
      }
    }
  }
  
  console.log(`   ✓ Valid citations: ${validCount}`);
  if (invalidCount > 0) {
    console.log(`   ⚠️  Invalid citations: ${invalidCount}`);
  } else {
    console.log(`   ✅ All citations valid`);
  }
}

/**
 * Main generation workflow
 */
async function generateWiki(): Promise<void> {
  console.log('🚀 Starting wiki documentation generation...\n');
  
  try {
    // Step 1: Extract frontmatter
    console.log('Step 1/6: Extracting frontmatter...');
    await extractFrontmatterTrie();
    console.log('');
    
    // Step 2: Generate Wikipedia articles
    console.log('Step 2/6: Generating Wikipedia articles...');
    await generateAllWikipediaArticles();
    console.log('');
    
    // Step 3: Generate arXiv paper
    console.log('Step 3/6: Generating arXiv paper...');
    const paper = await generateArxivPaper();
    const bibliography = generateBibliography();
    const paperPath = path.join(__dirname, 'arxiv-paper.tex');
    const bibPath = path.join(__dirname, 'bibliography.bib');
    fs.writeFileSync(paperPath, paper);
    fs.writeFileSync(bibPath, bibliography);
    console.log(`   ✓ Generated arxiv-paper.tex`);
    console.log(`   ✓ Generated bibliography.bib`);
    console.log('');
    
    // Step 4: Generate index and navigation
    console.log('Step 4/6: Generating index and navigation...');
    const triePath = path.join(__dirname, 'frontmatter-trie.json');
    const citationsPath = path.join(__dirname, 'academic-citations.json');
    const trieData = JSON.parse(fs.readFileSync(triePath, 'utf-8'));
    const citations = JSON.parse(fs.readFileSync(citationsPath, 'utf-8'));
    const index = generateIndex(trieData, citations);
    const navigation = generateNavigation(trieData);
    const indexPath = path.join(__dirname, 'INDEX.md');
    const navPath = path.join(__dirname, 'NAVIGATION.md');
    fs.writeFileSync(indexPath, index);
    fs.writeFileSync(navPath, navigation);
    console.log(`   ✓ Generated INDEX.md`);
    console.log(`   ✓ Generated NAVIGATION.md`);
    console.log('');
    
    // Step 5: Validate
    console.log('Step 5/6: Validating generated files...');
    await validateGeneratedFiles();
    await validateCitations();
    console.log('');
    
    console.log('✅ Wiki documentation generation complete!');
    console.log(`\n📁 Output directory: ${__dirname}`);
    console.log(`\n📚 Generated files:`);
    console.log(`   - Frontmatter trie: frontmatter-trie.json`);
    console.log(`   - Reference trie: reference-trie.json`);
    console.log(`   - Wikipedia articles: *.md`);
    console.log(`   - arXiv paper: arxiv-paper.tex`);
    console.log(`   - Bibliography: bibliography.bib`);
    console.log(`   - Index: INDEX.md`);
    console.log(`   - Navigation: NAVIGATION.md`);
    
  } catch (error) {
    console.error('❌ Error during wiki generation:', error);
    process.exit(1);
  }
}

/**
 * Main execution
 */
async function main() {
  await generateWiki();
}

if (require.main === module) {
  main();
}

export { generateWiki, validateGeneratedFiles, validateCitations };
